(* File Wasmcomp.fs
   A compiler from micro-C, a sublanguage of the C language, to
   WebAssembly. Direct (forwards) compilation without optimization
   of jumps to jumps, tail-calls etc.

   based on Comp.fs by
   sestoft@itu.dk * 2009-09-23, 2011-11-10

   A value is an integer; it may represent an integer or a pointer,
   where a pointer is just an address in the store (of a variable or
   pointer or the base address of an array).

   The compile-time environment maps a global variable to a fixed
   store address, and maps a local variable to an offset into the
   current stack frame, relative to its bottom.  The run-time store
   maps a location to an integer.  This freely permits pointer
   arithmetics, as in real C.  A compile-time function environment
   maps a function name to a code label.  In the generated code,
   labels are replaced by absolute code addresses.

   Expressions can have side effects.  A function takes a list of
   typed arguments and may optionally return a result.

   Arrays can be one-dimensional and constant-size only.  For
   simplicity, we represent an array as a variable which holds the
   address of the first array element.  This is consistent with the
   way array-type parameters are handled in C, but not with the way
   array-type variables are handled.  Actually, this was how B (the
   predecessor of C) represented array variables.

   The store behaves as a stack, so all data except global variables
   are stack allocated: variables, function parameters and arrays.
*)

module MicroWac.Wasmcomp

open Absyn
open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open WasmMachine

//#region Environment types
type FunEnv = { Ids:   Map<string, int>;
                Types: Map<(Typ option * Typ list), int>;
                Decs:  Map<int, Topdec>; }

type GloVar = { Addr: int; Type: Typ; }
type ArgVar = { Id: int; Type: Typ; }
type LocVar = { Addr: int; Offset: int; Type: Typ; Depth: int; InScope: bool; }

type Var =
  | GloVar of GloVar
  | ArgVar of ArgVar
  | LocVar of LocVar

type VarOp =
  | Set of Expr
  | GetValue
  | GetAddr

type VarEnv = { Vars: (string * Var) list;
                NextFreeArgId: int;
                NextFreeAddress: int;
                CurrentOffset: int;
                CurrentDepth: int; }

let rec lookup env depth x =
  match env with
  | []         -> failwith (sprintf "%s not found" x)
  | (y, v)::yr -> if x=y then
                    match v with
                    | LocVar lv when lv.Depth > depth || not lv.InScope -> lookup yr depth x
                    | _ -> v
                  else lookup yr depth x
//#endregion

//#region Local and Global variable helper functions
let varWidth = function
  | TypC -> 1
  | _    -> 4

let varAccOp isFunArg = function
  | _ when isFunArg -> GET_LOCAL
  | TypC -> I32_LOAD8_U
  | _    -> I32_LOAD

let varAssOp isFunArg = function
  | _ when isFunArg -> SET_LOCAL
  | TypC -> I32_STORE8
  | _    -> I32_STORE

let updateNextFreeAddress allocdTyp varEnv =
  { varEnv with NextFreeAddress = varEnv.NextFreeAddress + (varWidth allocdTyp) }

let allocateGloVar varEnv name typ =
  let gloVar = GloVar { Addr = varEnv.NextFreeAddress; Type = typ; }
  updateNextFreeAddress typ { varEnv with Vars = (name, gloVar) :: varEnv.Vars; }

let allocateLocVar varEnv name typ =
  let locVar = LocVar { Addr = varEnv.NextFreeAddress;
                        Offset = varEnv.CurrentOffset;
                        Type = typ;
                        Depth = varEnv.CurrentDepth;
                        InScope = true; }
  updateNextFreeAddress typ { varEnv with Vars = (name, locVar) :: varEnv.Vars }
//#endregion

//#region Function declaration helper functions
let getFunSig f =
  let getArgTypes types (typ, _) = typ :: types
  match f with
  | Funsig(_, retTyp, _, args)
  | Fundec(_, retTyp, _, args, _) -> (retTyp, List.fold getArgTypes [] args)
  | _ -> failwith (sprintf "can't get function signature of %A" f)

let updTypes func funEnv =
  let funsig = getFunSig func
  if Map.containsKey funsig (funEnv.Types)
  then funEnv
  else { funEnv with Types = Map.add funsig (Map.count funEnv.Types) funEnv.Types }

let getFunId name funEnv =
  Map.find name funEnv.Ids

let getFunDec funId funEnv =
  Map.find funId funEnv.Decs
//#endregion

/// Get the keys of a map, as a sequence.
let mapKeys map = map |> Map.toSeq |> Seq.map fst
/// Get the values of a map, as a sequence.
let mapValues map = map |> Map.toSeq |> Seq.map snd

/// shallow steps a single up in depth in the variable environment.
/// The keyword 'raise' was already taken.
let shallow varEnv = { varEnv with CurrentDepth = varEnv.CurrentDepth - 1 }

/// deepen increases the current depth of the variable environment.
let deepen varEnv = { varEnv with CurrentDepth = varEnv.CurrentDepth + 1 }

let rec accessVar varEnv funEnv op = function
  | AccVar x ->
    let var = lookup varEnv.Vars varEnv.CurrentDepth x
    match var with | LocVar lv -> match op with
                                  | GetValue -> [I32_CONST lv.Addr; varAccOp false lv.Type lv.Offset]
                                  | GetAddr  -> [I32_CONST (lv.Addr + lv.Offset)]
                                  | Set exp  -> I32_CONST lv.Addr
                                                :: cExpr varEnv funEnv exp
                                                @ [varAssOp false lv.Type lv.Offset]
                   | ArgVar av -> match op with
                                  | GetValue -> [varAccOp true av.Type av.Id]
                                  | GetAddr  -> match av.Type with
                                                | TypP _ -> [varAccOp true av.Type av.Id]
                                                | _      -> failwith (sprintf "can't get address of function argument %A" av)
                                  | Set exp  -> cExpr varEnv funEnv exp @ [varAssOp true av.Type av.Id]
                   | GloVar gv -> match op with
                                  | GetValue -> [I32_CONST gv.Addr; varAccOp false gv.Type 0]
                                  | GetAddr  -> [I32_CONST gv.Addr]
                                  | Set exp  -> I32_CONST gv.Addr
                                                :: cExpr varEnv funEnv exp
                                                @ [varAssOp false gv.Type 0]
  | AccDeref exp -> match op with
                    | GetValue -> cExpr varEnv funEnv exp @ [varAccOp false TypI 0]
                    | GetAddr  -> cExpr varEnv funEnv exp
                    | Set expS -> cExpr varEnv funEnv exp
                                  @ cExpr varEnv funEnv expS
                                  @ [varAssOp false TypI 0]
  | AccIndex (acc, exp) -> failwith "compilation of arrays has not been implemented"
and cExpr varEnv funEnv = function
  | Access acc              -> accessVar varEnv funEnv GetValue acc
  | Assign (acc, exp)       -> accessVar varEnv funEnv (Set exp) acc
  | Addr acc                -> accessVar varEnv funEnv GetAddr acc
  | Cond (bExp, expT, expF) -> cExpr varEnv funEnv expT
                               @ cExpr varEnv funEnv expF
                               @ cExpr varEnv funEnv bExp
                               @ [SELECT]
  | CstI i                  -> [I32_CONST i]
  | Prim1 (op, exp) ->
    cExpr varEnv funEnv exp
    @ (match op with
       | "!"      -> [I32_EQZ]
       | "printi"
       | "printc" -> [CALL (getFunId op funEnv)]
       | _        -> failwith (sprintf "unknown prim1 operator: %s" op))
  | Prim2 (op, exp1, exp2) ->
    cExpr varEnv funEnv exp1
    @ cExpr varEnv funEnv exp2
    @ (match op with
       | "+"  -> [I32_ADD]
       | "-"  -> [I32_SUB]
       | "*"  -> [I32_MUL]
       | "/"  -> [I32_DIV_S]
       | "%"  -> [I32_REM_U]
       | "==" -> [I32_EQ]
       | "!=" -> [I32_NE]
       | ">"  -> [I32_GT_S]
       | "<"  -> [I32_LT_S]
       | ">=" -> [I32_GE_S]
       | "<=" -> [I32_LE_S]
       | _    -> failwith (sprintf "unknown prim2 operator: %s" op))
  | Andalso (exp1, exp2) ->
    let lhs = cExpr varEnv funEnv exp1
    let rhs = cExpr varEnv funEnv exp2
    lhs
    @ IF (BReturn I32) ::
        rhs
    @ [ IF (BReturn I32);
          I32_CONST 1;
        ELSE;
          I32_CONST 0;
        END;
      ELSE;
        I32_CONST 0;
      END]
  | Orelse (exp1, exp2) ->
    let lhs = cExpr varEnv funEnv exp1
    let rhs = cExpr varEnv funEnv exp2
    lhs
    @ IF (BReturn I32)
      :: I32_CONST 1 ::
      ELSE ::
        rhs
        @[IF (BReturn I32);
            I32_CONST 1;
          ELSE;
            I32_CONST 0;
          END;
      END]
  | Call (name, argExprs) ->
    let funId = getFunId name funEnv
    let funArgCount = List.length (snd (getFunSig (getFunDec funId funEnv)))

    if argExprs.Length <> funArgCount
    then failwith (sprintf "function %s expects %d args, got %d. Args received: %A" name funArgCount argExprs.Length argExprs)

    let cArgs = List.collect (fun e -> cExpr varEnv funEnv e) argExprs
    cArgs @ [CALL funId]
and cStmtOrDec varEnv funEnv = function
  | Dec (typ, name) -> allocateLocVar varEnv name typ, []
  | Stmt stmt       -> cStmt varEnv funEnv stmt
and cBlock varEnv funEnv = function
  | Block stmtOrDecs ->
    // discard all variables already declared at- and below the current depth
    let discardScopes currentDepth locals =
      locals
      |> List.map (fun ((name, var) as x) ->
          match var with
          | LocVar lv -> if lv.Depth >= currentDepth
                         then name, LocVar { lv with InScope = false }
                         else x
          | _         -> x)

    let varEnv, cStmts =
      List.fold (fun (varEnv, cStmts) stmtOrDec ->
                  let varEnv, cStmt = (cStmtOrDec varEnv funEnv stmtOrDec)
                  varEnv, cStmts @ cStmt
                ) (varEnv, []) stmtOrDecs
    let varEnv = { varEnv with Vars = discardScopes varEnv.CurrentDepth varEnv.Vars }
    varEnv, BLOCK (BVoid) :: cStmts @ [END]
  | anyOtherStmt ->
    failwith (sprintf "attempted to cBlock compile a non-block statement: %A" anyOtherStmt)
and cStmt varEnv funEnv = function
  | If (bExp, stmT, stmF) ->
    let varEnvT, stmTCode =
      cStmt (deepen varEnv) funEnv stmT
    let varEnvF, stmFCode =
      cStmt varEnvT funEnv stmF

    let stmtContainsReturn =
      stmTCode |> List.exists (fun instr -> instr = RETURN)
    let ifRetTyp = if stmtContainsReturn then BReturn I32 else BVoid
    shallow varEnvF, cExpr varEnv funEnv bExp
                     @ IF (ifRetTyp) :: stmTCode
                     @ ELSE :: stmFCode
                     @ [END]
  | While (exp, stm) ->
    let loopVarEnv, loopCode = cStmt (deepen varEnv) funEnv stm
    shallow loopVarEnv, BLOCK (BVoid)
                        :: LOOP (BVoid)
                          :: cExpr loopVarEnv funEnv exp @ I32_EQZ :: BR_IF 1uy
                          :: loopCode
                          @  BR 0uy
                        :: [END;
                        END]
  | Expr exp          -> varEnv, cExpr varEnv funEnv exp
  | Return (Some exp) -> varEnv, cExpr varEnv funEnv exp @ [RETURN]
  | Return None       -> varEnv, [RETURN]
  | Block _ as b      -> let deeperVarEnv, code = cBlock (deepen varEnv) funEnv b
                         // step up the depth after the block
                         let shallowerVarEnv = shallow deeperVarEnv
                         shallowerVarEnv, code
let cProgram (Prog topdecs) =
  let emptyFunEnv = { Ids = Map.empty; Types = Map.empty; Decs = Map.empty; }
  let emptyVarEnv = { Vars = [];
                      NextFreeArgId = 0;
                      NextFreeAddress = 0;
                      CurrentOffset = 0;
                      CurrentDepth = 0 }

  // add printi and printc import functions required for spec compliance with MicroC
  let topdecs = Funsig(true, None, "printi", [(TypI, "i")])
                :: Funsig(true, None, "printc", [(TypC, "c")])
                :: topdecs

  let typeFolder funEnv = function
    | Funsig _
    | Fundec _ as f -> updTypes f funEnv
    | _             -> funEnv
  let funEnv = List.fold typeFolder emptyFunEnv topdecs

  let importsFolder (funEnv, imports) = function
    | Funsig(imported, _, name, _) as f when imported ->
      let funId = Map.count funEnv.Ids
      ({ funEnv with Ids = Map.add name funId funEnv.Ids
                     Decs = Map.add funId f funEnv.Decs },
       Map.add name (Map.find (getFunSig f) funEnv.Types) imports)
    | _ -> funEnv, imports
  let funEnv, imports = List.fold importsFolder (funEnv, Map.empty) topdecs

  let envFolder (varEnv, funEnv) = function
    | Fundec(_, _, name, _, _) as f ->
      let funId = Map.count funEnv.Ids
      varEnv, { funEnv with Ids = Map.add name funId funEnv.Ids
                            Decs = Map.add funId f funEnv.Decs }
    | Vardec (typ, name) -> allocateGloVar varEnv name typ, funEnv
    | _ -> varEnv, funEnv
  let varEnv, funEnv = List.fold envFolder (emptyVarEnv, funEnv) topdecs

  let exportFolder topdec exports =
    match topdec with
    | Fundec(exported, _, name, _, _) as f when exported ->
      Map.add name (getFunId name funEnv) exports
    | _ -> exports
  let exports = List.foldBack exportFolder topdecs Map.empty

  let funCodeFolder topdec (varEnvs, code, offset) =
    match topdec with
    | Fundec(_, _, _, args, block) as f ->
      let allocateFunArg varEnv name typ id =
        let argVar = ArgVar { Id = id;
                              Type = typ; }
        // XXX: duplicate variable name checking can be done here
        { varEnv with Vars = (name, argVar) :: varEnv.Vars }, id+1
      let argsToLocVars = List.fold (fun (env, id) (typ, name) -> allocateFunArg env name typ id)
      let varEnv, _ = argsToLocVars (varEnv, 0) args
      let varEnv = { varEnv with CurrentOffset = offset;
                                 NextFreeAddress = 0;
                                 CurrentDepth = 0; }
      let varEnv, funCode = cBlock varEnv funEnv block
      // function blocks in WASM do not have the block begin instruction, so discard it
      varEnv :: varEnvs, (List.tail funCode) :: code, offset + varEnv.NextFreeAddress
    | _ -> varEnvs, code, offset
  let varEnvs, funCode, _ =
    List.foldBack funCodeFolder (funEnv.Decs |> mapValues
                                             |> List.ofSeq) ([], [], varEnv.NextFreeAddress)
  funEnv, varEnvs, imports, exports, funCode

let code2bytes code =
  List.foldBack emitbytes code []

let compileWasmBinary fileName (funEnv, varEnvs, imports, exports, funCode) =
  // open binary file stream
  let writer filename = new BinaryWriter(File.Open(filename, FileMode.Create))
  use wasmFile = writer fileName

  // stream writer helper functions
  let writeBytes bytes = List.iter (fun (b : byte) -> wasmFile.Write(b)) bytes
  let writeVarInt n = i2leb n |> writeBytes
  let strToBytes (s : string) = Encoding.ASCII.GetBytes(s) |> Array.toList
  let ofSeqConcat = List.ofSeq >> List.concat
  let mapToSeqSortedBy f map = map |> Map.toSeq |> Seq.sortBy f
  let gSection sectionType data =
    getSectionCode sectionType :: i2leb (Seq.length data) @ data
  let writeSection sectionType = (gSection sectionType) >> writeBytes

  //#region WASM Header [0]
  let wasmHeader = [i2b 0x0061736D; i2b 0x01000000]
  List.iter writeBytes wasmHeader
  //#endregion

  //#region Type section [1]
  let typeSectMapper ((retTyp, argTyps), i) =
    getValueTypeCode Func                               // type code
    :: i2leb (List.length argTyps)                      // num of args
    @  List.map (fun _ -> getValueTypeCode I32) argTyps // arg types
    @  match retTyp with                                // number of return types (can only be 1, as of WASM v1.0)
       | Some _ -> [1uy; getValueTypeCode I32]          // the return type
       | None   -> [0uy]                                // ... or nothing returned
  let typeSectionData = i2leb (Map.count funEnv.Types)
                        @ (funEnv.Types |> mapToSeqSortedBy snd
                                        |> Seq.map typeSectMapper
                                        |> ofSeqConcat)
  writeSection TYPE typeSectionData
  //#endregion

  //#region Import section [2]
  let importSectMapper (fieldName, id) =
    let importNameAsBytes = strToBytes "imports"
    let fieldNameAsBytes = strToBytes fieldName
    // XXX: allow import of global variables.
    //      requires updating the import compilation fold function
    //      and extension of the abstract syntax, the lexer and parser.
    let importKind = 0x00uy // 0x00 = function extern type
    let importFunctionSignatureIndex = i2leb id
    i2leb (importNameAsBytes.Length) @ importNameAsBytes
    @ i2leb (fieldNameAsBytes.Length) @ fieldNameAsBytes
    @ importKind :: importFunctionSignatureIndex
  let importSectionData = i2leb (Map.count imports)
                          @ (imports |> mapToSeqSortedBy snd
                                     |> Seq.map importSectMapper
                                     |> ofSeqConcat)
  writeSection IMPORT importSectionData
  //#endregion

  //#region Function section [3]
  let funcSectMapper funDec =
    i2leb (Map.find (getFunSig funDec) funEnv.Types)
  if (Map.count funEnv.Decs) > 0 then
    let funDecs = funEnv.Decs |> mapToSeqSortedBy fst
                              |> Seq.choose (fun (_, dec) -> match dec with
                                                             | Fundec _ -> Some dec
                                                             | _        -> None)
    let funSectionData = i2leb (Seq.length funDecs)
                         @ (funDecs |> Seq.map funcSectMapper
                                    |> ofSeqConcat)
    writeSection FUNCTION funSectionData
  //#endregion

  //#region Memory section [5]
  writeSection MEMORY [1uy; // number of memories (max 1 for WASM MVP)
                       0uy; // limits: flags, should be 0
                       1uy] // limits: initial, should be 1 (1 page = 64 KiB)
  //#endregion

  //#region Global section [6]
  // XXX: removed after globals were moved to the linear memory
  //#endregion

  //#region Export section [7]
  let exportSectMapper (name, id) =
    let nameAsBytes = strToBytes name
    let exportKind = 0x00uy // 0x00 = function extern type
    i2leb (nameAsBytes.Length) @ nameAsBytes
    @ exportKind :: i2leb id
  if (Map.count exports) > 0 then
    let exportSectionData = i2leb (Map.count exports)
                            @ (exports |> mapToSeqSortedBy snd
                                       |> Seq.map exportSectMapper
                                       |> ofSeqConcat)
    writeSection EXPORT exportSectionData
  //#endregion

  //#region Start section [8]
  let expectedStartSignature = (None, [])
  match Map.tryFind "start" funEnv.Ids with
  | Some id -> let startFunSignature = getFunSig (Map.find id funEnv.Decs)
               if startFunSignature = expectedStartSignature
               then writeSection START (i2leb id)
               else failwith "start function has arguments or a return value"
  | None    -> ()
  //#endregion

  //#region Code section [10]
  let varEnvAndFunCode = List.zip varEnvs funCode
  let codeSectMapper (varEnv, instrs) =
    // there will always be 0 local variable declarations
    // as variables are stored in linear memory.
    let codeBytes = 0uy :: code2bytes instrs
    i2leb codeBytes.Length @ codeBytes
  if not (List.isEmpty varEnvAndFunCode) then
    let codeSectionData = i2leb (funCode.Length)
                          @ List.concat (List.map codeSectMapper varEnvAndFunCode)
    writeSection CODE codeSectionData
  //#endregion

let generateHtml (wasmFileName : string) =
  // not using app.config, as it still won't give us the path of the
  // actual application. not using an installer which would allow
  // keys containing static locations of template file.
  let microWacPath = Reflection.Assembly.GetEntryAssembly().Location
  let wasmBootstrapTemplatePath = Path.Combine((Path.GetDirectoryName microWacPath), "WasmBootstrapTemplate.html")
  let wasmTemplate =
    seq {
      for line in File.ReadLines(wasmBootstrapTemplatePath) ->
        Regex.Replace(line, "\${{WASM_FILENAME}}", wasmFileName)
    }
  let htmlFileName = (Path.GetFileNameWithoutExtension wasmFileName) + ".html"
  File.WriteAllLines(htmlFileName, wasmTemplate)

let compileToFile fileName program withHtml =
  (cProgram >> compileWasmBinary fileName) program
  if withHtml then generateHtml fileName
