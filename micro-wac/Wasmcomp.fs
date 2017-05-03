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

type LocVar = { Id: int; InScope: bool; FunArg: bool; }
// Keys of VarEnv Locals contain their name and the depth of declaration,
// their values contains the ID of the variable and if it is declared in the current scope.
type VarEnv = { Locals:  Map<(string * int), LocVar>;
                Globals: Map<string, int>; }

type VariableIndex = | Glo of int
                     | Loc of int
//#endregion

//#region Local and Global variable helper functions
let allocateGloVar varEnv name =
  { varEnv with Globals = Map.add name (Map.count varEnv.Globals) varEnv.Globals }

let allocateLocVar varEnv name depth isFunArg =
  let key = name, depth
  let id = if Map.containsKey key varEnv.Locals
           then (Map.find key varEnv.Locals).Id
           else Map.count varEnv.Locals
  let locVar = { Id = id; InScope = true; FunArg = isFunArg; }
  { varEnv with Locals = Map.add key locVar varEnv.Locals }

let getGlobalVarId name varEnv =
  if Map.containsKey name varEnv.Globals
  then Some (Map.find name varEnv.Globals)
  else None

let rec getLocalVarId (name, depth) varEnv =
  let exists x m = Map.containsKey x m && (Map.find x m).InScope
  match depth with
  | 0 ->
    if exists (name, depth) varEnv.Locals
    then Some (Map.find (name, depth) varEnv.Locals).Id
    else None
  | _ when depth > 0 ->
    if exists (name, depth) varEnv.Locals
    then Some (Map.find (name, depth) varEnv.Locals).Id
    else getLocalVarId (name, depth-1) varEnv
  | _ -> failwith (sprintf "negative depth lookup: %s at depth %d" name depth)

let rec varAccessAtDepth varEnv ((x, depth) as var) =
  match (getLocalVarId var varEnv, getGlobalVarId x varEnv) with
  | Some varId, _    -> Loc varId
  | None, Some varId -> Glo varId
  | _                -> failwith (sprintf "can't find variable: %s" x)

let accessVar varEnv depth = function
  | AccVar x            -> varAccessAtDepth varEnv (x, depth)
  | AccDeref exp        -> failwith "Access dereferencing not yet implemented"
  | AccIndex (acc, exp) -> failwith "Access indexes not yet implemented"
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
  else {funEnv with Types = Map.add funsig (Map.count funEnv.Types) funEnv.Types }

let getFunId name funEnv =
  Map.find name funEnv.Ids

let getFunDec funId funEnv =
  Map.find funId funEnv.Decs
//#endregion

/// Get the keys of a map, as a sequence.
let mapKeys map = map |> Map.toSeq |> Seq.map fst
/// Get the values of a map, as a sequence.
let mapValues map = map |> Map.toSeq |> Seq.map snd

let rec cExpr varEnv funEnv depth = function
  | Access acc              -> match accessVar varEnv depth acc with
                               | Loc i -> [GET_LOCAL i]
                               | Glo i -> [GET_GLOBAL i]
  | Assign (acc, exp)       -> cExpr varEnv funEnv depth exp
                               @ match accessVar varEnv depth acc with
                                 | Loc i -> [SET_LOCAL i]
                                 | Glo i -> [SET_GLOBAL i]
  | Addr acc                -> failwith "Address of variable not yet implemented"
  | Cond (bExp, expT, expF) -> cExpr varEnv funEnv depth expT
                               @ cExpr varEnv funEnv depth expF
                               @ cExpr varEnv funEnv depth bExp
                               @ [SELECT]
  | CstI i                  -> [I32_CONST i]
  | Prim1 (op, exp)         ->
      cExpr varEnv funEnv depth exp
    @ (match op with
       | "!" -> [I32_EQZ]
       | "printi" -> [CALL printIntFunctionIndex]
       | "printc" -> [CALL printCharFunctionIndex]
       | _   -> failwith (sprintf "unknown prim1 operator: %s" op))
  | Prim2 (op, exp1, exp2)  ->
      cExpr varEnv funEnv depth exp1
    @ cExpr varEnv funEnv depth exp2
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
  | Andalso (exp1, exp2)    ->
    let lhs = cExpr varEnv funEnv depth exp1
    let rhs = cExpr varEnv funEnv depth exp2
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
  | Orelse (exp1, exp2)     ->
    let lhs = cExpr varEnv funEnv depth exp1
    let rhs = cExpr varEnv funEnv depth exp2
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
  | Call (name, argExprs)   ->
    let cArgs = List.concat (List.map (fun e -> cExpr varEnv funEnv depth e) argExprs)
    let funId = getFunId name funEnv
    let funArgCount = List.length (snd (getFunSig (getFunDec funId funEnv)))
    if cArgs.Length = funArgCount
    then cArgs @ [CALL funId]
    else failwith (sprintf "function %s expects %d args, got %d" name funArgCount cArgs.Length)
and cStmtOrDec varEnv funEnv depth = function
  | Dec (typ, name)      -> allocateLocVar varEnv name depth false, []
  | Stmt stm             -> cStmt varEnv funEnv depth stm
and cBlock varEnv funEnv depth = function
  | Block stmtOrDecs ->
      // discard all variables already declared at- and below the current depth
      let discardScopes currentDepth locals =
        locals
        |> mapKeys
        |> Seq.choose (fun ((name, varDepth) as x) ->
            if varDepth >= currentDepth
            then Some(x)
            else None
          )
        |> Seq.fold (fun locals key ->
             // it may look like we try to find locals with scope set to false here,
             // however, it is just record notation for setting InScope to false
             Map.add key { Map.find key locals with InScope = false } locals
           ) locals

      let varEnv, stmts =
        List.fold (fun (varEnv, stmts) elem ->
                    let varEnv, stmt = (cStmtOrDec varEnv funEnv depth elem)
                    varEnv, (List.rev stmt) @ stmts
                  ) (varEnv, []) stmtOrDecs
      let varEnv = { varEnv with Locals = discardScopes depth varEnv.Locals }
      varEnv, BLOCK (BVoid) :: (List.rev stmts) @ [END]
  | anyOtherStmt ->
    failwith (sprintf "attempted to cBlock compile a non-block statement: %A" anyOtherStmt)
and cStmt varEnv funEnv depth = function
  | If (exp, stmT, stmF) ->
    let varEnvT, stmTCode = cStmt varEnv funEnv (depth+1) stmT
    let varEnvF, stmFCode = cStmt varEnvT funEnv (depth+1) stmF
    varEnvF, cExpr varEnv funEnv depth exp
             @ IF (BVoid) :: stmTCode
             @ ELSE :: stmFCode
             @ [END]
  | While (exp, stm)     ->
    let loopVarEnv, loopCode = cStmt varEnv funEnv (depth+1) stm
    loopVarEnv, BLOCK (BVoid)
                :: LOOP (BVoid)
                  :: cExpr varEnv funEnv depth exp @ I32_EQZ :: BR_IF 1uy
                  :: loopCode
                  @  BR 0uy
                :: [END;
                END]
  | Expr exp
  | Return (Some exp)    -> varEnv, cExpr varEnv funEnv depth exp
  | Return None          -> varEnv, []
  | Block _ as b         -> cBlock varEnv funEnv (depth+1) b

let cProgram (Prog topdecs) =
  let emptyFunEnv = { Ids = Map.empty; Types = Map.empty; Decs = Map.empty; }
  let emptyVarEnv = { Locals = Map.empty; Globals = Map.empty; }

  // add printi and printc import functions, required for spec compliance with MicroC
  // the order of insertion is important, due to printIntFunctionIndex and
  // printCharFunctionIndex.
  let topdecs = Funsig(true, None, "printi", [(TypI, "i")])
                :: Funsig(true, None, "printc", [(TypI, "c")])
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
    | Vardec (typ, name) ->
      allocateGloVar varEnv name, funEnv
    | _ -> varEnv, funEnv
  let varEnv, funEnv = List.fold envFolder (emptyVarEnv, funEnv) topdecs

  let exportFolder topdec exports =
    match topdec with
    | Fundec(exported, _, name, _, _) as f when exported ->
      Map.add name (getFunId name funEnv) exports
    | _ -> exports
  let exports = List.foldBack exportFolder topdecs Map.empty

  let funCodeFolder topdec (varEnvs, code) =
    match topdec with
    | Fundec(_, _, _, args, block) as f ->
      let argsToLocVars = List.fold (fun env (_, name) -> allocateLocVar env name 0 true)
      let varEnv = argsToLocVars varEnv args
      let varEnv, funCode = cBlock varEnv funEnv 0 block
      // in WASM, functions are a special kind of block without the block OP code, so discard it
      varEnv :: varEnvs, (List.tail funCode) :: code
    | _ -> varEnvs, code
  let varEnvs, funCode =
    List.foldBack funCodeFolder (funEnv.Decs |> mapValues
                                             |> List.ofSeq) ([], [])

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
    :: i2leb (List.length argTyps)                   // num of args
    @  List.map (fun _ -> getValueTypeCode I32) argTyps // arg types
    @  match retTyp with                                // number of return types (can only be 1, as of WASM v1.0)
       | Some _ -> [1uy; getValueTypeCode I32]          // the return type
       | None   -> [0uy]                                // ... or nothing returned
  if (Map.count funEnv.Types) > 0 then
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
    // TODO: allow import of global variables.
    //       requires updating the import compilation fold function
    //       and extending of the abstract syntax, the lexer and parser.
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

  //#region Memory section [5] W.I.P.

  //#endregion

  //#region Global section [6]
  let globalSectMapper (name, id) =
    let mutabilityCode = 1uy // all globals in MicroC are mutable
    getValueTypeCode I32
    :: mutabilityCode
    :: List.concat (List.map (fun i -> emitbytes i []) [I32_CONST 0; END])
  let globals = (List.head varEnvs).Globals
  if (Map.count globals) > 0 then
    let globalSectionData = i2leb (Map.count globals)
                          @ (globals |> mapToSeqSortedBy snd
                                     |> Seq.map globalSectMapper
                                     |> ofSeqConcat)
    writeSection GLOBAL globalSectionData
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
    let decCount = Seq.sumBy (fun lv -> if lv.FunArg then 0 else 1) (mapValues varEnv.Locals)
    let decCountBytes = i2leb decCount
    let localDecBytes = if decCount = 0
                        then decCountBytes
                        // between MicroC and Wasm there is only 1 data type available: i32
                        // so # of params is also # of type i32 params
                        else decCountBytes @ decCountBytes @ [getValueTypeCode I32]
    let codeBytes = localDecBytes @ code2bytes instrs
    i2leb codeBytes.Length @ codeBytes
  if (List.length varEnvAndFunCode) > 0 then
    let codeSectionData = i2leb (funCode.Length)
                          @ List.concat (List.map codeSectMapper varEnvAndFunCode)
    writeSection CODE codeSectionData
  //#endregion

  //#region Data section [11] W.I.P.

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
