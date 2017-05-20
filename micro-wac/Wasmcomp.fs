(* File Wasmcomp.fs
   A compiler from micro-C, a sublanguage of the C language, to
   WebAssembly. Direct (forwards) compilation without optimization
   of jumps to jumps, tail-calls etc.

   based on Comp.fs by
   sestoft@itu.dk * 2009-09-23, 2011-11-10
*)

/// Wasmcomp contains functions to compile a MicroC program (as an AST) to a WebAssembly binary.
module MicroWac.Wasmcomp

open Absyn
open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open WasmMachine

//#region Environment types
/// FunEnv is a function environment that contains the ID's of functions (lookup by name),
/// a list of unique function types (signatures) and all function declarations.
type FunEnv = { Ids:   Map<string, int>;
                Types: Map<(Typ option * Typ list), int>;
                Decs:  Map<int, Topdec>; }

/// GloVar is a global variable. A global variable has an absolute address
/// in linear memory, and a type.
type GloVar = { Addr: int; Type: Typ; }

/// ArgVar is a function argument variable. A argument variable has an ID and a type.
type ArgVar = { Id: int; Type: Typ; }

/// LocVar is a local variable, stored in linear memory. It has an address,
/// an offset (each function offset is equal to the address of the next free piece of memory),
/// a type, a depth (to ensure consistent scoping) and a boolean,
/// indicating whether the variable is in scope or not.
type LocVar = { Addr: int; Offset: int; Type: Typ; Depth: int; InScope: bool; }

/// Var is a discriminated union type that encapsulates the differently
/// behaving variable types.
///
/// Note to OO devs: it's sort of like an abstract class, only better ;-).
type Var =
  | GloVar of GloVar
  | ArgVar of ArgVar
  | LocVar of LocVar

/// VarOp is a variable operation which can be either:
/// Setting a variable to the result of some expression,
/// and getting the value or address of a variable.
type VarOp =
  | Set of Expr
  | GetValue
  | GetAddr

/// VarEnv is a variable environment that stores state regarding the variables
/// in a function, the next free argument ID (used for local argument variables),
/// the next free address and current offset (used for variables in linear memory).
///
/// Also keeps track of the current depth, to ensure that scopes do not bleed into eachother.
type VarEnv = { Vars: (string * Var) list;
                NextFreeArgId: int;
                NextFreeAddress: int;
                CurrentOffset: int;
                CurrentDepth: int; }

/// lookup finds variables that are in scope. Looks through
/// the latest declared local variables, then the function arguments and then
/// the global variables, in that order.
let rec lookup env depth x =
  match env with
  | []         -> failwith (sprintf "%s not found" x)
  | (y, v)::yr -> if x=y then
                    match v with
                    | LocVar lv when lv.Depth > depth || not lv.InScope ->
                      lookup yr depth x
                    | _ -> v
                  else lookup yr depth x
//#endregion

//#region Local and Global variable helper functions
/// varWidth returns the apt width of a variable type, as needed when
/// allocating variables. Chars are 1 byte wide, arrays are
/// <their type width> * <their length> wide, and anything else is 4 bytes.
let rec varWidth = function
  | TypC            -> 1
  | TypA (typ, num) -> if num.IsSome && num.Value > 0
                       then varWidth typ * num.Value
                       else varWidth typ
  | _               -> 4

/// varAssOp takes a boolean for whether the variable is a function argument
/// (as some special rules apply to those) and the type of the variable.
/// It returns an access instruction code that is missing its immediate (index or offset).
/// Used to generate variable accesses on arbitrary variable types.
let rec varAccOp isFunArg = function
  | _ when isFunArg -> GET_LOCAL
  | TypC            -> I32_LOAD8_U
  | TypA (typ, _)   -> varAccOp isFunArg typ
  | _               -> I32_LOAD

/// varAssOp takes a boolean for whether the variable is a function argument
/// (as some special rules apply to those) and the type of the variable.
/// It returns an assignment instruction code that is missing its immediate (index or offset).
/// Used to generate variable assignments on arbitrary variable types.
let rec varAssOp isFunArg = function
  | _ when isFunArg -> SET_LOCAL
  | TypC            -> I32_STORE8
  | TypA (typ, _)   -> varAssOp isFunArg typ
  | _               -> I32_STORE

/// updateNextFreeAddress takes the type of the allocated variable
/// and a variable environment. It returns an environment with
/// the next free address variable updated.
let updateNextFreeAddress allocdTyp varEnv =
  { varEnv with NextFreeAddress = varEnv.NextFreeAddress + (varWidth allocdTyp) }

/// allocateGloVar allocates a global variable in linear memory, then returns the
/// given variable environment with the next free address variable updated.
let allocateGloVar varEnv name typ =
  let gloVar = GloVar { Addr = varEnv.NextFreeAddress; Type = typ; }
  updateNextFreeAddress typ { varEnv with Vars = (name, gloVar) :: varEnv.Vars; }

/// allocateLocVar allocates a local variable in linear memory, then returns the
/// given variable environment with the next free address variable updated.
let allocateLocVar varEnv name typ =
  let locVar = LocVar { Addr = varEnv.NextFreeAddress;
                        Offset = varEnv.CurrentOffset;
                        Type = typ;
                        Depth = varEnv.CurrentDepth;
                        InScope = true; }
  updateNextFreeAddress typ { varEnv with Vars = (name, locVar) :: varEnv.Vars }
//#endregion

//#region Function declaration helper functions
/// getFunSig returns the function signature of some function declaration or signature.
let getFunSig f =
  let getArgTypes types (typ, _) = typ :: types
  match f with
  | Funsig(_, retTyp, _, args)
  | Fundec(_, retTyp, _, args, _) -> (retTyp, List.fold getArgTypes [] args)
  | _ -> failwith (sprintf "can't get function signature of %A" f)

/// updTypes takes a function declaration or signature and updates the type section of
/// a given function environment.
let updTypes func funEnv =
  let funsig = getFunSig func
  if Map.containsKey funsig (funEnv.Types)
  then funEnv
  else { funEnv with Types = Map.add funsig (Map.count funEnv.Types) funEnv.Types }

/// getFunId returns a function ID by name in a given function environment.
let getFunId name funEnv =
  Map.find name funEnv.Ids

/// getFunDec returns a function declaration by function ID in a given function environment.
let getFunDec funId funEnv =
  Map.find funId funEnv.Decs
//#endregion

/// Get the values of a map, as a sequence.
let mapValues map = map |> Map.toSeq |> Seq.map snd

/// shallow steps a single up in depth in the variable environment.
/// The keyword 'raise' was already taken.
let shallow varEnv = { varEnv with CurrentDepth = varEnv.CurrentDepth - 1 }

/// deepen increases the current depth of the variable environment.
let deepen varEnv = { varEnv with CurrentDepth = varEnv.CurrentDepth + 1 }

/// accessVar compiles the access of a MicroC variable to instruction codes.
/// Specific variable behavior (getting the value of, getting the the address of or
/// setting the value of) is defined by the caller in `op`.
///
/// XXX: this is probably the least clean piece of code in the project - if work is continued, put the effort here first
let rec accessVar varEnv funEnv op = function
  | AccVar x ->
    let var = lookup varEnv.Vars varEnv.CurrentDepth x
    match var with | LocVar lv -> match op with
                                  | GetValue -> match lv.Type with | TypA _ -> [I32_CONST (lv.Addr + lv.Offset)]
                                                                   | _ -> [I32_CONST lv.Addr; varAccOp false lv.Type lv.Offset]
                                  | GetAddr  -> [I32_CONST (lv.Addr + lv.Offset)]
                                  | Set exp  -> I32_CONST lv.Addr
                                                :: cExpr varEnv funEnv exp
                                                @ [varAssOp false lv.Type lv.Offset]
                   | ArgVar av -> match op with
                                  | GetValue -> [varAccOp true av.Type av.Id]
                                  | GetAddr  -> failwith (sprintf "can't get address of function arguments %A" av)
                                  | Set exp  -> cExpr varEnv funEnv exp @ [varAssOp true av.Type av.Id]
                   | GloVar gv -> I32_CONST gv.Addr
                                  :: match op with
                                     | GetValue -> match gv.Type with | TypA _ -> []
                                                                      | _ -> [varAccOp false gv.Type 0]
                                     | GetAddr  -> []
                                     | Set exp  -> cExpr varEnv funEnv exp
                                                   @ [varAssOp false gv.Type 0]
  | AccDeref exp -> cExpr varEnv funEnv exp
                    @ match op with
                      // XXX: chars can't get dereferenced
                      | GetValue -> [varAccOp false TypI 0]
                      | GetAddr  -> []
                      | Set expS -> cExpr varEnv funEnv expS
                                    @ [varAssOp false TypI 0]
  | AccIndex (acc, exp) -> accessVar varEnv funEnv GetAddr acc
                           @ cExpr varEnv funEnv exp
                           // XXX: char's cant be used as arrays
                           @ [I32_CONST 4; I32_MUL]
                           @ [I32_ADD]
                           @ match op with
                             | GetValue -> [varAccOp false TypI 0]
                             | GetAddr  -> []
                             | Set expS -> cExpr varEnv funEnv expS
                                           @ [varAssOp false TypI 0]
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

/// cProgram consumes an MicroC abstract syntax tree and outputs a function environment,
/// a variable environment, imports, exports and the instruction codes of functions.
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

  /// typeFolder can be used to get unique function signatures in the program.
  let typeFolder funEnv = function
    | Funsig _
    | Fundec _ as f -> updTypes f funEnv
    | _             -> funEnv
  let funEnv = List.fold typeFolder emptyFunEnv topdecs

  /// importsFolder can be used to get imports described in the program.
  let importsFolder (funEnv, imports) = function
    | Funsig(imported, _, name, _) as f when imported ->
      let funId = Map.count funEnv.Ids
      ({ funEnv with Ids = Map.add name funId funEnv.Ids
                     Decs = Map.add funId f funEnv.Decs },
       Map.add name (Map.find (getFunSig f) funEnv.Types) imports)
    | _ -> funEnv, imports
  let funEnv, imports = List.fold importsFolder (funEnv, Map.empty) topdecs

  /// envFolder can be used to get function declarations and allocated global variables.
  /// These can then be used to look up functions and globals with their names.
  let envFolder (varEnv, funEnv) = function
    | Fundec(_, _, name, _, _) as f ->
      let funId = Map.count funEnv.Ids
      varEnv, { funEnv with Ids = Map.add name funId funEnv.Ids
                            Decs = Map.add funId f funEnv.Decs }
    | Vardec (typ, name) -> allocateGloVar varEnv name typ, funEnv
    | _ -> varEnv, funEnv
  let varEnv, funEnv = List.fold envFolder (emptyVarEnv, funEnv) topdecs

  /// exportFolder can be used to get exports described in the program.
  let exportFolder topdec exports =
    match topdec with
    | Fundec(exported, _, name, _, _) as f when exported ->
      Map.add name (getFunId name funEnv) exports
    | _ -> exports
  let exports = List.foldBack exportFolder topdecs Map.empty

  /// funCodeFolder can be used to get isolated variable environments and
  /// compiled instruction codes for each function.
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

/// Convert instructions to bytes.
let code2bytes code =
  List.foldBack emitbytes code []

/// Compile a WASM binary from a C program located at fileName.
/// Specifically designed to get the output of a program compiler which yields
/// a function environment, a variable environment, imports, exports and function code.
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

  // XXX: removed after globals were moved to linear memory

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

/// Generate a HTML template for the compiled .wasm file.
let generateHtml (wasmFileName : string) =
  // grab the location of the running compiler .exe to find the WasmBootstrapTemplate
  let microWacPath = Reflection.Assembly.GetEntryAssembly().Location
  let wasmBootstrapTemplatePath = Path.Combine((Path.GetDirectoryName microWacPath), "WasmBootstrapTemplate.html")
  let wasmTemplate =
    seq {
      for line in File.ReadLines(wasmBootstrapTemplatePath) ->
        Regex.Replace(line, "\${{WASM_FILENAME}}", wasmFileName)
    }
  let htmlFileName = (Path.GetFileNameWithoutExtension wasmFileName) + ".html"
  File.WriteAllLines(htmlFileName, wasmTemplate)

/// Compile a MicroC program (as an AST) to a .wasm file.
let compileToFile fileName program withHtml =
  (cProgram >> compileWasmBinary fileName) program
  if withHtml then generateHtml fileName
