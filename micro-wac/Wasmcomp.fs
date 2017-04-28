﻿(* File Wasmcomp.fs
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

open System.IO
open Absyn
open WasmMachine
open System.Text
open System

(* ------------------------------------------------------------------- *)

// Recipe for progress (compiler recipe for MicroC -> WebAssembly)
// 1. Construct WASM headers
// 2. Construct type section (count distinct function types)

type FunEnv = { Ids:   Map<string, int>;
                Types: Map<(Typ option * Typ list), int>;
                Decs:  Map<int, Topdec> }

/// VarEnv local keys contain their name and the depth of declaration,
/// their values contains the ID of the variable and if it is declared in the current scope.
type LocVar = { Id: int; InScope: bool; }
type VarEnv = { Locals:  Map<(string * int), LocVar>;
                Globals: Map<string, int>; }

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

let rec varAccess varEnv ((x, depth) as var) =
  match (getLocalVarId var varEnv, getGlobalVarId x varEnv) with
  | Some varId, _    -> [GET_LOCAL varId]
  | None, Some varId -> [GET_GLOBAL varId]
  | _                -> failwith (sprintf "can't find variable: %s" x)

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

/// Depth is not used for globals
let allocateGloVar varEnv name =
  { varEnv with Globals = Map.add name (Map.count varEnv.Globals) varEnv.Globals }

let allocateLocVar varEnv name depth =
  let key = name, depth
  let id = if Map.containsKey key varEnv.Locals
           then (Map.find key varEnv.Locals).Id
           else Map.count varEnv.Locals
  let locVar = { Id = id; InScope = true; }
  { varEnv with Locals = Map.add key locVar varEnv.Locals }

let accessVar varEnv depth = function
  | AccVar x            -> varAccess varEnv (x, depth)
  | AccDeref exp        -> [NOP]// *exp
  | AccIndex (acc, exp) -> [NOP]// acc[exp]

let rec cExpr varEnv funEnv depth = function
  | Access acc              -> accessVar varEnv depth acc
  | Assign (acc, exp)       -> [NOP]
  | Addr acc                -> [NOP]
  | Cond (bExp, expT, expF) -> cExpr varEnv funEnv depth expT
                               @ cExpr varEnv funEnv depth expF
                               @ cExpr varEnv funEnv depth bExp
                               @ [SELECT]
  | CstI i                  -> [I32_CONST i]
  | Prim1 (op, exp)         ->
      cExpr varEnv funEnv depth exp
    @ (match op with
       | "!" -> [I32_EQZ]
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
  | Dec (typ, name)      -> allocateLocVar varEnv name depth, []
  | Stmt stm             -> cStmt varEnv funEnv depth stm
and cBlock varEnv funEnv depth = function
  | Block stmtOrDecs ->
      let discardScopes currentDepth locals =
        locals |> Map.toSeq
        |> Seq.map fst // get keys
        |> Seq.choose (fun ((name, varDepth) as x) -> // get keys with depth >= currentDepth
            if varDepth >= currentDepth
            then Some(x)
            else None
          )
        |> Seq.fold (fun locals key ->
             // it could look like we find locals with scope set to false here,
             // however it is just record notation for setting InScope to false
             Map.add key { Map.find key locals with InScope = false } locals
           ) locals

      let varEnv, stmts =
        List.fold (fun (varEnv, stmts) elem ->
                    let varEnv, stmt = (cStmtOrDec varEnv funEnv depth elem)
                    varEnv, stmt @ stmts
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

  let typeFolder funEnv = function
    | Funsig _
    | Fundec _ as f -> updTypes f funEnv
    | _             -> funEnv
  let funEnv = List.fold typeFolder emptyFunEnv topdecs

  let importsFolder (funEnv, imports) = function
    | Funsig(imported, _, name, _) as f when imported ->
      let funId = Map.count funEnv.Ids
      (
        { funEnv with Ids = Map.add name funId funEnv.Ids
                      Decs = Map.add funId f funEnv.Decs },
        Map.add name (Map.find (getFunSig f) funEnv.Types) imports
      )
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
      let argsToLocVars = List.fold (fun env (_, name) -> allocateLocVar env name 0)
      let varEnv = argsToLocVars varEnv args
      let varEnv, funCode = cBlock varEnv funEnv 0 block
      // in WASM, functions are a special kind of block without the block OP code, so discard it
      varEnv :: varEnvs, (List.tail funCode) :: code
    | _ -> varEnvs, code
  let varEnvs, funCode = List.foldBack funCodeFolder (funEnv.Decs |> Map.toSeq |> Seq.map snd |> List.ofSeq) ([], [])

  funEnv, varEnvs, imports, exports, funCode

let code2bytes code =
  List.foldBack emitbytes code []

let compileWasmBinary (funEnv, varEnvs, imports, exports, funCode) =
  // open binary file stream
  let writer filename = new BinaryWriter(File.Open(filename, FileMode.Create))
  use wasmFile = writer "comptest.wasm"
  // stream writer helper functions
  let writeBytes bytes = List.iter (fun (b : byte) -> wasmFile.Write(b)) (List.concat bytes)
  let writeVarInt n = [i2bNoPad n] |> writeBytes

  //#region Write to WASM file
  let wasmHeader = [i2b 0x0061736D; i2b 0x01000000]
  writeBytes wasmHeader
  // Type (1) header

  let funBinary = List.map code2bytes funCode
  writeBytes funBinary
  //#endregion

let compileToFile program = (cProgram >> compileWasmBinary) program
