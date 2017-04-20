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

type VarEnv = { Locals:  Map<string, int>;
                Globals: Map<string, int>; }

let rec varAccess varEnv x =
  let exists x m = Map.containsKey x m
  match (exists x varEnv.Locals, exists x varEnv.Globals) with
  | true, _     -> [GET_LOCAL (Map.find x varEnv.Locals)]
  | false, true -> [GET_GLOBAL (Map.find x varEnv.Globals)]
  | _           -> failwith (sprintf "can't find variable: %s" x)

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

let allocateVar isGloVar varEnv name =
  let addToMap name map = Map.add name (Map.count map) map
  match isGloVar with
  | true  -> { varEnv with Globals = addToMap name varEnv.Globals }
  | false -> { varEnv with Locals  = addToMap name varEnv.Locals  }

let accessVar varEnv = function
  | AccVar x            -> varAccess varEnv x
  | AccDeref exp        -> [NOP]// *exp
  | AccIndex (acc, exp) -> [NOP]// acc[exp]

let rec cExpr varEnv funEnv = function
  | Access acc              -> accessVar varEnv acc
  | Assign (acc, exp)       -> [NOP]
  | Addr acc                -> [NOP]
  | Cond (bExp, expT, expF) -> cExpr varEnv funEnv expT
                               @ cExpr varEnv funEnv expF
                               @ cExpr varEnv funEnv bExp
                               @ [SELECT]
  | CstI i                  -> [I32_CONST i]
  | Prim1 (op, exp)         ->
      cExpr varEnv funEnv  exp
    @ (match op with
       | "!" -> [I32_EQZ]
       | _   -> failwith (sprintf "unknown prim1 operator: %s" op))
  | Prim2 (op, exp1, exp2)  ->
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
  | Andalso (exp1, exp2)    ->
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
  | Orelse (exp1, exp2)     ->
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
  | Call (name, argExprs)   ->
    let cArgs = List.concat (List.map (fun e -> cExpr varEnv funEnv e) argExprs)
    let funId = getFunId name funEnv
    let funArgCount = List.length (snd (getFunSig (getFunDec funId funEnv)))
    if cArgs.Length <> funArgCount
    then failwith (sprintf "function %s expects %d args, got %d" name cArgs.Length funArgCount)
    else cArgs @ [CALL funId]
and cStmtOrDec varEnv funEnv = function
  | Dec (typ, name)      -> allocateVar false varEnv name, []
  | Stmt stm             -> varEnv, (cStmt varEnv funEnv stm)
and cBlock varEnv funEnv = function
  | Block stmtOrDecs ->
      let varEnv, stmts =
        List.foldBack (fun elem (varEnv, stmts) ->
                        let varEnv, stmt = (cStmtOrDec varEnv funEnv elem)
                        varEnv, stmt @ stmts
                      ) stmtOrDecs (varEnv, [])
      BLOCK (BVoid) :: [END]
  | anyStmt -> failwith (sprintf "attempted to cBlock compile a non-block statement: %A" anyStmt)
and cStmt varEnv funEnv = function
  | If (exp, stmT, stmF) ->
    cExpr varEnv funEnv exp
    @ IF (BVoid) :: cStmt varEnv funEnv stmT
    @ ELSE :: cStmt varEnv funEnv stmF
    @ [END]
  | While (exp, stm)     ->
      BLOCK (BVoid)
      :: LOOP (BVoid)
        :: cExpr varEnv funEnv exp @ I32_EQZ :: BR_IF 1uy
        :: cStmt varEnv funEnv stm
        @  BR 0uy
      :: [END;
      END]
  | Expr exp
  | Return (Some exp)    -> cExpr varEnv funEnv exp
  | Return None          -> []
  | Block _ as b         -> cBlock varEnv funEnv b

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
        { funEnv with Ids = Map.add name funId funEnv.Ids },
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
      allocateVar true varEnv name, funEnv
    | _ -> varEnv, funEnv
  let varEnv, funEnv = List.fold envFolder (emptyVarEnv, funEnv) topdecs

  let exportFolder topdec exports =
    match topdec with
    | Fundec(exported, _, name, _, _) as f when exported ->
      Map.add name (getFunId name funEnv) exports
    | _ -> exports
  let exports = List.foldBack exportFolder topdecs Map.empty

  let rec buildLocalVarEnv stmt varEnv =
    let stmtOrDecFolder stmtOrDec varEnv =
      match stmtOrDec with
      | Dec (_, name) -> { varEnv with Locals =  }
      | Stmt stm      -> buildLocalVarEnv stm varEnv
    match stmt with
    | Block b            ->
    | If (_, stm1, stm2) ->
    | While (_, stm)     ->
    | _                  ->

  let funCodeFolder topdec code =
    match topdec with
    | Fundec(_, _, _, args, block) as f ->
      let argsToLocVars = List.fold (fun env (_, name) -> allocateVar false env name)
      let varEnv = argsToLocVars varEnv args
      let varEnv = buildLocalVarEnv block varEnv
      // in WASM, functions are a special kind of block without the block OP code, so discard it
      let code = (List.tail (cBlock varEnv funEnv block)) :: code
    | _ -> code
  let funCode = List.foldBack funCodeFolder (funEnv.Decs |> Map.toSeq |> Seq.map snd |> List.ofSeq) []

  (funEnv, varEnv, imports, exports, funCode)
