(* File MicroC/Absyn.fs
   Abstract syntax of micro-C, an imperative language.
   sestoft@itu.dk 2009-09-25

   Must precede Interp.fs, Comp.fs and Contcomp.fs in Solution Explorer
 *)

module Absyn

type Typ =
  | TypI                             (* Type int                    *)
  | TypC                             (* Type char                   *)
  | TypA of Typ * int option         (* Array type                  *)
  | TypP of Typ                      (* Pointer type                *)

and Expr =
  | Access of Access                 (* x    or  *p    or  a[e]     *)
  | Assign of Access * Expr          (* x=e  or  *p=e  or  a[e]=e   *)
  | Addr of Access                   (* &x   or  &*p   or  &a[e]    *)
  | Cond of Expr * Expr * Expr       (* x ? a : b                   *)
  | CstI of int                      (* Constant                    *)
  | Prim1 of string * Expr           (* Unary primitive operator    *)
  | Prim2 of string * Expr * Expr    (* Binary primitive operator   *)
  | Andalso of Expr * Expr           (* Sequential and              *)
  | Orelse of Expr * Expr            (* Sequential or               *)
  | Call of string * Expr list       (* Function call f(...)        *)

and Access =
  | AccVar of string                 (* Variable access        x    *)
  | AccDeref of Expr                 (* Pointer dereferencing  *p   *)
  | AccIndex of Access * Expr        (* Array indexing         a[e] *)

and Stmt =
  | If of Expr * Stmt * Stmt         (* Conditional                 *)
  | While of Expr * Stmt             (* While loop                  *)
  | Expr of Expr                     (* Expression statement   e;   *)
  | Return of Expr option            (* Return from method          *)
  | Block of Stmtordec list          (* Block: grouping and scope   *)

and Stmtordec =
  | Dec of Typ * string              (* Local variable declaration  *)
  | Stmt of Stmt                     (* A statement                 *)

and Topdec =
  | Fundec of bool * Typ option * string * (Typ * string) list * Stmt
  | Vardec of Typ * string

and Program =
  | Prog of Topdec list
