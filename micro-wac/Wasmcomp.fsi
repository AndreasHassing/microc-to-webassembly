module MicroWac.Wasmcomp

open Absyn

/// Compile a MicroC program (as an AST) to a .wasm file.
val compileToFile : wasmFileName:string -> Program -> withHtml:bool -> unit
