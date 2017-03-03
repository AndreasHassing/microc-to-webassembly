;; Run imported log function with 42, WASM AST s-expression

(module
  (func $log (import "imports" "log") (param i32))
  (func (export "run")
    i32.const 42
    call $log)  
)
