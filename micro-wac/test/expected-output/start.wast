(module
  (func $printi (import "imports" "printi") (param i32))
  (func $printc (import "imports" "printc") (param i32))
  (func $start
    i32.const -42
    call $printi

    i32.const 10
    call $printc
  )
  (start $start)
)
