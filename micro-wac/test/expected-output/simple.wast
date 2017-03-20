(module
  (func $printi (import "imports" "printi") (param i32))
  (func $println (import "imports" "println") (param i32))
  (func $start
    (local i32)
    i32.const 5
    i32.const 3
    i32.add
    set_local 0
    get_local 0
    call $printi
  )
  (start $start)
)
