(module
  (func $printi (import "imports" "printi") (param i32))
  (memory 1)
  (func $start
    i32.const 0
    i32.const 5
    i32.const 3
    i32.add
    i32.store offset=0

    i32.const 0
    i32.load offset=0
    call $printi
  )
  (start $start)
)
