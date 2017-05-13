(module
  (func $printi (import "imports" "printi") (param i32))
  (memory 1)
  (func $test (export "test") (result i32)
    i32.const 0
    i32.const 40
    i32.const 2
    i32.add
    i32.store offset=0

    i32.const 0
    i32.load offset=0
    call $printi

    i32.const 0
    i32.load offset=0)
)
