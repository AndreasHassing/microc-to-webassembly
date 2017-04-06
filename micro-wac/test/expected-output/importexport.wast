(module
  (func $printi (import "imports" "printi") (param i32))
  (func $test (export "test") (result i32)
    (local i32)
    i32.const 5
    i32.const 3
    i32.add
    tee_local 0
    call $printi
    get_local 0)
)
