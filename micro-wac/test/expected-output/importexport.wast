(module
  (func $printi (import "imports" "printi") (param i32))
  (func $test (export "test") (result i32)
    (local i32)
    i32.const 40
    i32.const 2
    i32.add
    tee_local 0
    call $printi
    get_local 0)
  (func $internaltest
    call $test
    drop)
)
