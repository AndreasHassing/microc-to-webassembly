(module
  (func $printi (import "imports" "printi") (param i32))
  (func $test (export "test") (result i32)
    (local i32 i32 i32)
    (block
      i32.const 42
      tee_local 2
      drop
    )
    i32.const 5
    i32.const 3
    i32.add
    tee_local 0
    call $printi
    get_local 0)
  (func $hasparam (param i32))
  (func $hasparams (param i32 i32))
  (func $hasresult (result i32)
    i32.const 42)
  (func $hasparamsandresult (param i32 i32) (result i32)
    get_local 0
    get_local 1
    i32.add)
  (func $haslocali32
    (local i32))
  (func $haslocali64
    (local i64))
  (func $hasseverallocals
    (local i32 i64 i64 i32)
    i32.const 42
    set_local 0)
  (func $hasmanylocals
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 ))
  (global $gb (mut i64) (i64.const 0))
)
