(module
  (func $changeValue (import "imports" "changeValue") (param i32 i32))
  (func $start_func
    i32.const 0
    i32.const 42
    call $changeValue
  )
  (start $start_func)
)
