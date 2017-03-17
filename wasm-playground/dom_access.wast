(module
  (func $changeValue (import "imports" "changeValue") (param i32 i32))
  (func $start_func
    (call $changeValue (i32.const 0) (i32.const 42))
  )
  (start $start_func)
)
