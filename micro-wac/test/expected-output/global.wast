(module
  (global $globalInteger (mut i32) (i32.const 0))
  (func $changeGlobalInteger (param i32)
    get_local 0
    set_global $globalInteger
  )
  (func (export "returnGlobalInteger") (result i32)
    get_global $globalInteger
  )
)
