(module
  (memory 1)
  (func $changeGlobalInteger (param i32)
    i32.const 0 ;; address of global variable
    get_local 0 ;; the number to change the global variable to
    i32.store   ;; store it to linear memory
  )
  (func (export "returnGlobalInteger") (result i32)
    i32.const 0 ;; address of global variable
    i32.load    ;; load it from linear memory
  )
)
