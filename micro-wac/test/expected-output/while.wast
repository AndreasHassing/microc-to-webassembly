(module
  (func (export "while") (param i32 i32) (result i32)
    (local i32)
    (set_local 1 (i32.const 1))
    (block
      (loop
        (br_if 1 (i32.eqz (get_local 0)))
        (set_local 1 (i32.mul (get_local 0) (get_local 1)))
        (set_local 0 (i32.sub (get_local 0) (i32.const 1)))
        (br 0)
      )
    )
    (get_local 1)
  )
)
