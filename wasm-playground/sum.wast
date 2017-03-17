(module                                                     ;; specify a module
  (func $sum (export "sum") (param i32 i32) (result i32)    ;; define and export a function called "sum"
    (local i32)                                             ;; define a local variable (sum)
    (block                                                  ;; start a new loop block
      (loop                                                 ;; define a loop
        (br_if 1 (i32.eq (get_local 0) (get_local 1)))      ;; branch out of loop if a = b
        (set_local 2 (i32.add (get_local 2) (get_local 0))) ;; sum = sum + a
        (set_local 0 (i32.add (get_local 0) (i32.const 1))) ;; a = a + 1
        (br 0)                                              ;; branch to top of loop
      )
    )
    (i32.add (get_local 2) (get_local 0))                   ;; add sum to stack (and return the function)
  )
)
