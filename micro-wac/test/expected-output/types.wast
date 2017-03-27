(module
  (func $one) ;; type 1
  (func $two (param i32)) ;; type 2
  (func $three (param i32) (result i32) ;; type 3
    (i32.const 5))
  (func $four (param i32 i32)) ;; type 4
  (func $five (param i32)) ;; type 2
)
