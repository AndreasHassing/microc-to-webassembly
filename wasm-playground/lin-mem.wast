(module
  (func $printi (import "imports" "printi") (param i32))
  (func $printc (import "imports" "printc") (param i32))
  (memory 1)
  (func $getIntWithAddress (export "getIntWithAddress") (param i32) (result i32)
    get_local 0 ;; address of int in linear memory, retrieved from function arg
    i32.load    ;; load the int (4 bytes) at the address fetched above
  )
  (func $getCharWithAddress (export "getCharWithAddress") (param i32) (result i32)
    get_local 0 ;; address of char in linear memory, retrieved from function arg
    i32.load8_u ;; load the byte at the address fetched above
  )
  (func $printChar (export "printChar") (param i32)
    get_local 0              ;; address of char in linear memory, retrieved from function arg
    call $getCharWithAddress ;; call the function getCharWithAddress, using the value on the stack as argument
    call $printc             ;; call the imported print char function
  )
  (func $getTest
    i32.const 0
    i32.load offset=9
    call $printi
  )
  (func $start
    i32.const 0     ;; memory position 0
    i32.const -2000 ;; the number -2000
    i32.store       ;; store the number -2000 at memory position 0 (spanning 4 bytes)

    i32.const 4     ;; memory position 4
    i32.const 42    ;; the number: 42
    i32.store       ;; store the number 42 at memory position 4 (spanning 4 bytes)

    i32.const 8     ;; memory position 8
    i32.const 54    ;; 54 in ascii is the char '6'
    i32.store8      ;; store the number 54, cast to a byte, at memory position 8 (spanning 1 byte)

    i32.const 9     ;; memory position 9
    i32.const 62    ;; 62 in ascii is the char '>'
    i32.store8      ;; store the number 62, cast to a byte, at memory position 9 (spanning 1 byte)

    i32.const 0
    i32.const 5
    i32.store8 offset=10

    call $getTest
  )
  (start $start)
)
