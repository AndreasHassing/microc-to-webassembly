(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32)))
  (type (;3;) (func))
  (import "imports" "printi" (func (;0;) (type 0)))
  (import "imports" "printc" (func (;1;) (type 1)))
  (memory 1)
  (func $increment (type 2) (param i32)
    get_local 0
    get_local 0
    i32.load
    i32.const 1
    i32.add
    i32.store
  )
  (func $start (type 3)
    (; x = 0 ;)
    i32.const 0
    i32.const 0
    i32.store
    (; print x ;)
    i32.const 0
    i32.load
    call 0
    (; println ;)
    i32.const 10
    call 1

    (; increment(&x) ;)
    i32.const 0
    call $increment
    (; print x ;)
    i32.const 0
    i32.load
    call 0
    (; println ;)
    i32.const 10
    call 1
  )
  (start $start)
)
