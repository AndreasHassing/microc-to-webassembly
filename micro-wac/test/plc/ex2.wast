(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func))
  (import "imports" "printi" (func (;0;) (type 0)))
  (import "imports" "printc" (func (;1;) (type 1)))
  (func (;2;) (type 2)
    i32.const 4
    i32.load
    call 0
    i32.const 0
    i32.load
    call 0
    i32.const 0
    i32.const 4
    i32.store
    i32.const 0
    i32.load
    call 0
    i32.const 48
    i32.const 8
    i32.load
    i32.store
    i32.const 48
    i32.load
    i32.load
    call 0
    i32.const 0
    i32.load
    i32.const 227
    i32.store
    i32.const 0
    i32.load
    call 0
    i32.const 4
    i32.load
    call 0
    i32.const 4
    i32.const 12
    i32.store
    i32.const 4
    i32.load
    call 0
    i32.const 0
    i32.const 0
    i32.load
    i32.store
    i32.const 0
    i32.load
    i32.load
    call 0
    i32.const 0
    i32.const 8
    i32.load
    i32.store
    i32.const 8
    i32.load
    i32.const 14
    i32.store
    i32.const 8
    i32.const 0
    i32.add
    i32.load
    call 0
    i32.const 8
    i32.load
    i32.const 9
    i32.add
    i32.const 114
    i32.store
    i32.const 8
    i32.const 9
    i32.add
    i32.load
    call 0
    i32.const 52
    i32.const 2
    i32.add
    i32.const 0
    i32.load
    i32.store
    i32.const 52
    i32.const 2
    i32.add
    i32.load
    call 0
    i32.const 52
    i32.const 2
    i32.add
    i32.load
    i32.const 52
    i32.load
    i32.const 2
    i32.add
    i32.load
    i32.eq
    call 0
    i32.const 92
    i32.const 8
    i32.store
    i32.const 92
    i32.load
    i32.const 2
    i32.add
    i32.const 92
    i32.load
    i32.load
    i32.const 2
    i32.add
    i32.eq
    call 0)
  (func (;3;) (type 2)
    call 2)
  (memory (;0;) 1)
  (start 3))
