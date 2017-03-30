# Inspecting WASM
> The binary output of compiling to WebAssembly.

This has been done several times before, but doing it for oneself can be a very knowledge-yielding exercise and can assist in achieving a better understanding of WebAssembly in general.

### simple.wast
```webassembly
(module
  (func $printi (import "imports" "printi") (param i32))
  (func $start
    (local i32)
    i32.const 5
    i32.const 3
    i32.add
    set_local 0
    get_local 0
    call $printi
  )
  (start $start)
)
```

Compiled to WASM (slightly more detailed than `wast2wasm -v`)
```
; Header
00 61 73 6D                  ; translates to '\0asm', WebAssembly's magic number. Is there for quick file type detection.
01 00 00 00                  ; Version number (1)

; Section "Type" (1) - spec: required
01                           ; Section code: 0x1 = Type
08                           ; Section size: 0x8 bytes (the following 8 bytes are contained within this section)
02                           ; Number of types in section: 2 types
; type #0
60                           ; Type code: 0x60 = function
01                           ; Number of params: 0x1
7F                           ; Param 1's type: 0x7F = i32
00                           ; Number of results: 0x0
; type #1
60                           ; Type code: 0x60 = function
00                           ; Number of params: 0x0
00                           ; Number of results: 0x0

; Section "Import" (2) - spec: optional
02                           ; Section code: 0x2 = Import
12                           ; Section size: 0x12 bytes (decimal: 18)
01                           ; Number of imports: 0x1
; import header #0
07                           ; String length: 0x7 bytes
69 6D 70 6F 72 74 73         ; Import module name: ascii => "imports"
06                           ; String length: 0x6 bytes
70 72 69 6E 74 69            ; Import field name: ascii => "printi"
00                           ; Import kind: 0x0 (using external_kinds table: 0 = function) https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md#external_kind
00                           ; Import signature index: 0x0 (referring to function signature at type #0)

; Section "Function" (3) - spec: required
03                           ; Section code: 0x3 = Function
02                           ; Section size: 0x2 bytes
01                           ; Number of functions: 0x1
01                           ; Function #0 signature index: 0x1 (refers to type #1)

; Section "Start" (8) - spec: optional
08                           ; Section code: 0x8 = Start
01                           ; Section size: 0x1 bytes
01                           ; Start function index (refers to function #1)

; Section "Code" (10) - spec: required
0A                           ; Section code: 0xa = Code (decimal: 10)
11                           ; Section size: 0x11 bytes (decimal: 17)
01                           ; Number of functions: 0x1
0F                           ; Function body size: 0xf (decimal: 15)
01                           ; Local variable declaration count: 0x1
01                           ; Local type count: 0x1 (?)
7F                           ; Type identifier for i32
41                           ; i32.const (the next opcode will be a literal)
05                           ; i32 literal: 0x05
41                           ; i32.const
03                           ; i32 literal: 0x03
6A                           ; i32.add (pop two values from stack, and add them, push result onto stack)
21                           ; set_local, take the next value pushed to the stack, and assign local at that index with stack pop value
00                           ; local variable index: 0x0
20                           ; get_local, take the next value pushed to the stack, and push local at that index onto stack
00                           ; local variable index: 0x0
10                           ; call function at index from next value pushed to stack
00                           ; function index: 0x0
0B                           ; end
```
