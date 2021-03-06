﻿(* File MicroC/WasmMachine.fs

   WasmMachine emits WebAssembly code in the form of
   binary stack machine code.
   abhn@itu.dk 2017-03-30

   Based on:
   Instructions and code emission for a stack-based
   abstract machine * sestoft@itu.dk 2009-09-23
 *)

module MicroWac.WasmMachine

type Index = int
type RelativeDepth = byte

type ValueType =
  | I32
  | AnyFunc
  | Func

type BlockType =
  | BReturn of ValueType                (* single result *)
  | BVoid                               (* 0 results     *)

type MemoryOffset = int

let getValueTypeCode = function
  | I32     -> 0x7fuy
  | AnyFunc -> 0x70uy
  | Func    -> 0x60uy

let getBlockTypeCode = function
  | BReturn t -> getValueTypeCode t
  | BVoid     -> 0x40uy

type Instruction =
  // Language types
  | TYPE of ValueType                  (* argument type                                           *)
  // Control flow operators
  | UNREACHABLE                        (* trap immediately (whatever that means)                  *)
  | NOP                                (* no operation                                            *)
  | BLOCK of BlockType                 (* begin a sequence of expressions, yielding 0 or 1 values *)
  | LOOP of BlockType                  (* begin a block which can also form control flow loops    *)
  | IF of BlockType                    (* begin if expression                                     *)
  | ELSE                               (* begin else expression of if                             *)
  | END                                (* end a block, loop, or if                                *)
  | BR of RelativeDepth                (* break that targets an outer nested block                *)
  | BR_IF of RelativeDepth             (* conditional break that targets an outer nested block    *)
  | RETURN                             (* return zero or one value from this function             *)
  // Call operators
  | CALL of Index                      (* call function by its index                              *)
  // Paremetric operators
  | DROP                               (* ignore value                                            *)
  | SELECT                             (* select one of two values based on condition             *)
  // Variable access
  | GET_LOCAL of Index                 (* read the current value of a local variable              *)
  | SET_LOCAL of Index                 (* set the current value of a local variable               *)
  | TEE_LOCAL of Index                 (* set the current value of a local variable and return it *)
  | GET_GLOBAL of Index                (* get the current value of a global variable              *)
  | SET_GLOBAL of Index                (* set the current value of a global variable              *)
  // Memory-related operators
  | I32_LOAD of MemoryOffset           (* load 4 bytes from linear memory as i32                  *)
  | I32_LOAD8_U of MemoryOffset        (* load 1 byte from linear memory and zero-extend to i32   *)
  | I32_STORE of MemoryOffset          (* store i32 sequentially in linear memory                 *)
  | I32_STORE8 of MemoryOffset         (* wrap i32 to byte and store in linear memory             *)
  // Constants (MicroC only supports 32-bit signed integer operands)
  | I32_CONST of int32                 (* 32-bit signed integer constant                          *)
  // Comparison operators
  | I32_EQ                             (* sign-agnostic compare equal                             *)
  | I32_EQZ                            (* compare equal to zero                                   *)
  | I32_NE                             (* sign-agnostic compare unequal                           *)
  | I32_LT_S                           (* signed less than                                        *)
  | I32_LT_U                           (* unsigned less than                                      *)
  | I32_GT_S                           (* signed greater than                                     *)
  | I32_GT_U                           (* unsigned greater than                                   *)
  | I32_LE_S                           (* signed less than or equal                               *)
  | I32_LE_U                           (* unsigned less than or equal                             *)
  | I32_GE_S                           (* signed greater than or equal                            *)
  | I32_GE_U                           (* unsigned greater than or equal                          *)
  // Numeric operators
  | I32_ADD                            (* sign-agnostic addition: x+y                             *)
  | I32_SUB                            (* sign-agnostic subtraction: x-y                          *)
  | I32_MUL                            (* sign-agnostic multiplication (lower 32-bits)            *)
  | I32_DIV_S                          (* signed division (result is truncated toward zero)       *)
  | I32_DIV_U                          (* unsigned division (result is floored)                   *)
  | I32_REM_S                          (* signed remainder (result has the sign of the dividend)  *)
  | I32_REM_U                          (* unsigned remainder (effectively modulo)                 *)
  | I32_AND                            (* sign-agnostic bitwise and                               *)
  | I32_OR                             (* sign-agnostic bitwise inclusive or                      *)
  | I32_XOR                            (* sign-agnostic bitwise exclusive or                      *)
  | I32_SHL                            (* sign-agnostic shift left                                *)
  | I32_SHR_U                          (* zero-replicating (logical) shift right                  *)
  | I32_SHR_S                          (* sign-replicating (arithmetic) shift right               *)
  | I32_ROTL                           (* sign-agnostic rotate left                               *)
  | I32_ROTR                           (* sign-agnostic rotate right                              *)
  | I32_CLZ                            (* sign-agnostic count leading zero bits                   *)
  | I32_CTZ                            (* sign-agnostic count trailing zero bits                  *)
  | I32_POPCNT                         (* sign-agnostic count number of one bits                  *)
  // NOTE: It is not possible to create function pointers in MicroC
  //| CALL_INDIRECT                      (* call function indirectly via a table                    *)
  // Conversions (no need to implement, not supported by MicroC)
  // Reinterpretations (no need to implement, not supported by MicroC)

let getOpCode = function
  // Language types
  | TYPE valTyp   -> getValueTypeCode valTyp
  // Control flow operators
  | UNREACHABLE   -> 0x00uy
  | NOP           -> 0x01uy
  | BLOCK _       -> 0x02uy
  | LOOP _        -> 0x03uy
  | IF _          -> 0x04uy
  | ELSE          -> 0x05uy
  | END           -> 0x0buy
  | BR _          -> 0x0cuy
  | BR_IF _       -> 0x0duy
  | RETURN        -> 0x0fuy
  // Call operators
  | CALL _        -> 0x10uy
  // Paremetric operators
  | DROP          -> 0x1auy
  | SELECT        -> 0x1buy
  // Variable access
  | GET_LOCAL _   -> 0x20uy
  | SET_LOCAL _   -> 0x21uy
  | TEE_LOCAL _   -> 0x22uy
  | GET_GLOBAL _  -> 0x23uy
  | SET_GLOBAL _  -> 0x24uy
  // Memory-related operators
  | I32_LOAD _    -> 0x28uy
  | I32_LOAD8_U _ -> 0x2duy
  | I32_STORE _   -> 0x36uy
  | I32_STORE8 _  -> 0x3auy
  // The distinct union of types between MicroC and WASM is a 32 bit integer.
  // Constants
  | I32_CONST _   -> 0x41uy
  // Comparison operators
  | I32_EQZ       -> 0x45uy
  | I32_EQ        -> 0x46uy
  | I32_NE        -> 0x47uy
  | I32_LT_S      -> 0x48uy
  | I32_LT_U      -> 0x49uy
  | I32_GT_S      -> 0x4auy
  | I32_GT_U      -> 0x4buy
  | I32_LE_S      -> 0x4cuy
  | I32_LE_U      -> 0x4duy
  | I32_GE_S      -> 0x4euy
  | I32_GE_U      -> 0x4fuy
  // Numeric operators
  | I32_CLZ       -> 0x67uy
  | I32_CTZ       -> 0x68uy
  | I32_POPCNT    -> 0x69uy
  | I32_ADD       -> 0x6auy
  | I32_SUB       -> 0x6buy
  | I32_MUL       -> 0x6cuy
  | I32_DIV_S     -> 0x6duy
  | I32_DIV_U     -> 0x6euy
  | I32_REM_S     -> 0x6fuy
  | I32_REM_U     -> 0x70uy
  | I32_AND       -> 0x71uy
  | I32_OR        -> 0x72uy
  | I32_XOR       -> 0x73uy
  | I32_SHL       -> 0x74uy
  | I32_SHR_S     -> 0x75uy
  | I32_SHR_U     -> 0x76uy
  | I32_ROTL      -> 0x77uy
  | I32_ROTR      -> 0x78uy

type Section =
  | TYPE
  | IMPORT
  | FUNCTION
  | TABLE
  | MEMORY
  | GLOBAL
  | EXPORT
  | START
  | ELEMENT
  | CODE
  | DATA

let getSectionCode = function
  | TYPE     -> 1uy
  | IMPORT   -> 2uy
  | FUNCTION -> 3uy
  | TABLE    -> 4uy
  | MEMORY   -> 5uy
  | GLOBAL   -> 6uy
  | EXPORT   -> 7uy
  | START    -> 8uy
  | ELEMENT  -> 9uy
  | CODE     -> 10uy
  | DATA     -> 11uy

open System
open System.Linq

let i2b (n : int) =
  List.ofSeq (BitConverter.GetBytes(n).Reverse())

let i2leb value =
  // integer to LEB128 varint as specced in http://dwarfstd.org/doc/Dwarf3.pdf
  // implementation inspired by: (MIT licensed project)
  // https://github.com/yageek/LEB128/blob/d05713742dd56bc2d1eaed44956f20f6755fa4e9/Sources/LEB128.swift#L79
  // translated to FP
  let rec inner bytes value more count =
    if not more then List.rev bytes else

    let b = byte (value &&& 0x7F)
    let value = value >>> 7

    let stop = (value = 0) && ((b >>> 6) = 0uy) || (value = -1 && (b >>> 6) = 1uy)
    let more = not stop
    let b = if not stop then b ||| 0x80uy else b

    let bytes = b :: bytes
    let count = count + 1
    inner bytes value more count
  inner [] value true 0

let emitbytes instr bytes =
  let opCode = getOpCode instr
  match instr with
  | BLOCK b | LOOP b | IF b    -> opCode :: getBlockTypeCode b :: bytes
  | BR rd | BR_IF rd           -> opCode :: rd :: bytes
  | CALL i
  | GET_LOCAL i
  | SET_LOCAL i
  | TEE_LOCAL i
  | GET_GLOBAL i
  | SET_GLOBAL i               -> opCode :: i2leb i @ bytes
  | I32_CONST n                -> opCode :: i2leb n @ bytes
  // mem store instructions also need alignment and offset immediates,
  // alignment is, however, always the same for 32-bit integer and single byte values
  | I32_STORE offset | I32_LOAD offset     -> opCode :: 2uy :: i2leb offset @ bytes
  | I32_STORE8 offset | I32_LOAD8_U offset -> opCode :: 0uy :: i2leb offset @ bytes
  // other immediate-free instructions
  | _                          -> opCode :: bytes
