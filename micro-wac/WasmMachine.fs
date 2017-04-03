(* File MicroC/WasmMachine.fs

   WasmMachine emits WebAssembly code in the form of
   binary stack machine code.
   abhn@itu.dk 2017-03-30

   Based on:
   Instructions and code emission for a stack-based
   abstract machine * sestoft@itu.dk 2009-09-23
 *)

module MicroWac.WasmMachine

type UInt8 = UInt8 of uint8
type UInt16 = UInt16 of uint16
type UInt32 = UInt32 of uint32

type Bit =
  | Bit of bool // hack to emulate a single bit
  override b.ToString() = let (Bit bitValue) = b
                          if bitValue then "1" else "0"

type Varuint1 = Varuint1 of Bit

// 7 bits unsigned goes from 0..+127
type Varuint7 =
  | Varuint7 of Bit * Bit * Bit * Bit * Bit * Bit * Bit
  override bits.ToString() =
    match bits with
    | Varuint7(a, b, c, d, e, f, g) ->
        a.ToString() + b.ToString() + c.ToString() +
        d.ToString() + e.ToString() + f.ToString() +
        g.ToString()

type Varuint32 =  Varuint32 of uint32

let nthBit n x =
  if n >= 32 || n < 0 then failwith "nthBit only works on integers"
  (x >>> n) &&& 1

let makeBit n = Bit (if n > 0 then true else false)
let makeVaruint7 n =
  if n > 0b1111111 || n < 0b0000000
  then failwith "Can only make varuint7 of a number within the range 0-127"
  // make bit for nth bit in x
  let mbf n x = nthBit n x |> makeBit
  Varuint7 (mbf 6 n, mbf 5 n, mbf 4 n, mbf 3 n, mbf 2 n, mbf 1 n, mbf 0 n)

// 7 bits signed goes from -64..+63 (twos complement)
type Varint7 = Varint7 of int8
type Varint32 = Varint32 of int32
type Varint64 = Varint64 of int64

let makeVarint7 n =
  if n > 63 || n < -64
  then failwith "Can only make varint7 of a number within the range (-63..+64)"
  Varint7(int8(n))

type Index = I of int

type RelativeDepth = RD of int

type ValueType =
  | I32
  | AnyFunc
  | Func

type BlockType =
  | ValueType of ValueType             (* single result *)
  | ZeroResults                        (* 0 results     *)

// The function type
type FuncType = FT of Varint7 * Varuint32 * ValueType list option * Varuint1 * ValueType option

let getBlockTypeCode = function
  | ValueType t -> match t with
                   | I32     -> 0x7f
                   | AnyFunc -> 0x70
                   | Func    -> 0x60
  | ZeroResults -> 0x40

type Instruction =
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
  | BR_TABLE                           (* NOT IMPLEMENTED DUE TO TIME CONSTRAINTS                 *)
  | RETURN                             (* return zero or one value from this function             *)
  // Call operators
  | CALL of Index                      (* call function by its index                              *)
  | CALL_INDIRECT                      (* NOT IMPLEMENTED DUE TO TIME CONSTRAINTS                 *)
  // Paremetric operators
  | DROP                               (* ignore value                                            *)
  | SELECT                             (* select one of two values based on condition             *)
  // Variable access
  | GET_LOCAL of Index                 (* read the current value of a local variable              *)
  | SET_LOCAL of Index                 (* set the current value of a local variable               *)
  | TEE_LOCAL of Index                 (* set the current value of a local variable and return it *)
  | GET_GLOBAL of Index                (* get the current value of a global variable              *)
  | SET_GLOBAL of Index                (* set the current value of a global variable              *)
  // Constants (MicroC only supports 32-bit signed integer operators)
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
  // Memory-related operators -- NOT IMPLEMENTED DUE TO TIME CONSTRAINTS
  // Conversions (no need to implement, not supported by MicroC)
  // Reinterpretations (no need to implement, not supported by MicroC)

(* Generate new distinct labels *)

let (resetLabels, newLabel) =
  let lastlab = ref -1
  ((fun () -> lastlab := 0), (fun () -> (lastlab := 1 + !lastlab; "L" + (!lastlab).ToString())))

(* Simple environment operations *)

type 'data Env = (string * 'data) list

let rec lookup env x =
  match env with
  | []         -> failwith (x + " not found")
  | (y, v)::yr -> if x=y then v else lookup yr x

(* An instruction list is emitted in two phases:
   * pass 1 builds an environment labenv mapping labels to addresses
   * pass 2 emits the code to file, using the environment labenv to
     resolve labels
 *)

let getOpCode = function
  // Control flow operators
  | UNREACHABLE   -> 0x00
  | NOP           -> 0x01
  | BLOCK _       -> 0x02
  | LOOP _        -> 0x03
  | IF _          -> 0x04
  | ELSE          -> 0x05
  | END           -> 0x06
  | BR _          -> 0x0c
  | BR_IF _       -> 0x0d
  | BR_TABLE      -> 0x0e
  | RETURN        -> 0x0f
  // Call operators
  | CALL _        -> 0x10
  | CALL_INDIRECT -> 0x11
  // Paremetric operators
  | DROP          -> 0x1a
  | SELECT        -> 0x1b
  // Variable access
  | GET_LOCAL _   -> 0x20
  | SET_LOCAL _   -> 0x21
  | TEE_LOCAL _   -> 0x22
  | GET_GLOBAL _  -> 0x23
  | SET_GLOBAL _  -> 0x24
  // MicroC only supports 32-bit signed integer data, so only implement that
  // Constants
  | I32_CONST _   -> 0x41
  // Comparison operators
  | I32_EQZ       -> 0x45
  | I32_EQ        -> 0x46
  | I32_NE        -> 0x47
  | I32_LT_S      -> 0x48
  | I32_LT_U      -> 0x49
  | I32_GT_S      -> 0x4a
  | I32_GT_U      -> 0x4b
  | I32_LE_S      -> 0x4c
  | I32_LE_U      -> 0x4d
  | I32_GE_S      -> 0x4e
  | I32_GE_U      -> 0x4f
  // Numeric operators
  | I32_CLZ       -> 0x67
  | I32_CTZ       -> 0x68
  | I32_POPCNT    -> 0x69
  | I32_ADD       -> 0x6a
  | I32_SUB       -> 0x6b
  | I32_MUL       -> 0x6c
  | I32_DIV_S     -> 0x6d
  | I32_DIV_U     -> 0x6e
  | I32_REM_S     -> 0x6f
  | I32_REM_U     -> 0x70
  | I32_AND       -> 0x71
  | I32_OR        -> 0x72
  | I32_XOR       -> 0x73
  | I32_SHL       -> 0x74
  | I32_SHR_S     -> 0x75
  | I32_SHR_U     -> 0x76
  | I32_ROTL      -> 0x77
  | I32_ROTR      -> 0x78

(* Bytecode emission, first pass: build environment that maps
   each label to an integer address in the bytecode.
 *)


(* Bytecode emission, second pass: output bytecode as integers *)
open System
open System.Linq

/// Little-endian
let intToBytes (n : int) =
  List.ofSeq (BitConverter.GetBytes(n).Reverse())

let emitbytes instr bytes =
  let opCode = intToBytes <| getOpCode instr
  match instr with
  | BLOCK b | LOOP b | IF b    -> opCode :: intToBytes (getBlockTypeCode b) :: bytes
  | BR (RD rd) | BR_IF (RD rd) -> opCode :: intToBytes rd :: bytes
  | CALL (I i)
  | GET_LOCAL (I i)
  | SET_LOCAL (I i)
  | TEE_LOCAL (I i)
  | GET_GLOBAL (I i)
  | SET_GLOBAL (I i)           -> opCode :: intToBytes i :: bytes
  | I32_CONST n                -> opCode :: intToBytes n :: bytes
  // IMMEDIATE-FREE OPERATORS
  | _                          -> opCode :: bytes

let code2bytes code =
  let wasmBinaryMagicNumber = 0x0061736d
  let wasmBinaryVersionNumber = 0x01000000
  let wasmHeader = [intToBytes wasmBinaryMagicNumber; intToBytes wasmBinaryVersionNumber]

  let opSeparatedBytes = List.foldBack emitbytes code wasmHeader
  List.concat opSeparatedBytes
