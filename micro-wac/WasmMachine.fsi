module MicroWac.WasmMachine

//#region Types
/// Index, as used in a WASM binary to select functions.
type Index = int

/// RelativeDepth, as used in a WASM binary to break to a specified depth.
type RelativeDepth = byte

/// ValueType, as used in a WASM binary to describe what type a value has,
/// function, int, etc.
type ValueType =
  | I32
  | AnyFunc
  | Func

/// BlockType, as used in a WASM binary to describe the type of a block.
type BlockType =
  | BReturn of ValueType (* single result *)
  | BVoid                (* 0 results     *)

/// Instruction, a named operation in a WASM module.
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

/// Section, as used in a WASM binary to describe the type of a section.
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
//#endregion

//#region Functions
/// Get the function index of the print function.
val printFunctionIndex : Index

/// Get the function index of the println function.
val printLnFunctionIndex : Index

/// Get the binary type code of a ValueType as a byte.
val getValueTypeCode: v:ValueType -> byte

/// Get the binary type code of a BlockType as a byte.
val getBlockTypeCode: BlockType -> byte

/// Get the binary instruction (or operation) code as a byte.
val getOpCode: Instruction -> byte

/// Get the binary section code of a Section type as a byte.
val getSectionCode: Section -> byte

/// Convert a 32 bit integer to a byte list.
val i2b: int -> byte list

/// Convert a 32 bit integer to a byte list, and remove prefixed (padded) zeroes.
val i2bNoPad: int -> byte list

/// Conversion folder, turns an instruction into a byte lists
/// and prepends the result onto the 2nd parameter byte list.
val emitbytes: Instruction -> byte list -> byte list
//#endregion