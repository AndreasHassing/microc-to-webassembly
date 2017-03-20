(* File MicroC/WasmMachine.fs

   WasmMachine emits WebAssembly code in the form of
   binary stack machine code.

   Based on:
   Instructions and code emission for a stack-based
   abstract machine * sestoft@itu.dk 2009-09-23
 *)

module WasmMachine

type Label = string

type Instr =
  | Label of Label                     (* symbolic label; pseudo-instruc. *)
  | CSTI of int                        (* constant                        *)
  | ADD                                (* addition                        *)
  | SUB                                (* subtraction                     *)
  | MUL                                (* multiplication                  *)
  | DIV                                (* division                        *)
  | MOD                                (* modulus                         *)
  | EQ                                 (* equality: s[sp-1] == s[sp]      *)
  | LT                                 (* less than: s[sp-1] < s[sp]      *)
  | NOT                                (* logical negation:  s[sp] != 0   *)
  | DUP                                (* duplicate stack top             *)
  | SWAP                               (* swap s[sp-1] and s[sp]          *)
  | LDI                                (* get s[s[sp]]                    *)
  | STI                                (* set s[s[sp-1]]                  *)
  | GETBP                              (* get bp                          *)
  | GETSP                              (* get sp                          *)
  | INCSP of int                       (* increase stack top by m         *)
  | GOTO of Label                      (* go to label                     *)
  | IFZERO of Label                    (* go to label if s[sp] == 0       *)
  | IFNZRO of Label                    (* go to label if s[sp] != 0       *)
  | CALL of int * Label                (* move m args up 1, push pc, jump *)
  | TCALL of int * int * Label         (* move m args down n, jump        *)
  | RET of int                         (* pop m and return to s[sp]       *)
  | PRINTI                             (* print s[sp] as integer          *)
  | PRINTC                             (* print s[sp] as character        *)
  | LDARGS                             (* load command line args on stack *)
  | STOP                               (* halt the abstract machine       *)

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

(* These numeric instruction codes must agree with Machine.java: *)

let CODECSTI   = 0x41 // (i32.const)
let CODEADD    = 0x6a // (i32.add)
let CODESUB    = 0x6b // (i32.sub)
let CODEMUL    = 0x6c // (i32.mul)
let CODEDIV    = 0x6d // (i32.div_s) signed division
let CODEMOD    = 0x6f // (i32.rem_s) signed remainder (modulus)
let CODEEQ     = 0x46 // (i32.eq)
let CODELT     = 0x48 // (i32.lt_s) signed less than
let CODENOT    = 0x45 // (i32.eqz)
let CODEDUP    = 9 // not implemented in WASM
let CODESWAP   = 10 // not implemented in WASM
let CODELDI    = 11 // not implemented in WASM
let CODESTI    = 12 // not implemented in WASM
let CODEGETBP  = 13 // not used in WASM
let CODEGETSP  = 14 // not used in WASM
let CODEINCSP  = 15 // not used in WASM
let CODEGOTO   = 0x0c // (br)
let CODEIFZERO = 0x0d // (br_if)
let CODEIFNZRO = 0x0d // (br_if) - negated logic
let CODECALL   = 0x10 // (call)
let CODETCALL  = 20 // not implemented in WASM
let CODERET    = 0x0f // (return)
let CODEPRINTI = 22 // not implemented in WASM
let CODEPRINTC = 23 // not implemented in WASM
let CODELDARGS = 24 // not implemented in WASM
let CODESTOP   = 25 // not implemented in WASM

(* Bytecode emission, first pass: build environment that maps
   each label to an integer address in the bytecode.
 *)

let makelabenv (addr, labenv) instr =
  match instr with
  | Label lab      -> (addr, (lab, addr) :: labenv)
  | CSTI i         -> (addr+2, labenv)
  | ADD            -> (addr+1, labenv)
  | SUB            -> (addr+1, labenv)
  | MUL            -> (addr+1, labenv)
  | DIV            -> (addr+1, labenv)
  | MOD            -> (addr+1, labenv)
  | EQ             -> (addr+1, labenv)
  | LT             -> (addr+1, labenv)
  | NOT            -> (addr+1, labenv)
  | DUP            -> (addr+1, labenv)
  | SWAP           -> (addr+1, labenv)
  | LDI            -> (addr+1, labenv)
  | STI            -> (addr+1, labenv)
  | GETBP          -> (addr+1, labenv)
  | GETSP          -> (addr+1, labenv)
  | INCSP m        -> (addr+2, labenv)
  | GOTO lab       -> (addr+2, labenv)
  | IFZERO lab     -> (addr+2, labenv)
  | IFNZRO lab     -> (addr+2, labenv)
  | CALL(m,lab)    -> (addr+3, labenv)
  | TCALL(m,n,lab) -> (addr+4, labenv)
  | RET m          -> (addr+2, labenv)
  | PRINTI         -> (addr+1, labenv)
  | PRINTC         -> (addr+1, labenv)
  | LDARGS         -> (addr+1, labenv)
  | STOP           -> (addr+1, labenv)

(* Bytecode emission, second pass: output bytecode as integers *)

let rec emitints getlab instr ints =
  match instr with
  | Label lab      -> ints
  | CSTI i         -> CODECSTI   :: i :: ints
  | ADD            -> CODEADD    :: ints
  | SUB            -> CODESUB    :: ints
  | MUL            -> CODEMUL    :: ints
  | DIV            -> CODEDIV    :: ints
  | MOD            -> CODEMOD    :: ints
  | EQ             -> CODEEQ     :: ints
  | LT             -> CODELT     :: ints
  | NOT            -> CODENOT    :: ints
  | DUP            -> CODEDUP    :: ints
  | SWAP           -> CODESWAP   :: ints
  | LDI            -> CODELDI    :: ints
  | STI            -> CODESTI    :: ints
  | GETBP          -> CODEGETBP  :: ints
  | GETSP          -> CODEGETSP  :: ints
  | INCSP m        -> CODEINCSP  :: m :: ints
  | GOTO lab       -> CODEGOTO   :: getlab lab :: ints
  | IFZERO lab     -> CODEIFZERO :: getlab lab :: ints
  | IFNZRO lab     -> CODEIFNZRO :: getlab lab :: ints
  | CALL(m,lab)    -> CODECALL   :: m :: getlab lab :: ints
  | TCALL(m,n,lab) -> CODETCALL  :: m :: n :: getlab lab :: ints
  | RET m          -> CODERET    :: m :: ints
  | PRINTI         -> CODEPRINTI :: ints
  | PRINTC         -> CODEPRINTC :: ints
  | LDARGS         -> CODELDARGS :: ints
  | STOP           -> CODESTOP   :: ints

(* Convert instruction list to int list in two passes:
   Pass 1: build label environment
   Pass 2: output instructions using label environment
 *)

let code2ints (code : Instr list) : int list =
  let wasmBinaryMagicNumber = 0x0061736d
  let wasmBinaryVersionNumber = 0x01000000
  let wasmHeader = wasmBinaryMagicNumber :: [wasmBinaryVersionNumber]

  let (_, labenv) = List.fold makelabenv (0, []) code
  let getlab lab = lookup labenv lab
  List.foldBack (emitints getlab) code wasmHeader
