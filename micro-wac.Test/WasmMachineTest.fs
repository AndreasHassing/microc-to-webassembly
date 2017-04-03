open NUnit.Framework
open FsUnit

open MicroWac.WasmMachine

module ``Wasm Machine Tests`` =
  open System
  open System.Text

  [<Test>]
  let ``When calling intToBytes 5, expect [0; 0; 0; 5]`` () =
    intToBytes 5 |> should equal [0; 0; 0; 5]

  [<Test>]
  let ``When calling intToBytes 512, expect [0; 0; 2; 0]`` () =
    intToBytes 512 |> should equal [0; 0; 2; 0]

  [<Test>]
  let ``When calling intToBytes of int maxvalue, expect [127, 255; 255; 255]`` () =
    intToBytes Int32.MaxValue |> should equal [0b01111111; 0xff; 0xff; 0xff]

  [<Test>]
  let ``When calling code2bytes with no code, expect only wasm header`` () =
    let ``\0`` = Char.MinValue.ToString() // using '\0' is not allowed in F#, and "\0" does not work
    let asmString = ``\0`` + "asm"

    let wasmMagicNumber = List.ofArray (Encoding.ASCII.GetBytes(asmString))
    let wasmVersion = 0x01000000
    code2bytes [] |> should equal (List.concat [wasmMagicNumber; intToBytes wasmVersion])

module ``Wasm Machine Auxiliary Functions Tests`` =
  [<Test>]
  let ``When nthBit with n=3 of 5, expect 0b101`` () =
    // 5 = 0b101, 0th and 2nd indexes are ones
    nthBit 0 5 |> should equal 1
    nthBit 1 5 |> should equal 0
    nthBit 2 5 |> should equal 1

  [<Test>]
  let ``nthBit bounds`` () =
    for i = 3 to 31 do
      nthBit i 5 |> should equal 0

    // at the 32nd bit it overflows,
    // and negative bits are not allowed
    let bounds = [-1; 32]

    for i in bounds do
      (fun () -> nthBit i 5 |> ignore)
      |> should throw typeof<System.Exception>

