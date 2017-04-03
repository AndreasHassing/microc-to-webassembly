[<EntryPoint>]
let main argv =
    printfn "Hello there, compiling your stuff"
    printfn "%A" argv
    printfn "Done compiling your stuff LOL"

    // pause until user presses the enter key
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
