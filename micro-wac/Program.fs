module MicroWac.Main

type CommandLineOptions = {
  cfile: string;
  verbose: bool;
  withHtml: bool;
  error: bool;
}

let printHelpMsg () =
  printfn @"To compile: wasmcomp.exe [-v] [-html] <microc-file>.c"

/// Parses command line arguments into a CommandLineOptions record.
/// Inspired by https://fsharpforfunandprofit.com/posts/pattern-matching-command-line/
let parseCommandLine args =
  let rec parseArgs optionsSoFar = function
    | []            -> optionsSoFar
    | "-v" :: xs    -> let newOpts = { optionsSoFar with verbose = true }
                       parseArgs newOpts xs
    | "-html" :: xs -> let newOpts = { optionsSoFar with withHtml = true }
                       parseArgs newOpts xs
    | cfile :: []   -> { optionsSoFar with cfile = cfile }
    | _             -> printHelpMsg (); { optionsSoFar with error = true }
  parseArgs { cfile = ""; verbose = false; withHtml = false; error = false; } args

open System.IO
let renameExtension filename (toExt: string) =
  let filenameWithoutExtension = Path.GetFileNameWithoutExtension filename
  if toExt.[0] = '.'
  then filenameWithoutExtension + toExt
  else filenameWithoutExtension + "." + toExt

[<EntryPoint>]
let main argv =
  let opts = parseCommandLine (List.ofArray argv)

  if opts.error || opts.cfile = "" then printHelpMsg (); 1 else

  if opts.verbose then printfn "Compiling %s to WebAssembly Binary" opts.cfile

  let programAsAST = Parse.fromFile opts.cfile
  let wasmFilename = renameExtension opts.cfile "wasm"
  Wasmcomp.compileToFile wasmFilename programAsAST opts.withHtml

  if opts.verbose then printfn "Finished compiling to %s" wasmFilename
  0 // return an integer exit code; 0 means A-OK
