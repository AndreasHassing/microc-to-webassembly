# Bachelor Project
## Problem Definition
WebAssembly (WASM) is a new up-and-coming binary code format for the user-facing web, designed to work alongside JavaScript as a more portable, size- and load-time-efficient, ahead-of-time-compiled alternative.

The binary WASM format is not bound to be emitted by JavaScript (asm.js) only, potentially bringing your favorite language to the browser front-end. If a language can compile to a intermediate representation supported by WASM (LLVM IR, for instance), it can be compiled to the binary WASM format.

The purpose of this project is to shed some light on the unfinished WebAssembly specification. To assist in meeting this goal, a compiler will be designed, that compiles from a simple programming language, MicroC, to a WASM format that can be run in a WASM-enabled browser today (currently requiring Firefox Nightly or Chrome Canary with WASM flags set to enabled).

## Method
* Translation of MicroC to the binary WebAssembly format, using FsLexYacc (http://fsprojects.github.io/FsLexYacc/), as used in the Programs as Data course (fall 2016).
* Course books from Programs as Data (fall 2016) will be used as knowledge base for MicroC to WebAssembly translation:
  * Peter Sestoft: Programming Language Concepts. Springer 2012.
  * Torben Mogensen: Basics of Compiler Design. DIKU 2010. Chapters 2 and 3.
* Knowledge of WebAssembly will be gained through the official website (http://webassembly.org/) and links branching out from it.
* Creation of prototype: web interface that lets users type MicroC in a window, and see it generate WebAssembly in some human-readable format, either s-expression or linear bytecode. The prototype will also give users the ability to run their code as WebAssembly, given that the browser in use supports it.
* Reflection over, either the security behind WebAssembly or the portability of it between browsers and platforms.

If time allows:
* Extensions to the MicroC language will be implemented.
* Simple optimizations to the generated WebAssembly code.
