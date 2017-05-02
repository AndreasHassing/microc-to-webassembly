# Micro C to WebAssembly, Bachelor Project
> Bachelor Project at [the IT University of Copenhagen](https://www.itu.dk/), creating a compiler from [Micro C](https://www.itu.dk/people/sestoft/plc/) to [WebAssembly](http://webassembly.org) (the new web standard [~~in draft state~~](http://webassembly.org/roadmap/)).

See [AboutWebAssembly.md](./About%20WebAssembly.md) for more information regarding the current (Q1 2017) state of WebAssembly.

## Compiling MicroWac
* Run `nuget restore` with the root of this repository as your working directory (or current directory). I personally recommend PowerShell.
* Run `./micro-wac/pre-build-lexpar-comp.ps1` in order to compile the lexer and parser (this step is also required if you're using VS2015 or later). The pre-build script will not do anything if you already have the latest compiled versions.
* Run `xbuild -target:micro-wac`, or simply build the project from within VS. The executable compiler can be found in `./micro-wac/bin/Debug/microwac.exe`.

## Repository Structure
* **[micro-wac](./micro-wac)**: contains the source code for the *Micro* C to *W*eb*A*ssembly *C*ompiler.
* **[wasm-playground](./wasm-playground)**: contains hand-crafted `.wast` samples, their compiled binaries and a tiny framework to run them in.
* **[web-backend](./web-backend)**: contains the source code for the back-end web server that exposes the compiler via a simple RESTful API.
* **[web-frontend](./web-frontend)**: contains the source code for the front-end web application that consumes the back-end API.
  The application lets users write Micro C code in a textfield, compile it to WASM, see the generated s-expression and run the compiled binary.
