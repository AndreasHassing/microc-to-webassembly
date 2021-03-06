# Micro C to WebAssembly, Bachelor Project
> Bachelor Project at [the IT University of Copenhagen](https://www.itu.dk/), creating a compiler from [Micro C](https://www.itu.dk/people/sestoft/plc/) to [WebAssembly](http://webassembly.org) (the new web standard [~~in draft state~~](http://webassembly.org/roadmap/)).

See [AboutWebAssembly.md](./About%20WebAssembly.md) for more information regarding the current (Q1 2017) state of WebAssembly.

## Compiling MicroWac
* Run `nuget restore` with the root of this repository as your working directory (or current directory).
* Run `./micro-wac/pre-build-lexpar-comp.ps1` (or `./micro-wac/pre-build-lexpar-comp.sh` on *nix) in order to compile the lexer and parser (this step is also required if you're using VS2015 or later). The pre-build script will not do anything if you already have the latest compiled versions.
  * The script is **not** baked into the VS build process because of execution policies on PowerShell scripts.
* Run `msbuild micro-wac/microwac.fsproj` (or `xbuild /target:micro-wac` on *nix), or simply build the project from within VS. The executable compiler can be run with `./micro-wac/bin/Debug/microwac.exe` (or `mono micro-wac/bin/Debug/microwac.exe` on *nix).

## Compiling a MicroC program to WebAssembly with MicroWac
Compile the MicroC program with the `microc.exe` executable. Run the compiler without any arguments to get information on how to use it.

WebAssembly modules cannot be loaded through the `file://` protocol, therefore you need to supply a locally hosted web server in order to see the modules run.
I suggest using `npm install -g http-server` and using the `http-server` command in the directory of your HTML/WASM files.
If you dislike Node (or JS), or like Python better, you can also run a web server in the current directory with Python using the following command:
`python -m http.server` (Python3) or `python -m SimpleHTTPServer` (Python2).

Once the web server is running, simply access http://localhost:8080/ (or http://localhost:8000) and open the html file of the corresponding WASM sample.

## Repository Structure
* **[micro-wac](./micro-wac)**: contains the source code for the *Micro* C to *W*eb*A*ssembly *C*ompiler.
* **[wasm-playground](./wasm-playground)**: contains hand-crafted `.wast` samples, their compiled binaries and, for each, a tiny framework to run them in.
* **[web-backend](./web-backend)**: contains the source code for the back-end web server that exposes the compiler via a simple RESTful API. **This part of the project was discarded in favor of spending more time on the compiler itself. The code that was created is kept for historical reasons.**
