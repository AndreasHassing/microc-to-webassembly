<details>
<summary><h2>Table of Contents</h2></summary>

* [WebAssembly](#webassembly-through-the-looking-glass)
  * [Key facts](#key-facts-about-webassembly)
    * [What is in the MVP](#what-is-in-the-mvp)
    * [Modules](#modules)
      * [Dynamic Linking](#dynamic-linking)
  * [Stories](#stories)
  * [References](#references)
* [Decisions](#decisions)
* [Informative Links](#informative-links)
</details>

# WebAssembly Through the Looking-Glass
> *"In another moment Alice was through the glass, and had jumped lightly down into the Looking-glass room."*

Also contains stories and experiences that may or may not be related to WebAssembly, but may be useful to me for the final project report.

## Key facts about WebAssembly
Following is a list of key information about WebAssembly, obtained via [WebAssembly.org](http://webassembly.org/).

* A [MVP](#what-is-in-the-mvp) (minimum viable product) will be released around Q1 2017, after the Browser Period, unless there are significant findings.
* Between the browser period and public launch, breaking changes will be introduced. The [WebAssembly Community Group](https://w3.org/community/webassembly/) (CG) will announce them ahead of time.
* [Proper tail calls will eventually be supported, but not in the MVP](https://github.com/WebAssembly/design/issues/189#issuecomment-113123641), this is particularly interesting when considering which languages to implement WebAssembly compilers for first. It makes less sense to implement a functional language with the MVP, as tail calls are an extremely important feature in any compiler for a functional language, and the alternative (trampoline functions) is not inherently efficient<sup>[1](#references)</sup>. This arguably paves the way for imperative languages as a start.
* A standard on how to import WebAssembly via HTML is [ongoing work](https://github.com/whatwg/loader/blob/master/roadmap.md), as it will [require an extension to the HTML spec](https://github.com/WebAssembly/design/blob/master/Modules.md#integration-with-es6-modules), to say when a script is parsed as a module instead of normal global code in `<script>` tags.
* [*Local variables have value types and are initialized to the appropriate zero value for their type (0 for integers, +0. for floating-point).*](https://github.com/WebAssembly/design/blob/master/Semantics.md#local-variables)
* "Even though WebAssembly is specified in terms of a stack machine, that's not how it works on the physical machine. When the browser translates WebAssembly to the machine code for the machine the browser is running on, it will use registers. Since the WebAssembly code doesn't specify registers, it gives the browser more flexibility to use the best register allocation for that machine." - https://hacks.mozilla.org/2017/02/creating-and-working-with-webassembly-modules/
* A WebAssembly page has a constant size of 65,536 bytes, i.e., 64KiB. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_objects/WebAssembly/Memory#Parameters
* WebAssembly modules can act both as libraries (think .dll's) and as executables (think .exe's). Libraries in: what the module exposes, and executables in: which function it starts when fully loaded.
* Module section requirements (in order) - also gracefully ripped from https://hacks.mozilla.org/2017/02/creating-and-working-with-webassembly-modules

  Required:
  1. **Type**. Contains the function signatures for functions defined in this module and any imported functions.
  1. **Function**. Gives an index to each function defined in this module.
  1. **Code**. The actual function bodies for each function in this module.

  Optional:
  1. **Export**. Makes functions, memories, tables, and globals available to other WebAssembly modules and JavaScript. This allows separately-compiled modules to be dynamically linked together. This is WebAssembly’s version of a .dll.
  1. **Import**. Specifies functions, memories, tables, and globals to import from other WebAssembly modules or JavaScript.
  1. **Start**. A function that will automatically run when the WebAssembly module is loaded (basically like a main function).
  1. **Global**. Declares global variables for the module.
  1. **Memory**. Defines the memory this module will use.
  1. **Table**. Makes it possible to map to values outside of the WebAssembly module, such as JavaScript objects. This is especially useful for allowing indirect function calls.
  1. **Data**. Initializes imported or local memory.
  1. **Element**. Initializes an imported or local table.


## What is in the MVP
There are goals for the WebAssembly project that the CG *know* that they want and need, but have to wait until after the MVP.

The MVP<sup>[2](#references)</sup> is broken up into separate subjects:

* Module: the distributable, loadable and executable unit of code (`.wasm`).
* Behavior of code in a module, as specified in terms of instructions for a structured stack machine.
* Binary format specification, which is a binary encoding of a module's structure and code.
* Text format specification, to be read and written using tools (e.g., assemblers, debuggers, profilers), as a textual version of a module's structure and code.
* WebAssembly is designed to be implemented by WebBrowsers and other execution environments (OS's, hardware, etc.)

## Modules
A module in WebAssembly<sup>[3](#references)</sup> is a distributable, loadable, and executable unit of code. *"At runtime, a module can be instantiated with a set of import values to produce an instance, which is an immutable tuple referencing all the state accessible to the running module. Multiple module instances can access the same shared state which is the basis for dynamic linking."*.

### Dynamic linking
WebAssembly modules are dynamically linked prior to runtime, to allow injection of state and data into the WebAssembly code, such that the module can use functions outside of its own scope (example: dynamically link jQuery into some WebAssembly module, to give it DOM-handling superpowers).

### Imports

## Stories
* Used Firefox Nightly to run WASM files generated by WABT. Updated Firefox Nightly version today (2017-03-03), and everything broke. Turns out the CG incremented the version (commits: [spec](https://github.com/WebAssembly/spec/commit/002e57c86ccf5c80db0de6e40246665340350c43), [wabt](https://github.com/WebAssembly/wabt/commit/529015dc83b426659047689922eee7f9a34f9f15)) from `0x0d` to `1`, which required a re-clone of the WABT repository and a fresh build with `cmake` and `make`.
* Running a function from a WebAssembly module in JavaScript seems to have a significant overhead. For instance, running a simple sum function written in WASM within a loop from JavaScript, versus running the same loop with the sum function from JS, seemingly takes double as long. But running a function from WASM that does a lot of work, vs running the same function in JavaScript yields better results for WebAssembly. I may be able to conclude that WebAssembly code runs faster than JavaScript, but making many calls to small functions from a WebAssembly module will most likely be slower than implementing them in JavaScript. With this conclusion I can also deduce that WebAssembly should not be used to create web frameworks that get called from the JavaScript environment, as the performance will be terrible (unless of course WebAssembly can access the DOM directly somehow). WebAssembly will be great for programs that have a `main` function, and get called once, and then perform all the necessary computations, or programs that contain high computing complexity functions (graph search algorithms, tree construction, sorting, etc.) that take a good chunk of time to run.

  This story has been verified by reading https://hacks.mozilla.org/2017/02/where-is-webassembly-now-and-whats-next/, which explains that the slowdown is caused by the JavaScript JIT trampolining WASM function calls, instead of direct calls. When browser vendors fix this (and properly implement the call procedure from JS to WASM) calling functions will be fast (perhaps even just as fast as calling a JavaScript function from within JavaScript?).
* WebAssembly does not have the notion of base- and stack pointers, [reference](https://github.com/WebAssembly/design/issues/338).

## References
1. Technical Overview of the Common Language Runtime, section 8.1 - Tailcalls ([PDF](http://www.math.uaa.alaska.edu/~afkjm/csce331/handouts/clr.pdf)), authors: Erik Meijer (Microsoft) and John Gough (QUT).
2. https://github.com/WebAssembly/design/blob/master/MVP.md
3. https://github.com/WebAssembly/design/blob/master/Modules.md


# Decisions
> Decisions made along the way towards finalizing the bachelor project.

## Implementation of compiler
* Use the FsLexYacc compiler-compiler framework, as it is pre-known and was used in the "Programs as Data" course previous semester.
* Compile MicroC to either **just** s-expressions, or all the way to binary. If you stick with s-expressions, use WABT to compile from s-expressions to wasm binary.
  * I'm having a hard time figuring out of it's easier/better to compile to s-expression form (wast) or directly to binary (wasm). The main issue is that the current compiler framework is built to generate bytecode (binary), not to transpile to another language.
* Find a way to implement WebAssembly exports and imports in MicroC.
  * Implemented exports in [df40f41](https://github.com/AndreasHassing/microc-to-webassembly/commit/df40f41c934d45bee4e01e5a68604dc20e7b8037).
* Find a way to compile the `print` function embedded in MicroC to WebAssembly, think: import a console.log function per default?
  * This is actually not possible. A hack could be to force users to inject a console.log-like function into modules compiled with Micro-WAC, but that seems unnecessary. It makes more sense to explain how this is a "limitation" in the current WebAssembly code format.

## Implementation of web-part
> Frontend: lets users type MicroC, and sends compilation to the server back-end.<br>Webserver: invokes the compiler, and returns the output to the front-end.

* Use Node.js for the server, that way all the www-facing content will be written in a single language (JavaScript).
* Use Vue.js or jQuery as a front-end framework, as I know these two, and they improve the readability and extensibility of JavaScript applications.
* Use bootstrap for styling, I know it already, and there is no need to reinvent the wheel here.

# Informative Links
* [Advanced JS performance with V8 (Google Chrome JS engine) and Web Assembly (Chrome Dev Summit 2016)](https://www.youtube.com/watch?v=PvZdTZ1Nl5o)
* [How WebAssembly Will Change the Way You Write Javascript (Seth Samuel at EmpireNode)](https://www.youtube.com/watch?v=kq2HBddiyh0)
