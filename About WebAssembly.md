<details>
<summary><h2>Table of Contents</h2></summary>

* [WebAssembly](#webassembly-through-the-looking-glass)
  * [Key facts](#key-facts-about-webassembly)
    * [What is in the MVP](#what-is-in-the-mvp)
    * [Modules](#modules)
      * [Dynamic Linking](#dynamic-linking)
  * [References](#references)
* [Decisions](#decisions)
</details>

# WebAssembly Through the Looking-Glass
> *"In another moment Alice was through the glass, and had jumped lightly down into the Looking-glass room."*

## Key facts about WebAssembly
Following is a list of key information about WebAssembly, obtained via [WebAssembly.org](http://webassembly.org/).

* A [MVP](#what-is-in-the-mvp) (minimum viable product) will be released around Q1 2017, after the Browser Period, unless there are significant findings.
* Between the browser period and public launch, breaking changes will be introduced. The [WebAssembly Community Group](https://w3.org/community/webassembly/) (CG) will announce them ahead of time.
* [Proper tail calls will eventually be supported, but not in the MVP](https://github.com/WebAssembly/design/issues/189#issuecomment-113123641), this is particularly interesting when considering which languages to implement WebAssembly compilers for first. It makes less sense to implement a functional language with the MVP, as tail calls are an extremely important feature in any compiler for a functional language, and the alternative (trampoline functions) is not inherently efficient<sup>[1](#references)</sup>. This arguably paves the way for imperative languages as a start.
* A standard on how to import WebAssembly via HTML is [ongoing work](https://github.com/whatwg/loader/blob/master/roadmap.md), as it will [require an extension to the HTML spec](https://github.com/WebAssembly/design/blob/master/Modules.md#integration-with-es6-modules), to say when a script is parsed as a module instead of normal global code in `<script>` tags.
* [*Local variables have value types and are initialized to the appropriate zero value for their type (0 for integers, +0. for floating-point).*](https://github.com/WebAssembly/design/blob/master/Semantics.md#local-variables)


## What is in the MVP
There are goals for the WebAssembly project that the CG *know* that they want and need, but have to wait until after the MVP.

The MVP<sup>2</sup> is broken up into separate subjects:

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


## References
1. Technical Overview of the Common Language Runtime, section 8.1 - Tailcalls ([PDF](http://www.math.uaa.alaska.edu/~afkjm/csce331/handouts/clr.pdf)), authors: Erik Meijer (Microsoft) and John Gough (QUT).
2. https://github.com/WebAssembly/design/blob/master/MVP.md
3. https://github.com/WebAssembly/design/blob/master/Modules.md


# Decisions
> Decisions made along the way towards the final project.

## Implementation of compiler
* Use FsLexYacc, as it is pre-known, and used in the course from last semester.
* Compile MicroC to either **just** s-expressions, or all the way to binary. If you stick with s-expressions, use WABT to compile from s-expressions to wasm binary.

## Implementation of web-part
> Frontend: lets users type MicroC, and sends compilation to the server back-end.<br>Webserver: invokes the compiler, and returns the output to the front-end.

* Use Node.js for the server, that way all the www-facing content will be written in a single language.
* Use Vue.js or jQuery as a front-end framework, as I know these two, and they improve the readability and extensibility of javascript applications.
* Use bootstrap for styling, I know it already, and there is no need to reinvent the wheel here.

### Examples of code
* JavaScript, that runs in Node.js, and prints the output of `ls -lh /usr`. This sort of process spawning with output can work for the compiler:
  ```javascript
  const spawn = require('child_process').spawn
  const ls = spawn('ls', ['-lh', '/usr'])

  ls.stdout.on('data', (data) => {
    console.log(`stdout: ${data}`)
  })

  ls.stderr.on('data', (data) => {
    console.log(`stderr: ${data}`)
  })

  ls.on('close', (code) => {
    console.log(`child process exited with code ${code}`)
  })
  ```