# WebAssembly Playground
In this directory you can find a few hand-crafted WebAssembly modules and corresponding HTML pages to expose them with.

* dom_access: Attempt to give WebAssembly access to the DOM. Pretty cool, but not being able to ID with anything but integers makes it quite hard to use.

  Strings can be stored in memory, and used to ID elements with, but I didn't take the example that far.
* hello_wasm: First attempt at writing WebAssembly by hand (WAST) and using imports and exports.
* lin-mem: Sandbox to play around with how the linear memory model works.
* sum: An attempt to measure performance of a WebAssembly module vs a native JavaScript function in the browser.
