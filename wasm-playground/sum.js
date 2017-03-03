// global object for the sum function
let sum

fetch('sum.wasm').then(response =>
  response.arrayBuffer()
).then(buffer =>
  WebAssembly.instantiate(buffer)
).then(({module, instance}) =>
  sum = instance.exports.sum
).then(() => runTests())

// JavaScript equivalent to `sum.wast`
function sumJs(x, n) {
  let sum = 0;
  while (n-- != 0) {
    sum = sum + x;
  }
  return sum;
}

function runTests() {
  // test iterations
  let ti = 3
  // number to exponentiate
  let x = 4
  // exponent
  let n = 1000000000

  // ti = test iterations
  while (ti > 0) {
    console.time('wasm')
    sum(x, n)
    console.timeEnd('wasm')

    console.time('js')
    sumJs(x, n)
    console.timeEnd('js')

    ti--
  }
}
