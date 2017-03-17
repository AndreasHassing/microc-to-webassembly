// JavaScript equivalent to `sum.wast`
function sumJs(a, b) {
	let sum = 0
	while (a != b) {
		sum = sum + a
		a += 1
	}
	return sum + a
}

async function getWasmSumFunc() {
	const binary = await fetch('sum.wasm')
	const bytes = await binary.arrayBuffer()
	const module = await WebAssembly.compile(bytes)
	const instance = await WebAssembly.instantiate(module)

	return instance.exports.sum
}

getWasmSumFunc().then((wasmSumFunc) => {
	runTests(sumJs, wasmSumFunc)
})

function runTests(jsSumFunc, wasmSumFunc) {
	// test iterations
	const ti = 50000000

	// sum from a to and including b
	let a = 1, b = 50

	let wasmTime = 0
	let jsTime = 0

	let testCounter = ti
	while (testCounter > 0) {
		let wasmTimeBefore = Date.now()
		wasmSumFunc(a, b)
		let wasmTimeAfter = Date.now()
		wasmTime += (wasmTimeAfter - wasmTimeBefore)

		let jsTimeBefore = Date.now()
		jsSumFunc(a, b)
		let jsTimeAfter = Date.now()
		jsTime += (jsTimeAfter - jsTimeBefore)

		testCounter--
	}

	console.log(`wasm time: ${wasmTime}`)
	console.log(`js time: ${jsTime}`)
}
