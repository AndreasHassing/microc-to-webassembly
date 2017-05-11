/* Document Selectors */
let moduleSelect = document.getElementById("module");
let runStartButton = document.getElementById("runStart");

let functionNamesSelect = document.getElementById("functionNames");
let runFunctionButton = document.getElementById("runFunction");

/* Document helpers */
function clearSelectOptions(select) {
	for (let i = select.options.length; i >= 0; i--) {
		select.remove(i);
	}
}

function setDisableOnElements(disable, ...elements) {
	for (let element of elements) {
		element.disabled = disable;
	}
}

/* On Document Ready: Display available wasm modules */
document.addEventListener("DOMContentLoaded", function(event) {
	for (let moduleName in Available) {
		let option = document.createElement("option");
		option.text = moduleName;
		option.value = Available[moduleName];
		moduleSelect.add(option);
	}
});

/* WASM Functions */
let jsExports = {
	printi: i => console.log(i),
	printc: c => console.log(String.fromCharCode(c)),
};

let currentlyLoadedWasmModule = {};

async function fetchAndInstantiate(wasmFile) {
	let wasm = await fetch(wasmFile);
	let bytes = await wasm.arrayBuffer();
	let module = await WebAssembly.compile(bytes);
	let instance = await WebAssembly.instantiate(module, {
		imports: jsExports
	});
	return instance;
}

function extractExportedFunctions(wasmModule) {
	let exportedFunctionNames = [];
	for (var key in wasmModule.exports) {
		exportedFunctionNames.push(key);
	}
	return exportedFunctionNames;
}

/* Event Listeners */
function wasmEventHandler(alreadyLoaded, callback) {
	return e => {
		e.preventDefault(); // stop form submission
		let wasmModule = alreadyLoaded
			? currentlyLoadedWasmModule
			: fetchAndInstantiate(
					moduleSelect.options[moduleSelect.selectedIndex].value
				);
		wasmModule.then(module => {
			if (!alreadyLoaded) {
				// clear and disable 'function name select' and 'run function button'
				clearSelectOptions(functionNamesSelect, functionNamesSelect);
				setDisableOnElements(true, runFunctionButton, functionNamesSelect);

				let exportedFunctions = extractExportedFunctions(module);
				for (let name of exportedFunctions) {
					let option = document.createElement("option");
					option.value = option.text = name;
					functionNamesSelect.add(option);
				}

				if (exportedFunctions.length > 0)
					setDisableOnElements(false, runFunctionButton, functionNamesSelect);

				currentlyLoadedWasmModule = wasmModule;
			}

			if (callback) callback(module);
		});
	};
}

runStartButton.addEventListener("click", wasmEventHandler(false));

runFunctionButton.addEventListener(
	"click",
	wasmEventHandler(true, module => {
		let functionName =
			functionNamesSelect.options[functionNamesSelect.selectedIndex].value;
		console.log(
			`calling %c${functionName}%c, returned:`,
			"color:blue;",
			"",
			module.exports[functionName]()
		);
	})
);
