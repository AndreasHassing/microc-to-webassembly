#!/bin/bash

# Remove list of current available tests
rm "run-test-available.js"

echo "const Available = {" > run-test-available.js

# Generate new list
for f in *.wasm; do
	emptystring=""
	name=$(echo ${f/.wasm/$emptystring})
	echo -e "\t\"$name\": \"$f\"," >> run-test-available.js
done

echo "}" >> run-test-available.js
