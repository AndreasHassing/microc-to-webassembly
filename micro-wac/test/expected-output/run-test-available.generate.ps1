# Remove list of current available tests
Remove-Item "run-test-available.js"

New-Item "run-test-available.js" -type file -value "const Available = {`n"

# Generate new list
Get-ChildItem -Filter *.wasm | ForEach-Object {
    Add-Content "run-test-available.js" "`t`"$($_.BaseName)`": `"$($_.Name)`","
}
Add-Content "run-test-available.js" "}"
