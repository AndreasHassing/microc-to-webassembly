Get-ChildItem . -Filter "*.c" | ForEach-Object -Process { ..\..\bin\Debug\microwac.exe -html $_{Name}.c }
