$lexerFile = "CLex.fsl"
$parserFile = "CPar.fsy"

$lexerHashFile = ".lexer.compiled.hash"
$parserHashFile = ".parser.compiled.hash"

function AlreadyCompiledLexerAndParser {
    if (-not (Test-Path $lexerHashFile) -or -not (Test-Path $parserHashFile)) {
        return $false
    }

    $lexerHash = (Get-FileHash $lexerFile -Algorithm MD5).Hash
    $parserHash = (Get-FileHash $parserFile -Algorithm MD5).Hash

    $existingLexerHash = Get-Content $lexerHashFile
    $existingParserHash = Get-Content $parserHashFile

    return $lexerHash -eq $existingLexerHash -and $parserHash -eq $existingParserHash
}

function GenerateLexerAndParserHash {
    Set-Content $lexerHashFile (Get-FileHash $lexerFile -Algorithm MD5).Hash
    Set-Content $parserHashFile (Get-FileHash $parserFile -Algorithm MD5).Hash
}

function CompileLexerAndParser {
    $fslexyaccPath = "../packages/" + (Get-ChildItem ../packages/ "FsLexYacc.*" -Directory | Where-Object {$_.Name -match "FsLexYacc.\d"}).Name + "/build/"
    $fsyaccCmd = $fslexyaccPath + "fsyacc.exe --module CPar CPar.fsy -o CPar.fs"
    $fslexCmd = $fslexyaccPath + "fslex.exe --unicode CLex.fsl -o CLex.fs"

    Invoke-Expression $fsyaccCmd
    Invoke-Expression $fslexCmd
}

function main {
    if (-not (AlreadyCompiledLexerAndParser)) {
        CompileLexerAndParser
        GenerateLexerAndParserHash
    }
}

main
