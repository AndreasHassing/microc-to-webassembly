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
    # remember to change the version number if fslexyacc is updated in this project
    ../packages/FsLexYacc.7.0.5/build/fsyacc.exe --module CPar CPar.fsy -o CPar.fs
    ../packages/FsLexYacc.7.0.5/build/fslex.exe --unicode CLex.fsl -o CLex.fs
}

function main {
    $alreadyCompiled = AlreadyCompiledLexerAndParser
    if (-not $alreadyCompiled) {
        CompileLexerAndParser
        GenerateLexerAndParserHash
    }
}

main
