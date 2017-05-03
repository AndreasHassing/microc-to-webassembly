#!/bin/bash
lexerFile="CLex.fsl"
parserFile="CPar.fsy"

lexerHashFile=".lexer.compiled.hash"
parserHashFile=".parser.compiled.hash"

AlreadyCompiledLexerAndParser() {
    if [ ! -f $lexerHashFile ] || [ ! -f $parserHashFile ]; then
        # 0=true, 1=false
        return 1
    fi

    lexerHash=$(md5 < "${lexerFile}" | tr '[:lower:]' '[:upper:]')
    parserHash=$(md5 < "${parserFile}" | tr '[:lower:]' '[:upper:]')

    existingLexerHash=$(cat "${lexerHashFile}")
    existingParserHash=$(cat "${parserHashFile}")

    if [ $lexerHash = $existingLexerHash ] && [ $parserHash = $existingParserHash ]; then
        return 0
    else
        return 1
    fi
}

GenerateLexerAndParserHash() {
    md5 < "${lexerFile}" | tr '[:lower:]' '[:upper:]' > "${lexerHashFile}"
    md5 < "${parserFile}" | tr '[:lower:]' '[:upper:]' > "${parserHashFile}"
}

CompileLexerAndParser() {
    fslexyaccPath=$(echo mono ../packages/$(ls ../packages/ | grep "FsLexYacc.\d")/build/)
    fsyaccCmd="${fslexyaccPath}fsyacc.exe --module CPar CPar.fsy -o CPar.fs"
    fslexCmd="${fslexyaccPath}fslex.exe --unicode CLex.fsl -o CLex.fs"

    eval $fsyaccCmd
    eval $fslexCmd
}

if ! AlreadyCompiledLexerAndParser; then
    CompileLexerAndParser
    GenerateLexerAndParserHash
else
    echo "Parser and lexer is already compiled"
fi
