#!/bin/bash
lexerFile="CLex.fsl"
parserFile="CPar.fsy"

lexerHashFile=".lexer.compiled.hash"
parserHashFile=".parser.compiled.hash"

GetFileHash() {
	echo -n $(git hash-object $1)
}

AlreadyCompiledLexerAndParser() {
	if [ ! -f $lexerHashFile ] || [ ! -f $parserHashFile ]; then
		# 0=true, 1=false
		return 1
	fi

	lexerHash=$(GetFileHash "$lexerFile")
	parserHash=$(GetFileHash "$parserFile")

	existingLexerHash=$(cat "${lexerHashFile}")
	existingParserHash=$(cat "${parserHashFile}")

	if [ $lexerHash = $existingLexerHash ] && [ $parserHash = $existingParserHash ]; then
		return 0
	else
		return 1
	fi
}

GenerateLexerAndParserHash() {
	GetFileHash $lexerFile > "${lexerHashFile}"
	GetFileHash $parserFile > "${parserHashFile}"
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
