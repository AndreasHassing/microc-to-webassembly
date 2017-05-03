$lexerFile = "CLex.fsl"
$parserFile = "CPar.fsy"

$lexerHashFile = ".lexer.compiled.hash"
$parserHashFile = ".parser.compiled.hash"

function GetFileHash($fileName) {
	return git hash-object $fileName
}

function AlreadyCompiledLexerAndParser {
	if (-not (Test-Path $lexerHashFile) -or -not (Test-Path $parserHashFile)) {
		return $false
	}

	$lexerHash = GetFileHash $lexerFile
	$parserHash = GetFileHash $parserFile

	$existingLexerHash = Get-Content $lexerHashFile
	$existingParserHash = Get-Content $parserHashFile

	return $lexerHash -eq $existingLexerHash -and $parserHash -eq $existingParserHash
}

function GenerateLexerAndParserHash {
	Set-Content $lexerHashFile -NoNewline (GetFileHash $lexerFile)
	Set-Content $parserHashFile -NoNewline (GetFileHash $parserFile)
}

function CompileLexerAndParser {
	$fslexyaccPath = "../packages/" + (Get-ChildItem ../packages/ "FsLexYacc.*" -Directory | Where-Object {$_.Name -match "FsLexYacc.\d"}).Name + "/build/"
	$fsyaccCmd = $fslexyaccPath + "fsyacc.exe --module CPar CPar.fsy -o CPar.fs"
	$fslexCmd = $fslexyaccPath + "fslex.exe --unicode CLex.fsl -o CLex.fs"

	Invoke-Expression $fsyaccCmd
	Invoke-Expression $fslexCmd
}

if (-not (AlreadyCompiledLexerAndParser)) {
	CompileLexerAndParser
	GenerateLexerAndParserHash
} else {
	Write-Output "Parser and lexer is already compiled"
}
