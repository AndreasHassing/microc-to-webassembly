// The start function is special, in WebAssembly
// it is the entry point of the module (and is optional).
// It is run automatically when instantiated in a browser,
// takes no arguments and does not return anything.
void start()
{
	print 42;
	println;
}
