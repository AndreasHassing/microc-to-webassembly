int globalInteger;

void changeGlobalInteger(int x)
{
	globalInteger = x;
}

export int returnGlobalInteger()
{
	return globalInteger;
}

void start() {
	changeGlobalInteger(42);
}
