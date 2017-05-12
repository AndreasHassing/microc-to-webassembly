void start() {
	int x;

	{
		int x;
		x = 5;
		print x;
		println;
	}

	{
		int x;
		print x; // should print 0, prints 5 with original MicroC compiler
		println;
		x = 3;
		print x;
		println;
	}

	{
		int y;
		y = 42;
		print y;
		println;
	}

	print x; // should print 0
	println;

	x = 7;
	print x;
}
