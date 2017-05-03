// micro-C example 1
// PASSES

void main(int n)
{
	while (n > 0)
	{
		print n;
		n = n - 1;
	}
	println;
}

void start()
{
	main(5);
}
