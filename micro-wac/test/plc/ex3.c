// micro-C example 3
// PASSES

void main(int n)
{
	int i;
	i = 0;
	while (i < n)
	{
		print i;
		i = i + 1;
	}
}

void start()
{
	main(5);
}
