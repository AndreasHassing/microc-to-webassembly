void increment(int* x) {
	*x = *x + 1;
}

void main() {
	int x;
	x = 0;

	print x;
	println;

	increment(&x);
	print x;
	println;
	increment(&x);
	print x;
	println;
}

void start() {
	main();
}
