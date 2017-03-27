void increment(int* x) {
	*x = *x + 1;
}

void main() {
	int x;
	x = 0;

	increment(&x);
	increment(&x);
}
