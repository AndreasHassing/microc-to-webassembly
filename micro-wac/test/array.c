void setToOne(int* x) {
	*x = 1;
}

void start() {
	int arr[3];

	arr[0] = 5;
	print arr[0];
	println;

	arr[1] = 42;
	print arr[1];
	println;

	setToOne(&arr[2]);
	print arr[2];
}
