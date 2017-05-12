// micro-C example 8 -- loop 20 million times
// PASSES

void main() {
  int i;
  i = 20000000;
  while (i) {
    i = i - 1;
  }
	print 999999;
}

void start() {
	main();
}
