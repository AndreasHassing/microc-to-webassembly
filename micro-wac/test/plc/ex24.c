// micro-C example 24
// Global and local variables; function call in expression
// PASSES (number of the beast, how positively evil!)

int x;

void main() {
  x = 111;
  print 222 + g(333);
}

int g(int y) {
  return x + y;
}

void start() {
	main();
}
