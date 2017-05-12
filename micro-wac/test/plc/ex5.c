// micro-C example 5 -- return a result via a pointer argument; nested blocks
// PASSES

void main(int n) {
  int r;
  r = n;
  {
    int r;
    square(n, &r);
    print r;
  }
  print r;
}

void square(int i, int *rp) {
  *rp = i * i;
}

void start() {
	main(5);
}
