// micro-C example 16 -- more optimization needed
// PASSES (BUT IS NOT OPTIMIZED)

void main(int n) {
  if (n)
    { }
  else
    print 1111;
  print 2222;
}

void start() {
	main(1);
}
