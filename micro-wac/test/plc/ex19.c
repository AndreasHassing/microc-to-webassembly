// micro-C example 19 -- clumsy code for if in the forwards compiler
// PASSES

void main(int x) {
  if (x == 0)
    print 33;
  else
    print 44;
}

void start() {
	main(0);
}
