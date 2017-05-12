// micro-C example 12 -- tail calls
// PASSES (BUT IS NOT OPTIMIZED)

int main(int n) {
  if (n)
    return main(n-1);
  else
    return 17;
}

void start() {
	print main(5);
}
