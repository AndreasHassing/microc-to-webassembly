// micro-C example 17 -- conjecture: seq will terminate for any n > 0
// PASSES? - I can't remember the point of this example.
// It never terminates in the browser. Maybe it just takes a VERY long time to finish?
// I added `i` to reduce the amount of times it would check the verify the seq function
// in hopes that it would terminate faster - but it still didn't. I let it run for about 5 minutes.

void main() {
  int k;
  k = 0;

  int i;
  i = 0;
  while (i < 30) {
    k = k+1;
    if (seq(k) > 240)
      print k;
    i = i + 1;
  }
}

int seq(int i) {
  int count;
  count = 0;
  while (i != 1) {
    count = count + 1;
    if (i % 2 == 0)
      i = i / 2;
    else
      i = i * 3 + 1;
  }
  return count;
}

void start() {
  main();
}
