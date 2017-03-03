void main() {
  if (11 < 22) {
    print 33;
  } // => CSTI 33; PRINTI;

  if (11 == 11) {
    print 11;
  } // => CSTI 11; PRINTI;

  if (11 == 22) {
    print -1;
  } // => <optimized away>

  if (11 != 11) {
    print 11;
  } // => <optimized away>
  else {
    print 12;
  } // => CSTI 12; PRINTI;

  if (11 != 22) {
    print 11;
  } // => CSTI 11; PRINTI;

  if (11 <= 22) {
    print 33;
  } // => CSTI 33; PRINTI;

  if (15 >= 7) {
      print 22;
  } // => CSTI 22; PRINTI;
  else {
      // this is does not get optimized away,
      // but is always skipped with a goto, or
      // a return now that the if-block is
      // true.
      // Thus else blocks are not optimized away, yet.
      print -1;
  } // => CSTI -1; PRINTI; // see comment inside else-block

  print 0;
}
