// micro-C example 7 -- an infinite loop
// PASSES (but stalls the browser, obviously)
// To kill this example in Chrome: open your task
// manager and kill the Chrome thread that is using 100%
// of a single core.

void main()
{
	int i;
	i = 0;
	while (true)
	{
		i = i + 1;
	}
	print 999999;
}

void start()
{
	main();
}

/* Code generated by the new continuation-based compiler:

      LDARGS; CALL (0,L1); STOP;
  L1: CSTI 0; GETBP; CSTI 0; STI; INCSP -1;
  L2: GETBP; GETBP; LDI; CSTI 1; ADD; STI; INCSP -1; GOTO L2
*/
