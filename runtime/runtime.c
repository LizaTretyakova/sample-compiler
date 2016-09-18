# include <stdio.h>

extern int read () {
  int d;
  printf ("> ");
  scanf ("%d", &d);
  return d;
}

extern void write (long int x) {
  printf ("%ld\n", x);
}
