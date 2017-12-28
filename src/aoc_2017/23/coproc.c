#include <stdio.h>

/*
 * Mimicked assembly input file from day 23, in C code,
 * being that C is just one step up from assembly.
 *
 * Jump statments converted to for loops, but labels left in.
 *
 * Very inefficiently finds composite numbers.
 */

int main() {
  int a = 1; // set to 0 for 'debug'
  int b,c,d,e,f,g,h = 0;

  b = 84;
  c = 84;

  // Lines 5 - 8
non_debug:
  if (a != 0) {
    b = (b * 100) + 100000;
    c =  b + 17000;
  }

  // Loops starting at line 9
loop_1:
  for (; b <= c; b += 17) {
    f = 1;
loop_2:
    for (d = 2; d < b; d++) {
loop_3:
      for (e = 2; e < b; e++) {
        if (d * e == b) {
          f = 0;
        }
      } // end loop 3
    } // end loop 2

inc_h:
    if (f == 0) {
      h += 1;
    }
  } // end loop 1

  printf("h: %d\n", h);
  return h;
}
