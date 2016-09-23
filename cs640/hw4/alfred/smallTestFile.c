#include <stdio.h>
#include <math.h>
#include <stdlib.h> 
#include <string.h>
#include "alfred_types.h"
#include "sim.h"

int main(void) {
  state = init_state(2);
  state[0].re = 1;
  print_state();

  cplx* H = calloc(4, sizeof(cplx));
  H[0].re = 1/sqrt(2);
  H[1].re = 1/sqrt(2);
  H[2].re = 1/sqrt(2);
  H[3].re = -1/sqrt(2);
  applygate(2, 0, 0, 2, H, state);
  print_state();

  cplx* X = calloc(4, sizeof(cplx));
  X[0].re = 0;
  X[1].re = 1;
  X[2].re = 1;
  X[3].re = 0;
  applygate(2, 0, 2, 1, X, state);
  print_state();

  measure_state(2, state);
  print_state();

  return 0;
}
