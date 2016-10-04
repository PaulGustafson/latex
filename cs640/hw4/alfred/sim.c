#include <stdio.h>
#include <time.h>
#include <math.h>
#include <stdlib.h> 
#include <string.h>
#include "alfred_types.h"
#include "sim.h"

/* implicit parameters, manipulated by the routines below */
int Nbits =  -1; /* number of qubits used */
cplx *state = 0; /* state vector */ 
double thres = 0.001; /* numbers 0<=fabs(r)<thres are considered tiny */

#define istiny(x) ( (fabs((x))<thres) ? 1: 0 )

#define sq(x) ( (x)*(x) ) 

/* macros for complex number arithmetic */ 
#define add(a, b) ( (cplx) {(a).re+(b).re, (a).im+(b).im } )

#define mult(a, b) ((cplx) {(a).re*(b).re-(a).im*(b).im, \
  (a).re*(b).im+(a).im*(b).re }) 

/* manipulate state size */
cplx *init_state(int bits) {
  Nbits = bits;
  return calloc(1<<bits, sizeof(cplx));
}

cplx *resize_state(int bits, cplx *st) { 
  /* enlarge or reduce to *bits* qubits */ 
  int i;
  if(bits <= Nbits) { 
    st = realloc(st, (1<<bits)*sizeof(cplx)); 
  } else { 
    st = realloc(st, (1<<bits)*sizeof(cplx));
    for(i=1<<Nbits; i < (1<<bits); i++)
      st[i] = (cplx){ 0.0, 0.0};
  }
  Nbits = bits;
  return st;
}

/* manipulation of state content */
void clear_state() { 
  memset(state, 0, (1<<Nbits)*sizeof(cplx));
} 

void normalize_state() { 
  double len=0; 
  int i;
  for(i=0; i<(1<<Nbits); i++) 
    len += sq(state[i].re)+sq(state[i].im);
  len = 1.0/sqrt(len);  
  for(i=0; i<(1<<Nbits); i++) {
    state[i].re *= len;
    state[i].im *= len;
  }  
}

/* input: a single bit set at the position of the gate; 
   this bitposition is measured and the state vector  
   is changed according to the postulates of quantum 
   mechanics. 
   NOTE: the state must be normalized to length 1 */
void measure_state(int pos, cplx *state) { 
  int i;  
  double ran; 
  double prob; 
  
  srand(clock());
  ran = ((double)rand())/RAND_MAX;
  /* complete the missing code */ 

  /* compute the probability to observe 0,
   * determine whether 0 or 1 should be observed in this measurement
   * then modify state to produce the post measurement state 
   * of the quantum computer
   */ 

  // the probability of getting a zero in the ith position
  prob = 0;

  // calculate prob by adding up the squared moduli of coefficients
  // with 0 in the position "pos"
  for (i = 0; i < (1 << Nbits); i++) {
    if (((i / pos) % 2) == 0) {
      prob += sq(state[i].re)+sq(state[i].im);
    }
  }

  // do the measurement
  int measuredBit;

  if(ran < prob) 
    measuredBit = 0;
  else
    measuredBit = 1;
  
  // Apply the projection operator
  for (i = 0; i < (1 << Nbits); i++) {
    if (((i / pos) % 2) != measuredBit) {
      state[i].re = 0;
      state[i].im = 0;
    }
  }


  normalize_state();  
}


void set_bit(int pos, cplx *state) {
  int i=0;
  int limit = 1<<Nbits;

  for(i = 0; (state[i].re == 0.0) && (state[i].im == 0.0) ; i++);
  /* state[i] is now the first nonzero entry */

  if( !(i & pos) ) { /* bit is not set */
    for(  ; i< limit; i++) { /* swap entries */
      if( i & pos) { continue; } /* skip this index */ 
      state[ i|pos ].re = state[i].re;
      state[ i|pos ].im = state[i].im;
      state[i].re = 0.0;
      state[i].im = 0.0;  
    }
  } 
  print_state();
}

void erase_bit(int pos, cplx *state) {
  int i=0;
  int limit = 1<<Nbits;

  for(i = 0; (state[i].re == 0.0) && (state[i].im == 0.0) ; i++);
  /* state[i] is now the first nonzero entry */
 
  if( i & pos ) { /* bit is set */
    for(  ; i< limit; i++) { /* swap entries */
      if( i & pos ) { 
	state[i-pos].re = state[i].re;
	state[i-pos].im = state[i].im;
	state[i].re = 0.0;
	state[i].im = 0.0;
      }
    }
  } 
}

/* print a single ket in binary format */
void print_label(int d) { 
  int i;
  printf("|");
  for(i=Nbits-1; i>=0; i--) 
    printf((d & 1<<i) ? "1": "0"); 
  printf(")");
}

/* print the complete state, subpressing 
   tiny numbers */ 

/* sign of a float with exception for the 
   first occurence, where the + sign is 
   supressed */ 
#define sign(x) ( ((x)<0) ? "-" : \
   (first) ? "" : "+" ) 

void print_state(void) {
int i;
int first = 1;
double re, im;
  for(i=0; i< (1<<Nbits); i++) { 
    re = state[i].re; im = state[i].im;
    switch( (istiny(re)<<1 | istiny(im)) ) { 
    case 0: /* print re and im */ 
      /* printf("%s%1.3g", sign(re),fabs(re));
      printf("%si*%.3g",sign(im),fabs(im));
      */ 
      if( !first ) { 
	printf("+");
      };
      printf("(%1.3g%si%.3g)",re,sign(im),fabs(im));
      print_label(i);
      first = 0;
      break;
    case 1: /* print re */
      printf("%s%0.3g",sign(re),fabs(re)); 
      print_label(i);
      first = 0;
      break;
    case 2: /* print im */ 
      printf("%si*%1.3g",sign(im),fabs(im)); 
      print_label(i);
      first = 0;
      break;
    case 3: /* print nothing */  
      ;
    }
  }
  printf("\n");
}

/* convert the label of a ket in binary format into
   an integer d, the address of the state */
int convert_label(char *str) { 
int i, d=0;
  if(strlen(str) != Nbits) { 
     fprintf(stderr,"length of qubit label is wrong\n");
     exit(1);
  } else { 
    for(i=0; i<Nbits; i++) { 
      if (str[Nbits-1-i]=='1') { d += (1<<i); } 
    }
  }
  return d;
}


/* Applying a conditioned g-gate to a state vector */ 
/* ASSUME: ocnd, icnd, and cpos have disjoint support */
void applygate(int Nbits, int ocnd, int icnd, int gpos, 	       
               mat g, cplx *state  ) { 
  cplx tmp0, tmp1; 
  int i;
  int acnd = ( ocnd | icnd ); 
  

  /* complete the missing code */

  /* gpos is a integer of the form 2^k, where 
   * k is the position of the target qubit. 
   * The bits set in ocnd correspond to the 
   * non-filled circles, and the bits set in icnd
   * correspond to the filled circles
   */ 

  for (i = 0; i < (1 << Nbits); i++) {
    if(((i & acnd) ^ icnd) == 0) {
      if(((i / gpos) % 2) == 0) {
        tmp0 = state[i];
        tmp1 = state[i + gpos];
        state[i] =  add(mult(g[0], tmp0), mult(g[1], tmp1));
        state[i + gpos] = add(mult(g[2], tmp0), mult(g[3], tmp1));
      }
    }
  }
}





