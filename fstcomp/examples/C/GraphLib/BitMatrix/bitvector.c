// KMB 2005 July 15
// low-level bitvector operations, mainly for use in bitmatrices

#include <stdlib.h>
#include <stdio.h>
#include "bits_in_byte.h"
#include "bit_shift_table.h"

typedef int* bitvector_t;

bitvector_t bitvector_new(unsigned int nbits) {
  return calloc((1+((nbits-1)>>5)),sizeof(int));
}

void bitvector_free(bitvector_t b) {
  free(b);
}

#define bitvector_q(b,i)     (b[(i)>>5]&bit_shift_table[(i)&0x1f]) // bit query
#define bitvector_get(b,i)   (((b[(i)>>5]>>(i)&0x1f))&1)
#define bitvector_set(b,i)   (b[(i)>>5]|=bit_shift_table[(i)&0x1f])
#define bitvector_clear(b,i) (b[(i)>>5]&=!bit_shift_table[(i)&0x1f])
#define bitvector_flip(b,i)  (b[(i)>>5]^=bit_shift_table[(i)&0x1f])

unsigned int bitvector_count_slow(bitvector_t b, unsigned int nbits) {
  unsigned int i,j,k,c=0,ncells=1+((nbits-1)>>5);
  for (i=0; i<ncells; i++) {
    for (j=0; j<32; j++) if ((k=32*i+j)<nbits) c+=bitvector_get(b,k);
  }
  return c;
}

unsigned int bitvector_count(bitvector_t b, unsigned int nbits) {
  unsigned int i,c=0,ncells=1+((nbits-1)>>5);
  for (i=0; i<ncells; i++) {
    c+=bits_in_byte[(unsigned int)((b[i]>> 0)&0xff)];
    c+=bits_in_byte[(unsigned int)((b[i]>> 8)&0xff)];
    c+=bits_in_byte[(unsigned int)((b[i]>>16)&0xff)];
    c+=bits_in_byte[(unsigned int)((b[i]>>24)&0xff)];
  }
  return c;
}

void bitvector_show(bitvector_t b, unsigned int nbits) {
  unsigned int i,j,k,ncells=1+((nbits-1)>>5);
  for (i=0; i<ncells; i++) {
    for (j=0; j<32; j++) if ((k=32*i+j)<nbits) {
      bitvector_q(b,k)?printf("1"):printf("0");
    }
    printf(" ");
  }
}

#ifdef TEST_BIT_VECTOR
// gcc -Wall -O3 -DTEST_BIT_VECTOR  bitvector.c && ./a.out
int main() {
  const unsigned int nbits=40;
  bitvector_t b=bitvector_new(nbits);
  if (sizeof(int)!=4) {
    fprintf(stderr,"Size problem in bitvector.c");
    exit(1);
  }
  bitvector_show(b,nbits); printf("\n");
  bitvector_set(b,0);
  bitvector_set(b,1);
  bitvector_set(b,11);
  bitvector_set(b,38);
  bitvector_show(b,nbits); printf("\n");
  bitvector_clear(b,38);
  bitvector_show(b,nbits); printf("\n");
  bitvector_flip(b,0);
  bitvector_show(b,nbits); printf("\n");
  bitvector_flip(b,0);
  bitvector_show(b,nbits); printf("\n");
  printf("count_slow=%u\n",bitvector_count_slow(b,nbits));
  printf("count=%u\n",bitvector_count(b,nbits));
  bitvector_free(b);
  return 0;
}
#endif
