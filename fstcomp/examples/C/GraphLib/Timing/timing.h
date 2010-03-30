// timing functions...

/*! \file timing.h */

#include <time.h>
#include <sys/times.h>

static long tps; // ticks per second
static struct tms t;
static clock_t t0,t1;
double start, stop;

//! initialize timing
double timing_init(FILE *f) {
  tps=sysconf(_SC_CLK_TCK); // ticks per second
  times(&t); 
  t0=t.tms_utime; 
  fprintf(f,"start: t0=%.2f seconds\n",t0/(double)tps);
  return t0/(double)tps;
}

//! return time since last call to timing_init
double timing_end(FILE *f) {
  times(&t);
  t1=t.tms_utime;
  fprintf(f,"end: t1=%.2f seconds\n",t1/(double)tps);
  fprintf(f,"%f seconds user time elapsed\n",(double)(t1-t0)/tps);
  return (double)(t1-t0)/tps;
}
