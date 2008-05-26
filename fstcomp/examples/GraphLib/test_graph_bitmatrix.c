#include "graph_bitmatrix.h"
#include "timing.h"

unsigned int uni_int(unsigned int n) {
  return rand()%n;
}

int main(int argc, char** argv) {
  unsigned int n=5000,nflips=1000000;
  unsigned int k;
  double t0,t1;
  node_t i,j;
  if (argc>1) n=atoi(argv[1]);
  if (argc>2) nflips=atoi(argv[2]);
  graph_t g=graph_new(n);
  for (k=0; k<10000; k++) {
    i=uni_int(n);
    while ((j=uni_int(n))==i);
    graph_set_edge(g,i,j);
  }
  t0=timing_init(stdout);
  for (k=0; k<nflips; k++) {
    i=uni_int(n);
    while ((j=uni_int(n))==i);
    graph_flip_edge(g,i,j);
  }
  t1=timing_end(stdout);
  //graph_show_degree_dist(g);
  printf("%.0f flips per second\n",nflips/(t1-t0));
  return 0;
}
