unsigned int uni_int(unsigned int n) {
  return rand()%n;
}

void init() { original(); }

void beforeFlipEdge() {}

void afterFlipEdge(unsigned int nflips) {}

int main(int argc, char** argv) {
  unsigned int n=5000,nflips=1000000;
  unsigned int k;
  node_t i,j;
  if (argc>1) n=atoi(argv[1]);
  if (argc>2) nflips=atoi(argv[2]);
  init();
  graph_t g=graph_new(n);
  for (k=0; k<10000; k++) {
    i=uni_int(n);
    while ((j=uni_int(n))==i);
    graph_set_edge(g,i,j);
  }
  beforeFlipEdge();
  for (k=0; k<nflips; k++) {
    i=uni_int(n);
    while ((j=uni_int(n))==i);
    graph_flip_edge(g,i,j);
  }
  afterFlipEdge(nflips);
  return 0;
}
