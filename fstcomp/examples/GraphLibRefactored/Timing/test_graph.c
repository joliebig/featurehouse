
double t0, t1;

void beforeFlipEdge() { 
	t0=timing_init(stdout); 
}

void afterFlipEdge() {
	t1=timing_end(stdout);
	printf("%.0f flips per second\n",nflips/(t1-t0));
}
