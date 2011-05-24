
void beforeFlipEdge() { 
	start=timing_init(stdout); 
}

void afterFlipEdge(unsigned int nflips) {
	stop=timing_end(stdout);
	printf("%.0f flips per second\n",nflips/(stop-start));
}
