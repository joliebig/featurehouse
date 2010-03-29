test_graph_bitmatrix: test_graph.c graph_bitmatrix.h graph_bitmatrix.o
	gcc ${GCCOPTS} test_graph.c graph_bitmatrix.o -o test_graph
	@echo
	time ./test_graph 10000 1000000

graph_bitmatrix.o: graph_bitmatrix.h graph_bitmatrix.c
	gcc ${GCCOPTS} -c graph_bitmatrix.c 
