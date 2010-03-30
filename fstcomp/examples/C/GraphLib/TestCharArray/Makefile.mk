test_graph_char_array: test_graph.c graph_char_array.h graph_char_array.o
	gcc ${GCCOPTS} test_graph.c graph_char_array.o -o test_graph
	@echo
	time ./test_graph 10000 1000000

graph_char_array.o: graph_char_array.h graph_char_array.c
	gcc ${GCCOPTS} -c graph_char_array.c 
