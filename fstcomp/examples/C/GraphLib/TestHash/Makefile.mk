test_graph_hash: test_graph.c graph_hash.h graph_hash.o
	gcc ${GCCOPTS} `pkg-config --cflags --cflags --libs glib-2.0` test_graph.c graph_hash.o -o test_graph
	@echo
	time ./test_graph 10000 1000000

graph_hash.o: graph_hash.h graph_hash.c
	gcc ${GCCOPTS} `pkg-config --cflags --cflags glib-2.0` -c graph_hash.c 
