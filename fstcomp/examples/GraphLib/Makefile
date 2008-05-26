# KMB 2005 Jul 14

GCCOPTS=-Wall -O3 -fomit-frame-pointer

test: test_graph_bitmatrix test_graph_char_array test_graph_hash
	@echo
	time ./test_graph_bitmatrix 10000 1000000
	@echo
	time ./test_graph_char_array 10000 1000000
	@echo
	time ./test_graph_hash 10000 1000000

test_graph_bitmatrix: test_graph_bitmatrix.c graph_bitmatrix.h graph_bitmatrix.o
	gcc ${GCCOPTS} test_graph_bitmatrix.c graph_bitmatrix.o -o test_graph_bitmatrix

test_graph_char_array: test_graph_char_array.c graph_char_array.h graph_char_array.o
	gcc ${GCCOPTS} test_graph_char_array.c graph_char_array.o -o test_graph_char_array

test_graph_hash: test_graph_hash.c graph_hash.h graph_hash.o
	gcc ${GCCOPTS} `pkg-config --cflags --cflags --libs glib-2.0` test_graph_hash.c graph_hash.o -o test_graph_hash

graph_char_array.o: graph_char_array.h graph_char_array.c
	gcc ${GCCOPTS} -c graph_char_array.c 

graph_bitmatrix.o: graph_bitmatrix.h graph_bitmatrix.c
	gcc ${GCCOPTS} -c graph_bitmatrix.c 

graph_hash.o: graph_hash.h graph_hash.c
	gcc ${GCCOPTS} `pkg-config --cflags --cflags glib-2.0` -c graph_hash.c 

clean:
	rm -f *o test_graph_hash test_graph_bitmatrix test_graph_char_array a.out

bu:
	(cd ..; tar czvf graphlib-1.0.tgz graphlib-1.0/Makefile graphlib-1.0/Doxyfile graphlib-1.0/html/* graphlib-1.0/*h graphlib-1.0/*c)
	mcopy -o -v ../graphlib-1.0.tgz a:
	scp ../graphlib-1.0.tgz europa:
	#scp graph.h wulf1:

doc:
	doxygen
