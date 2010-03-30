// graph_char_array.h
// KMB 2005 Jul 15
/*!
A library of low-level graph functions, optimized for small, dense graphs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/times.h>

// graph_idx maps adjacency matrix elements to a linear array as follows.
// The diagonal elements are not used.

// 0 1 3 6 10 ...
// 2 4 7 11 ...
// 5 8 12 ...
// 9 13...
// 14 ...
// ...

#define div2(x) ((x)>>1)
#define graph_idx(i,j) ((i)<=(j)?(div2((j)*((j)+1))+(i)):(div2((i)*((i)+1))+(j)))

/*! \file graph_char_array.h */

//! type of a node
typedef unsigned int node_t;

//! type of a graph
typedef char* graph_t;

//! make a new undirected graph with n nodes
graph_t graph_new(node_t n);

//! destroy a graph
void graph_clear(graph_t g);

//! add edge (i,j) to the graph
void graph_set_edge(graph_t g, node_t i, node_t j);

//! return true if edge (i,j) exists in g
int graph_get_edge(graph_t g, node_t i, node_t j);

//! delete edge (i,j) in g (if it exists)
void graph_del_edge(graph_t g, node_t i, node_t j);

//! add edge (i,j) if it exists, else delete it.  Return new state.
int graph_flip_edge(graph_t g, node_t i, node_t j);

//! print the whole graph
void graph_show(graph_t g, node_t n);

//! return the degree of node i
node_t graph_get_degree(graph_t g, node_t i);

//! return the number of edges in the graph
node_t graph_get_nedges(graph_t g, node_t n);

//! write a file suitable for input to dot or neato
void graph_write_dotfile(char* fn, graph_t g, unsigned int n);


