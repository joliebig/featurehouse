// graph_bitmatrix.h
// KMB 2005 Jul 15
/*!
A library of low-level graph functions, optimized for medium size, dense graphs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

typedef int ** _graph_t; // lowlevel graph type, internal use only

struct _graph {
  _graph_t g;
  unsigned int nnodes;
  unsigned int *degree;        // degree of each node
  unsigned int *degree_dist;   // [k]=number of nodes of degree k
};

//! type of a graph
typedef struct _graph* graph_t;

/*! \file graph_bitmatrix.h */

//! type of a node
typedef unsigned int node_t;

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
node_t graph_get_nedges(graph_t g);

//! write a file suitable for input to dot or neato
void graph_write_dotfile(char* fn, graph_t g, unsigned int n);

//! initialize timing
double graph_timing_init(FILE *f);

//! finalize timing
double graph_timing_end(FILE *f);

