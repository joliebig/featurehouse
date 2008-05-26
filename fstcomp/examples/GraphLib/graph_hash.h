// graph_hash.h
// KMB 2005 Jul 14
/*!
A library of low-level graph functions, optimized for large, sparse graphs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/times.h>
#define NDEBUG // switches off asserts
#include <assert.h>
#include <glib.h>

/*! \file graph_hash.h */


//! type of a node
typedef gushort node_t;

//! type of a graph
typedef struct _graph* graph_t;

//! make a new undirected graph with n nodes
graph_t graph_new(node_t n);

//! destroy a graph
void graph_clear(graph_t g);

//! add edge (i,j) to the graph
void graph_set_edge(graph_t g, node_t i, node_t j);

//! return true if edge (i,j) exists in g
gboolean graph_get_edge(graph_t g, node_t i, node_t j);

//! delete edge (i,j) in g (if it exists)
gboolean graph_del_edge(graph_t g, node_t i, node_t j);

//! add edge (i,j) if it exists, else delete it
void graph_flip_edge(graph_t g, node_t i, node_t j);

//! print a table of degrees of all nodes
void graph_show_degrees(graph_t);

//! list neighbours of node i
void graph_show_neighbours(graph_t,node_t);

//! print the whole graph
void graph_show(graph_t g);

//! return the degree of node i
node_t graph_get_degree(graph_t g, node_t i);

//! return the number of edges in the graph
node_t graph_get_nedges(graph_t g);

//! print a table showing the degree distribution in g
void graph_show_degree_dist(graph_t g);

//! write a file suitable for input to dot or neato
void graph_write_dotfile(char* fn, graph_t g, unsigned int n);

//! write a file suitable for input to gloss (from AGLO)
void graph_write_glossfile(char* fn, graph_t g, unsigned int n);

//! initialize timing
double graph_timing_init(FILE *f);

//! finalize timing
double graph_timing_end(FILE *f);

//! show_ lib version
void graph_show_glib_version(void);

