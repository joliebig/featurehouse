module basicGraph
open color[Edge, Node]

sig Graph {
	node: some Node,
	edge: set Edge,
	id: one Int
}

// A node in the graph
sig Node {
	id: one Int
}

// there is at least one node in the graph - redundant
assert atLeastOneNode {
	all g: Graph |some g.node
}

// all nodes are disjunct
fact a {
	all disj n1, n2: Node | n1.id != n2.id
}

// a node can only be in one Graph
fact b {
	all n: Node | some g: Graph | n in g.node
}

// An edge in the graph
sig Edge {
	n1, n2: Node
} 

// an edge can only be in one Graph
fact c {
	all e: Edge | one g: Graph | e in g.edge
}

// add one Node 
pred addNode [n: Node, g: Graph] {
	g.node = g.node + n	
}

// check it for a scope of 5
check atLeastOneNode for 5

run addNode for 5 Node, 1 Graph, 2 Edge 
