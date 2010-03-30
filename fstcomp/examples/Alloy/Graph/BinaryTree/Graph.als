module Graph

assert isTree {
	all e, e': Edge | e != e' => e.(src + dest) != e'.(src + dest)
	some n: Node | (Graph.nodes = n) || 
		(Graph.nodes = n.^(edges.(src + dest)))
	all n: Node | n not in (n.^(outEdges.dest) + n.^(inEdges.src))
	all n: Node | lone n.inEdges
}

check isTree for 5

fact binaryTree {
	all n: Node | #n.outEdges =< 2
}