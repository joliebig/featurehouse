module Graph

one sig Graph {
	nodes: set Node
} {
	Node in nodes
}

sig Node {
	inEdges: set Edge, outEdges: set Edge,
	edges: set Edge
} {
	edges = inEdges + outEdges
}

sig Edge {
	src: one Node, dest: one Node
}

fact prevNext {
	all n: Node, e: Edge | 
		(n in e.src <=> e in n.outEdges) && 
		(n in e.dest <=> e in n.inEdges)
}

fun reachableNodes [n: Node] : Int {
	#(n.^(edges.(src + dest)))
}

pred noDoubleEdges {
	all e, e': Edge | e != e' => e.(src + dest) != e'.(src + dest)
}

run noDoubleEdges for 5

assert hasNoDoubleEdges {
	!noDoubleEdges
}

check hasNoDoubleEdges for 5

pred show { }

run show for exactly 5 Node, 2 LeftEdge, 2 RightEdge, 4 Edge