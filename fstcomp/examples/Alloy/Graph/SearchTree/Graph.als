module Graph

assert isBinaryTree {
	all e: Edge | e.src != e.dest
	some n: Node | (Graph.nodes = n) || 
		(Graph.nodes = n.^(edges.(src + dest)))
	all n: Node | n not in (n.^(outEdges.dest) + n.^(inEdges.src))
	all n: Node | (lone n.inEdges) && (#n.outEdges =< 2)
}

check isBinaryTree for 5

sig LeftEdge, RightEdge extends Edge { } {
	LeftEdge + RightEdge = Edge
}

fact searchTree {
	all n: Node | (#n.outEdges = 2) => 
		(some n.outEdges & LeftEdge && some n.outEdges & RightEdge)
	all n: Node | all l: LeftEdge | all r: RightEdge | 
		(l in n.outEdges => 
			(validLeftSubTree [l.dest.*(outEdges.dest), n])) && 
		(r in n.outEdges  => 
			(validRightSubTree [r.dest.*(outEdges.dest), n])) 
}

pred validLeftSubTree[children: Node, parent: one Node] {
	all child : Node | child in children => child.val < parent.val
}

pred validRightSubTree[children: Node, parent: one Node] {
	all child : Node | child in children => child.val > parent.val
}