module Graph

pred isConnected {
	some n: Node | (Graph.nodes = n) || 
		(Graph.nodes = n.^(edges.(src + dest)))
}

pred noCycles {
	all n: Node | n not in (n.^(outEdges.dest) + n.^(inEdges.src))
}

pred loneParent {
	all n: Node | lone n.inEdges
}

fact isTree {
	noDoubleEdges && isConnected && noCycles && loneParent
}