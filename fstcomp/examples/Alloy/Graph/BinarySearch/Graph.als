module Graph

assert isBinaryTree {
	all e: Edge | e.src != e.dest
	some n: Node | (Graph.nodes = n) || 
		(Graph.nodes = n.^(edges.(src + dest)))
	all n: Node | n not in (n.^(outEdges.dest) + n.^(inEdges.src))
	all n: Node | (lone n.inEdges) && (#n.outEdges =< 2)
}

one sig Graph {
	searchRel: Node -> Int -> Node
}

fact defineSearchRel {
	all current: Node, key: Int, found: Node 	| current -> key -> found in Graph.searchRel <=> ((current.val = key && found = current) || (current.val < key && ((current.(outEdges) & RightEdge).dest) -> key -> found in Graph.searchRel) || (current.val > key && ((current.(outEdges) & LeftEdge).dest) -> key -> found in Graph.searchRel))
}

fun search [n: Node, i: Int] : lone Node {
	let res = (i.(n.(Graph.searchRel))) | (one res) => res else none
} 