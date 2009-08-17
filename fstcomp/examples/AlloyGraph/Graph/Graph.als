module  Graph 
one sig  Graph {	nodes: set Node , 	searchRel: Node -> Int -> Node} {	Node in nodes} sig  Node {	inEdges: set Edge ,  outEdges: set Edge , 	edges: set Edge , 	val: one Int} {	edges = inEdges + outEdges} sig  Edge {	src: one Node, dest: one Node} 
fact prevNext {	all n: Node, e: Edge |	(n in e.src <=> e in n.outEdges) &&	(n in e.dest <=> e in n.inEdges)
} 
fun reachableNodes [n: Node] : Int {	#((n.^(edges.(src + dest)) - n))
} 
pred noDoubleEdges {	all e, e': Edge | e != e' => e.(src + dest) != e'.(src + dest)
} 
run noDoubleEdges for 5 
assert hasNoDoubleEdges {	!noDoubleEdges
} 
check hasNoDoubleEdges for 5 
pred show { } 
run show for exactly 5 Node, 2 LeftEdge, 2 RightEdge, 4 Edge 
pred isConnected {	some n: Node | (Graph.nodes = n) ||	(Graph.nodes = n.^(edges.(src + dest)))
} 
pred noCycles {	all n: Node | n not in (n.^(outEdges.dest) + n.^(inEdges.src))
} 
pred loneParent {	all n: Node | lone n.inEdges
} 
fact isTree {	noDoubleEdges && isConnected && noCycles && loneParent
} 
fact uniqueValues {	all disj n, n': Node | n.val != n'.val
} 
assert isTree {	all e, e': Edge | e != e' => e.(src + dest) != e'.(src + dest)	some n: Node | (Graph.nodes = n) ||	(Graph.nodes = n.^(edges.(src + dest)))	all n: Node | n not in (n.^(outEdges.dest) + n.^(inEdges.src))	all n: Node | lone n.inEdges
} 
check isTree for 5 
fact binaryTree {	all n: Node | #n.outEdges =< 2
} 
assert isBinaryTree {	all e: Edge | e.src != e.dest	some n: Node | (Graph.nodes = n) ||	(Graph.nodes = n.^(edges.(src + dest)))	all n: Node | n not in (n.^(outEdges.dest) + n.^(inEdges.src))	all n: Node | (lone n.inEdges) && (#n.outEdges =< 2)
} 
check isBinaryTree for 5 sig  LeftEdge ,  RightEdge  extends Edge {} {	LeftEdge + RightEdge = Edge} 
fact searchTree {	all n: Node | (#n.outEdges = 2) =>	(some n.outEdges & LeftEdge && some n.outEdges & RightEdge)	all n: Node | all l: LeftEdge | all r: RightEdge |	(l in n.outEdges =>	(validLeftSubTree [l.dest.*(outEdges.dest), n])) &&	(r in n.outEdges =>	(validRightSubTree [r.dest.*(outEdges.dest), n]))
} 
pred validLeftSubTree[children: Node, parent: one Node] {	all child : Node | child in children => child.val < parent.val
} 
pred validRightSubTree[children: Node, parent: one Node] {	all child : Node | child in children => child.val > parent.val
} 
fact defineSearchRel {	all current: Node, key: Int, found: Node	| current -> key -> found in Graph.searchRel <=> ((current.val = key && found = current) || (current.val < key && ((current.(outEdges) & RightEdge).dest) -> key -> found in Graph.searchRel) || (current.val > key && ((current.(outEdges) & LeftEdge).dest) -> key -> found in Graph.searchRel))
} 
fun search [n: Node, i: Int] : lone Node {	let res = (i.(n.(Graph.searchRel))) | (one res) => res else none
}
