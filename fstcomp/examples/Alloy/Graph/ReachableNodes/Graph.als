module Graph

fun reachableNodes [n: Node] : Int {
	#((n.^(edges.(src + dest)) - n))
}