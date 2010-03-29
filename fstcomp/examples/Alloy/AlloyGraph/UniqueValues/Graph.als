module Graph

sig Node {
	val: one Int
}

fact uniqueValues {
	all disj n, n': Node | n.val != n'.val
}