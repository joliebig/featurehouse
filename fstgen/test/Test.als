module filesystem

// allocation map = partition = file system - central file system allocation map needed for alloy
sig AMap {
	designation: one Device
} 

abstract sig Device {
}
one sig Dev0, Dev1, Dev2 extends Device {}

// These are two different partitions on the same system
pred disjointPartitions[fs1, fs2: AMap] {
	fs1 != fs2
	fs1.designation != fs2.designation
}

// All file systems are disjoint (they might coexist on the same machine)
pred allDisjointPartitions {
	all disj fs1, fs2: AMap | disjointPartitions[fs1, fs2]
}
run allDisjointPartitions for 6 but 3 AMap