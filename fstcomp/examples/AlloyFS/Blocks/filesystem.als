module filesystem

sig AMap {
	datablocks: some Block // blocks inside this partition available for keeping data
} 

abstract sig Device {
	blocks: disj some Block
}

fact Blocks_fact {
	all m: AMap | m.datablocks in m.designation.blocks
}


// Two file systems represent the same contents, but may reside on different devices (used for physical operations)
pred changedPartition[fs1, fs2: AMap] {
	fs1 != fs2
	fs1.designation = fs2.designation
}
run changedPartition for 6

sig Block { }


