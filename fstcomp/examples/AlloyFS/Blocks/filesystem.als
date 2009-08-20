module filesystem

sig AMap {
	datablocks: some Block // blocks inside this partition available for keeping data
} 

abstract sig Device {
	blocks: disj some Block
}

sig Block { }

fact Blocks_fact {
	all m: AMap | m.datablocks in m.designation.blocks
}

// Two file systems represent the same partition, but may have different contents (used for operations on Inodes)
pred changedFS[fs1, fs2: AMap] {
	fs1 != fs2
	fs1.designation = fs2.designation
	fs1.datablocks = fs2.datablocks
}

assert changingFSdoesNotChangePartition {
	all fs1, fs2: AMap | changedFS[fs1, fs2] => fs1.datablocks = fs2.datablocks
}
check changingFSdoesNotChangePartition for 5 but 2 AMap


// Two file systems represent the same contents, but may reside on different devices (used for physical operations)
pred changedPartition[fs1, fs2: AMap] {
	fs1 != fs2
	fs1.designation = fs2.designation
	fs1.datablocks != fs2.datablocks
}

assert changingPartitionChangesBlocks {
	all fs1, fs2: AMap | changedPartition[fs1, fs2] => fs1.datablocks != fs2.datablocks
}
check changingPartitionChangesBlocks for 5 but 2 AMap




