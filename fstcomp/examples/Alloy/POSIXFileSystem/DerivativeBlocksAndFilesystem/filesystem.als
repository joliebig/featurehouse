module filesystem

sig File extends Inode { 
	contents: disj some Block // essentially these are the file's clusters
}

fact FSBlocks_fact {
	all m:AMap, f: (File&m.objects) | f.contents in m.datablocks // all file contents of this file system are inside this partition
}

// count used blocks
fun du[fs: AMap]: one Int {
	#((fs.objects&File).contents)
}

// count free blocks
fun df [fs: AMap]: one Int {
	#(fs.datablocks) - du[fs]
}

pred fsWithFreeBlocks(fs: AMap) {
	df[AMap] > 0
}

// each block may belong to only one file
assert noSharedBlocks {
	no disj f1, f2: File | #(f1.contents & f2.contents) > 0
}

run fsWithFreeBlocks for 5 // passes
check noSharedBlocks for 5   // passes

// Two file systems represent the same partition, but may have different contents (used for operations on Inodes)
pred changedFS[fs1, fs2: AMap] {
	fs1 != fs2
	fs1.designation = fs2.designation
	fs1.root = fs2.root 
	fs1.datablocks = fs2.datablocks
	fs1.contents != fs2.contents
}

// Two file systems represent the same contents, but may reside on different devices (used for physical operations)
pred changedPartition[fs1, fs2: AMap] {
	fs1 != fs2 
	fs1.designation = fs2.designation 
	fs1.root = fs2.root 
	fs1.contents = fs2.contents 
	fs1.datablocks != fs2.datablocks 
}

// change the size of a partition
pred partitionResize[fs, fs': AMap] {
	#fs.datablocks != #fs'.datablocks
	changedPartition[fs, fs'] 
}

// Two file systems represent the same partition, but may have different contents (used for operations on Inodes)
assert resizingDoesNotChangeFS {
	all fs1, fs2: AMap | (
		partitionResize[fs1, fs2] =>
		fs1.root = fs2.root &&
		fs1.contents = fs2.contents)
}

run partitionResize for 5
check resizingDoesNotChangeFS for 5 but 2 AMap

