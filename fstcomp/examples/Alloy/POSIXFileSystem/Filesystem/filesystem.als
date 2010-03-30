module filesystem

sig AMap {
	root: one Dir,
 	objects: set Inode,
	contents: Composite lone-> Inode,
	parent: Inode ->lone Dir
} 

fact FS_fact {
	// root has no parent
	all m:AMap | no m.root.(m.parent)
	// objects are those reachable from directories, starting at root
	all m:AMap | m.objects = m.root.*(Dir <: m.contents)
	// contents only defined on objects
	all m:AMap | m.contents in m.objects->Inode
	// parent is the inverse of contents
	all m:AMap, d: Dir, n: Inode | n in (d.(m.contents)) => n.(m.parent) = d
}

// File system objects
abstract sig Inode {} {
	// all Inodes must have a parent (no Inodes belonging to no FS)
	all n: Inode | n in AMap.objects
}

sig File extends Inode { 
}

abstract sig Composite extends Inode { } // directory contents needed at top level

sig Dir extends Composite {}

// filesystem relations (relation between filesystems)

// Two file systems represent the same partition, but may have different contents (used for operations on Inodes)
pred changedFS[fs1, fs2: AMap] {
	fs1 != fs2
	fs1.designation = fs2.designation
	fs1.root = fs2.root
	fs1.contents != fs2.contents
}

assert changingFSChangesContents {
	all fs1, fs2: AMap | changedFS[fs1, fs2] => fs1.contents != fs2.contents
}
check changingFSChangesContents for 5 but 2 AMap


// Two file systems represent the same contents, but may reside on different devices (used for physical operations)
pred changedPartition[fs1, fs2: AMap] {
	fs1 != fs2
	fs1.designation = fs2.designation
	fs1.root = fs2.root
	fs1.contents = fs2.contents
}

assert changingPartitiondoesNotChangeFS {
	all fs1, fs2: AMap | changedPartition[fs1, fs2] => 	fs1.root = fs2.root &&	fs1.contents = fs2.contents
}
check changingPartitiondoesNotChangeFS for 5 but 2 AMap


// These are two different partitions on the same system
pred disjointPartitions[fs1, fs2: AMap] {
	fs1.designation != fs2.designation
	fs1.root != fs2.root
	#(fs1.objects&fs2.objects) = 0
}
