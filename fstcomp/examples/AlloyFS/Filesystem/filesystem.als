module filesystem

sig AMap {
	root: one Dir,
 	objects: set Inode,
	contents: Dir lone-> Inode,
	parent: Inode ->lone Dir
} 

fact FS_fact {
	// root has no parent
	all m:AMap | no m.root.(m.parent)
	// objects are those reachable from the root
	all m:AMap | m.objects = m.root.*(m.contents)
	// contents only defined on objects
	all m:AMap | m.contents in m.objects->m.objects
	// parent is the inverse of contents
	all m:AMap | m.parent = ~(m.contents)
}

// File system objects
abstract sig Inode {} {
	// all Inodes must have a parent (no Inodes belonging to no FS)
	all n: Inode | n in AMap.objects
}

sig File extends Inode { 
}

sig Dir extends Inode { } // directory contents needed at top level


// filesystem relations (relation between filesystems)

// Two file systems represent the same partition, but may have different contents (used for operations on Inodes)
pred changedFS[fs1, fs2: AMap] {
	fs1 != fs2
	fs1.designation = fs2.designation
	fs1.root = fs2.root
}
run changedFS for 6


// These are two different partitions on the same system
pred disjointPartitions[fs1, fs2: AMap] {
	fs1.designation != fs2.designation
	fs1.root != fs2.root
	#(fs1.objects&fs2.objects) = 0
}