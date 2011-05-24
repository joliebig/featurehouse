module filesystem


// find file
fun find [cd: Dir, n: Inode] : set Inode {
	n in cd.*(AMap.contents) => n else 0
}

// directory listing
fun ls[cd: Dir] : set Inode {
	cd.(AMap.contents)
}

// recursive directory listing
fun ls_r[cd: Dir] : set Inode {
	cd.^(AMap.contents)
}

// lists the full path of an Inode; actually, the parent directories are not ordered
fun fullPath[fs: AMap, f: Inode]: set Inode {
	f.^(fs.parent)
}

// if the searched Inode is in a different file system, then it cannot be found using find
assert unfindableInodes {
	all disj m1,m2: AMap, cd:Dir&m1.objects, n: m2.objects | disjointPartitions[m1,m2] => no Inode&find[cd, n]
}

assert ls_doesNotBreakFSboundaries {
	all disj m1,m2: AMap, cd:Dir&m1.objects | disjointPartitions[m1,m2] => no ls[cd] & m2.objects
}

assert ls_r_doesNotBreakFSboundaries {
	all disj m1,m2: AMap, cd:Dir&m1.objects | disjointPartitions[m1,m2] => no ls_r[cd] & m2.objects
}

check unfindableInodes for 5 but 2 AMap
check ls_doesNotBreakFSboundaries for 5 but 2 AMap
check ls_r_doesNotBreakFSboundaries for 5 but 2 AMap
