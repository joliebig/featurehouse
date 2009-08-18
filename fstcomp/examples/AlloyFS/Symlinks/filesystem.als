module filesystem

sig AMap {
	symlinks: SymLink ->lone (Inode - SymLink) // may contain invalid target in order to comply with delete and move operations
} 

fact SymLinks_fact {
	all m: AMap | ((m.objects&SymLink) -> (SymLink.(m.symlinks))) = m.symlinks
}


// These links are actually hard- and symlinks
// - hard links: they point directly to Inodes (there is no notion of path names)
// - sym links: they are Inodes themselves, cross fs boundaries (limited) and may be broken
sig SymLink extends Inode { 
} 

// This predicate only works for two AMap-Filesystems (thus it is somewhat restricted)
pred symLinksAcrossPartitions[disj fs1,fs2: AMap, link: SymLink] {	
	disjointPartitions[fs1, fs2]
	link in fs1.objects
	#(link.(fs1.symlinks)) > 0
	link.(fs1.symlinks) in fs2.objects
}

run symLinksAcrossPartitions for 4 but 2 AMap, 8 Inode

