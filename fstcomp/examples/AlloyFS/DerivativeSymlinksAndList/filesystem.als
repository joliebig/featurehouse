module filesystem


// recursive directory listing following symbolic links, inifinite loop aware (as Alloy naturally is)
fun ls_rH[cd: Dir] : set Inode {
	cd.^(AMap.contents + AMap.symlinks)
}

// there is an Inode in a different file system reachable with ls_rH
pred ls_rH_crossesFSboundaries {
	some fs1, fs2: AMap, cd: Dir, n: Inode |
		cd in fs1.objects &&
		n in ls_rH[cd] &&
		disjointPartitions[fs1, fs2] &&
		n in fs2.objects
}

run ls_rH_crossesFSboundaries for 5
