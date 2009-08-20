module filesystem

// must weaken changedFS to be able to remove
//pred changedFS[fs1, fs2: AMap] {
//	fs1 != fs2
//	fs1.designation = fs2.designation
//	fs1.root = fs2.root
//	fs1.contents != fs2.contents
//}

// create File
pred creat(fs, fs': AMap, cd: Dir, f: File) {
	cd in fs.objects
	!f in (AMap-fs').objects // f is new
	fs'.contents = fs.contents + (cd->f)
	changedFS[fs, fs'] 
}

// create (empty) Directory
pred mkdir(disj fs, fs': AMap, cd: Dir, dir: Dir) {
	cd in fs.objects
	!dir in (AMap-fs').objects // dir is new
	fs'.contents = fs.contents + (cd->dir)
	changedFS[fs, fs'] 
}

run creat for 4 but 2 AMap, 4 Inode
run mkdir for 4 but 2 AMap, 4 Inode

// Creating a file or a directory adds exactly one Inode
assert creatAddsOneFile {
  all fs, fs': AMap, cd: Dir, f: File |
    creat[fs, fs', cd, f]=> (#fs'.objects =#fs.objects + 1 && fs'.objects = fs.objects + f)
}

// Creating a file or a directory adds exactly one Inode
assert mkdirAddsOneDir {
  all fs, fs': AMap, cd: Dir, d: Dir |
    mkdir[fs, fs', cd, d]=> (#fs'.objects = #fs.objects +1 && fs'.objects = fs.objects + d)
}

// creat does not change another existing filesystem
assert creatKeepsDisjointFilesystemsUnchanged {
	all fs, fs', fs2: AMap, cd: Dir, f: File |
		creat[fs, fs', cd, f] && disjointPartitions[fs, fs2]
		=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
}

// mkdir does not change another existing filesystem
assert mkdirKeepsDisjointFilesystemsUnchanged {
	all fs, fs', fs2: AMap, cd: Dir, dir: Dir |
		mkdir[fs, fs', cd, dir] && disjointPartitions[fs, fs2]
		=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
}

check creatAddsOneFile for 5 
check mkdirAddsOneDir for 5
check mkdirKeepsDisjointFilesystemsUnchanged for 5 
check creatKeepsDisjointFilesystemsUnchanged for 5 
