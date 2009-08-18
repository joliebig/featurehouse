module filesystem


// Delete the file f
pred rm (fs, fs': AMap, f: File) {
	f in fs.objects
	fs'.contents = fs.contents - f.(fs.parent)->f
 	// problem: contents != objects; the following is essential
	//	fs'.objects =  fs.objects - f
	changedFS[fs, fs'] 
}

// Delete the directory d
pred rmdir(fs, fs': AMap, d: Dir) {
	d in fs.(objects - root)
	no d.(fs.contents) // d is empty
	fs'.contents = fs.contents - d.(fs.parent)->d
	changedFS[fs, fs'] 
}

// Recursively delete the file system object f
pred rm_r(fs, fs': AMap, f: Inode) {
	f in fs.(objects - root)
	let subtree = f.*(fs.contents) |
 	fs'.contents = fs.contents - subtree.(fs.parent)->subtree
	changedFS[fs, fs'] 
}

run rm for 4 but 2 AMap, 4 Inode
run rmdir for 4 but 2 AMap, 4 Inode
run rm_r for 4 but 2 AMap, 4 Inode

// rm removes exactly the specified file
assert rmRemovesOneFile {
  all fs, fs': AMap, f: File |
    rm[fs, fs', f]=> fs.objects - f = fs'.objects
}

// rmdir removes exactly the specified directory
assert rmdirRemovesOneDir {
  all fs, fs': AMap, d: Dir |
    rmdir[fs, fs', d] => fs.objects - d = fs'.objects
}

// rm_r removes exactly the specified subtree
assert rm_rRemovesSubtree {
  all fs, fs': AMap, f: Inode |
    rm_r[fs, fs', f] => fs.objects - f.*(fs.contents) = fs'.objects
}

// rm and rm_r same effect on files
assert rmAndrm_rSameForFiles {
  all fs, fs1, fs2: AMap, f: File |
    rm[fs, fs1, f] && rm_r[fs, fs2, f] => fs1.contents = fs2.contents
}

// removal operations don't change another existing filesystem
assert rmKeepsDisjointFilesystemsUnchanged {
	all fs, fs', fs2: AMap, f: File |
		rm[fs, fs', f] && disjointPartitions[fs, fs2]
		=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
}
assert rmdirKeepsDisjointFilesystemsUnchanged {
	all fs, fs', fs2: AMap, d: Dir |
		rmdir[fs, fs', d] && disjointPartitions[fs, fs2]
		=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
}
assert rm_rKeepsDisjointFilesystemsUnchanged {
	all fs, fs', fs2: AMap, n: Inode |
		rm_r[fs, fs', n] && disjointPartitions[fs, fs2]
		=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
}

check rmRemovesOneFile for 5   // passes
check rmdirRemovesOneDir for 5    // passes
check rm_rRemovesSubtree for 5    // passes
check rmAndrm_rSameForFiles for 5 // passes
check rmKeepsDisjointFilesystemsUnchanged for 5 // passes
check rmdirKeepsDisjointFilesystemsUnchanged for 5 // passes
check rm_rKeepsDisjointFilesystemsUnchanged for 5 // passes
