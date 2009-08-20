module filesystem

// must weaken changedFS to be able to remove
//pred changedFS[fs1, fs2: AMap] {
//	fs1 != fs2
//	fs1.designation = fs2.designation
//	fs1.root = fs2.root
//}

// Move Inode f to Directory d inside the same filesystem
pred mv (fs, fs': AMap, n: Inode, dir: Dir) {
	n in fs.objects // n is part of the file system
	dir in fs.objects // move inside the file system	
	fs'.contents = fs.contents - n.(fs.parent)->dir + dir->n
	changedFS[fs, fs'] 
}

// Moving doesn't add or delete any file system objects
assert moveAddsRemovesNone {
  all fs, fs': AMap, f: Inode, d:Dir |
    mv[fs, fs', f, d] => fs.objects = fs'.objects
}

check moveAddsRemovesNone for 5 but exactly 2 AMap // passes

