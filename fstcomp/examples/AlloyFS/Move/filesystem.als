module filesystem


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

check moveAddsRemovesNone for 5   // passes

