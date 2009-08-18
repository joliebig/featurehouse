module filesystem


// find file
fun find [fs: AMap, cd: Dir, f: Inode] : set Inode {
	f in cd.*(fs.contents) => f else 0
}

// directory listing
fun ls[fs: AMap, cd: Dir] : set Inode {
	cd.(fs.contents)
}

// recursive directory listing
fun ls_r[fs: AMap, cd: Dir] : set Inode {
	cd.^(fs.contents)
}

// lists the full path of an Inode; actually, the parent directories are not ordered
fun fullPath[fs: AMap, f: Inode]: set Inode {
	f.^(fs.parent)
}
