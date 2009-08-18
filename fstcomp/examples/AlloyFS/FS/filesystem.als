module  filesystem sig  AMap {	designation: one Device , 	datablocks: some Block , 	root: one Dir,	objects: set Inode,	contents: Dir lone-> Inode,	parent: Inode ->lone Dir , 	symlinks: SymLink ->lone (Inode - SymLink)} 
abstract sig  Device {	blocks: disj some Block} 
one sig  Dev0 ,  Dev1 ,  Dev2  extends Device {} // These are two different partitions on the same system
pred disjointPartitions[fs1, fs2: AMap] {	fs1.designation != fs2.designation	fs1.root != fs2.root	#(fs1.objects&fs2.objects) = 0
} // All file systems are disjoint (they might coexist on the same machine)
pred allDisjointPartitions {	all disj fs1, fs2: AMap | disjointPartitions[fs1, fs2]
} 
run allDisjointPartitions for 6 
fact Blocks_fact {	all m: AMap | m.datablocks in m.designation.blocks
} // Two file systems represent the same contents, but may reside on different devices (used for physical operations)
pred changedPartition[fs1, fs2: AMap] {	fs1 != fs2	fs1.designation = fs2.designation
} 
run changedPartition for 6 sig  Block {} 
fact FS_fact {	all m:AMap | no m.root.(m.parent)	all m:AMap | m.objects = m.root.*(m.contents)	all m:AMap | m.contents in m.objects->m.objects	all m:AMap | m.parent = ~(m.contents)
} // File system objects
abstract sig  Inode {} {	all n: Inode | n in AMap.objects} sig  File  extends Inode {} sig  Dir  extends Inode {} // Two file systems represent the same partition, but may have different contents (used for operations on Inodes)
pred changedFS[fs1, fs2: AMap] {	fs1 != fs2	fs1.designation = fs2.designation	fs1.root = fs2.root
} 
run changedFS for 6 // create File
pred creat(fs, fs': AMap, cd: Dir, f: File) {	cd in fs.objects	!f in (AMap-fs').objects	fs'.contents = fs.contents + (cd->f)	changedFS[fs, fs']
} // create (empty) Directory
pred mkdir(disj fs, fs': AMap, cd: Dir, dir: Dir) {	cd in fs.objects	!dir in (AMap-fs').objects	fs'.contents = fs.contents + (cd->dir)	changedFS[fs, fs']
} 
run creat for 4 
run mkdir for 4 // Creating a file or a directory adds exactly one Inode
assert creatAddsOneFile { all fs, fs': AMap, cd: Dir, f: File | creat[fs, fs', cd, f]=> (#fs'.objects =#fs.objects + 1 && fs'.objects = fs.objects + f)
} // Creating a file or a directory adds exactly one Inode
assert mkdirAddsOneDir { all fs, fs': AMap, cd: Dir, d: Dir | mkdir[fs, fs', cd, d]=> (#fs'.objects = #fs.objects +1 && fs'.objects = fs.objects + d)
} // creat does not change another existing filesystem
assert creatKeepsDisjointFilesystemsUnchanged {	all fs, fs', fs2: AMap, cd: Dir, f: File |	creat[fs, fs', cd, f] && disjointPartitions[fs, fs2]	=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
} // mkdir does not change another existing filesystem
assert mkdirKeepsDisjointFilesystemsUnchanged {	all fs, fs', fs2: AMap, cd: Dir, dir: Dir |	mkdir[fs, fs', cd, dir] && disjointPartitions[fs, fs2]	=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
} 
check creatAddsOneFile for 5 
check mkdirAddsOneDir for 5 
check mkdirKeepsDisjointFilesystemsUnchanged for 5 
check creatKeepsDisjointFilesystemsUnchanged for 5 // find file
fun find [fs: AMap, cd: Dir, f: Inode] : set Inode {	f in cd.*(fs.contents) => f else 0
} // directory listing
fun ls[fs: AMap, cd: Dir] : set Inode {	cd.(fs.contents)
} // recursive directory listing
fun ls_r[fs: AMap, cd: Dir] : set Inode {	cd.^(fs.contents)
} // lists the full path of an Inode; actually, the parent directories are not ordered
fun fullPath[fs: AMap, f: Inode]: set Inode {	f.^(fs.parent)
} // Move Inode f to Directory d inside the same filesystem
pred mv (fs, fs': AMap, n: Inode, dir: Dir) {	n in fs.objects	dir in fs.objects	fs'.contents = fs.contents - n.(fs.parent)->dir + dir->n	changedFS[fs, fs']
} // Moving doesn't add or delete any file system objects
assert moveAddsRemovesNone { all fs, fs': AMap, f: Inode, d:Dir | mv[fs, fs', f, d] => fs.objects = fs'.objects
} 
check moveAddsRemovesNone for 5 // mv (inside a FS) does not change the storage state of the filesystem
assert mvKeepsStorageStateUnchanged { all fs, fs': AMap, n: Inode, dir: Dir | mv[fs, fs', n, dir]=> fs.datablocks = fs'.datablocks
} 
check mvKeepsStorageStateUnchanged for 5 // Delete the file f
pred rm (fs, fs': AMap, f: File) {	f in fs.objects	fs'.contents = fs.contents - f.(fs.parent)->f	changedFS[fs, fs']
} // Delete the directory d
pred rmdir(fs, fs': AMap, d: Dir) {	d in fs.(objects - root)	no d.(fs.contents)	fs'.contents = fs.contents - d.(fs.parent)->d	changedFS[fs, fs']
} // Recursively delete the file system object f
pred rm_r(fs, fs': AMap, f: Inode) {	f in fs.(objects - root)	let subtree = f.*(fs.contents) |	fs'.contents = fs.contents - subtree.(fs.parent)->subtree	changedFS[fs, fs']
} 
run rm for 4 
run rmdir for 4 
run rm_r for 4 // rm removes exactly the specified file
assert rmRemovesOneFile { all fs, fs': AMap, f: File | rm[fs, fs', f]=> fs.objects - f = fs'.objects
} // rmdir removes exactly the specified directory
assert rmdirRemovesOneDir { all fs, fs': AMap, d: Dir | rmdir[fs, fs', d] => fs.objects - d = fs'.objects
} // rm_r removes exactly the specified subtree
assert rm_rRemovesSubtree { all fs, fs': AMap, f: Inode | rm_r[fs, fs', f] => fs.objects - f.*(fs.contents) = fs'.objects
} // rm and rm_r same effect on files
assert rmAndrm_rSameForFiles { all fs, fs1, fs2: AMap, f: File | rm[fs, fs1, f] && rm_r[fs, fs2, f] => fs1.contents = fs2.contents
} // removal operations don't change another existing filesystem
assert rmKeepsDisjointFilesystemsUnchanged {	all fs, fs', fs2: AMap, f: File |	rm[fs, fs', f] && disjointPartitions[fs, fs2]	=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
} 
assert rmdirKeepsDisjointFilesystemsUnchanged {	all fs, fs', fs2: AMap, d: Dir |	rmdir[fs, fs', d] && disjointPartitions[fs, fs2]	=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
} 
assert rm_rKeepsDisjointFilesystemsUnchanged {	all fs, fs', fs2: AMap, n: Inode |	rm_r[fs, fs', n] && disjointPartitions[fs, fs2]	=> changedFS[fs, fs'] && disjointPartitions[fs', fs2]
} 
check rmRemovesOneFile for 5 // passes
check rmdirRemovesOneDir for 5 // passes
check rm_rRemovesSubtree for 5 // passes
check rmAndrm_rSameForFiles for 5 // passes
check rmKeepsDisjointFilesystemsUnchanged for 5 // passes
check rmdirKeepsDisjointFilesystemsUnchanged for 5 // passes
check rm_rKeepsDisjointFilesystemsUnchanged for 5 // rm frees memory (in this model, every file uses at least one block)
assert rmFreesMemory { all fs, fs': AMap, f: File | rm[fs, fs', f]=> df[fs'] > df[fs]
} // rmdir (inside a FS) does not change the storage state of the filesystem
assert rmdirKeepsStorageStateUnchanged { all fs, fs': AMap, dir: Dir | rmdir[fs, fs', dir]=> fs.datablocks = fs'.datablocks
} // rm_r frees memory (in this model, every file uses at least one block)
assert rm_rFreesMemory { all fs, fs': AMap, n: Inode | rm_r[fs, fs', n] && some File&n.^(fs.contents) => df[fs'] > df[fs]
} 
check rmFreesMemory for 5 // passes
check rmdirKeepsStorageStateUnchanged for 5 // passes
check rm_rFreesMemory for 5 
fact SymLinks_fact {	all m: AMap | ((m.objects&SymLink) -> (SymLink.(m.symlinks))) = m.symlinks
} sig  SymLink  extends Inode {} // This predicate only works for two AMap-Filesystems (thus it is somewhat restricted)
pred symLinksAcrossPartitions[disj fs1,fs2: AMap, link: SymLink] {	disjointPartitions[fs1, fs2]	link in fs1.objects	#(link.(fs1.symlinks)) > 0	link.(fs1.symlinks) in fs2.objects
} 
run symLinksAcrossPartitions for 5 // recursive directory listing following symbolic links, inifinite loop aware (as Alloy naturally is)
fun ls_rH[cd: Dir] : set Inode {	cd.^(AMap.contents + AMap.symlinks)
} // there is an Inode in a different file system reachable with ls_rH
pred ls_rH_crossesFSboundaries {	some fs1, fs2: AMap, cd: Dir, n: Inode |	cd in fs1.objects &&	n in ls_rH[cd] &&	disjointPartitions[fs1, fs2] &&	n in fs2.objects
} 
run ls_rH_crossesFSboundaries for 5
