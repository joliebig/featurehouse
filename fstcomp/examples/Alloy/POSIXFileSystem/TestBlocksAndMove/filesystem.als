module filesystem

// mv (inside a FS) does not change the storage state of the filesystem
assert mvKeepsStorageStateUnchanged {
  all fs, fs': AMap, n: Inode, dir: Dir |
    mv[fs, fs', n, dir]=> fs.datablocks = fs'.datablocks
}

check mvKeepsStorageStateUnchanged for 5 // passes

