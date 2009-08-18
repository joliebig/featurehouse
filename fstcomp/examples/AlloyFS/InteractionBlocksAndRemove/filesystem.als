module filesystem


// rm frees memory (in this model, every file uses at least one block)
assert rmFreesMemory {
  all fs, fs': AMap, f: File |
    rm[fs, fs', f]=> df[fs'] > df[fs]
}

// rmdir (inside a FS) does not change the storage state of the filesystem
assert rmdirKeepsStorageStateUnchanged {
  all fs, fs': AMap, dir: Dir |
    rmdir[fs, fs', dir]=> fs.datablocks = fs'.datablocks
}

// rm_r frees memory (in this model, every file uses at least one block)
assert rm_rFreesMemory {
  all fs, fs': AMap, n: Inode |
    rm_r[fs, fs', n] && some File&n.^(fs.contents) => df[fs'] > df[fs]
}

check rmFreesMemory for 5   // passes
check rmdirKeepsStorageStateUnchanged for 5 // passes
check rm_rFreesMemory for 5   // passes

