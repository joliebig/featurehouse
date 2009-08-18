module filesystem


assert CreatNeedsFreeSpace {
  all fs, fs': AMap, cd: Dir, f: File |
    creat[fs, fs', cd, f]=> df[fs] > 0
}

// creat reduces the number of free blocks (in this model, every file uses at least one block)
assert creatLessensSpace {
  all fs, fs': AMap, cd: Dir, f: File |
    creat[fs, fs', cd, f]=> df[fs'] < df[fs] 
}

assert mkdirKeepsDatablocksConstant {
  all fs, fs': AMap, cd: Dir, dir: Dir |
    mkdir[fs, fs', cd, dir]=> df[fs'] = df[fs] 
}

check CreatNeedsFreeSpace for 5   // passes
check creatLessensSpace for 5   // passes
check mkdirKeepsDatablocksConstant for 5 //passes
