"""
rebal.py - rebalance a ham or spam test directory
usage: rebal.py [ options ]
options:
   -d     - dry run; display what would be moved, but don't do it [%(DRYRUN)s]
   -n num - specify number of files per Set dir desired [%(NPERDIR)s]
   -t     - top directory, holding Set and reservoir subdirs [%(TOPDIR)s]
   -v     - tell user what's happening; opposite of -q [%(VERBOSE)s]
   -q     - be quiet about what's happening; opposite of -v [not %(VERBOSE)s]
   -c     - confirm file moves into Set directory; opposite of -Q [%(CONFIRM)s]
   -Q     - don't confirm moves; opposite of -c; independent of -v/-q
   -h     - display this message and quit
If you have a non-standard test setup, you can use -r/-s instead of -t:
   -r res - specify an alternate reservoir [%(RESDIR)s]
   -s set - specify an alternate Set prefix [%(SETPREFIX)s]
Moves files randomly among the Set subdirectories and a reservoir directory to
leave -n files in each Set directory.  By default, the Set1, Set2, ..., and
reservoir subdirectories under (relative path) Data/Ham/ are rebalanced; this
can be changed with the -t option.  The script will work with a variable
number of Set directories, but they must already exist, and the reservoir
directory must also exist.
It's recommended that you run with the -d (dry run) option first, to see what
the script would do without actually moving any files.  If, e.g., you
accidentally mix up spam Sets with your Ham reservoir, it could be very
difficult to recover from that mistake.
See the module comments for examples.
"""
import os
import sys
import random
import glob
import getopt
try:
    True, False
except NameError:
    True, False = 1, 0
NPERDIR = 4000
TOPDIR = os.path.join('Data', 'Ham')
RESDIR = os.path.join(TOPDIR, 'reservoir')
SETPREFIX = os.path.join(TOPDIR, 'Set')
VERBOSE = True
CONFIRM = True
DRYRUN = False
def usage(msg=None):
    if msg:
        print >> sys.stderr, str(msg)
        print >> sys.stderr
    print >> sys.stderr, __doc__ % globals()
def migrate(f, targetdir, verbose):
    """Move f into targetdir, renaming if needed to avoid name clashes.
       The basename of the moved file is returned; this may not be the
       same as the basename of f, if the file had to be renamed because
       a file with f's basename already existed in targetdir.
    """
    base = os.path.basename(f)
    out = os.path.join(targetdir, base)
    while os.path.exists(out):
        basename, ext = os.path.splitext(base)
        digits = random.randrange(100000000)
        out = os.path.join(targetdir, str(digits) + ext)
    if verbose:
        print "moving", f, "to", out
    os.rename(f, out)
    return os.path.basename(out)
def main(args):
    nperdir = NPERDIR
    verbose = VERBOSE
    confirm = CONFIRM
    dryrun = DRYRUN
    topdir = resdir = setprefix = None
    try:
        opts, args = getopt.getopt(args, "dr:s:t:n:vqcQh")
    except getopt.GetoptError, msg:
        usage(msg)
        return 1
    for opt, arg in opts:
        if opt == "-n":
            nperdir = int(arg)
        elif opt == "-t":
            topdir = arg
        elif opt == "-r":
            resdir = arg
        elif opt == "-s":
            setprefix = arg
        elif opt == "-v":
            verbose = True
        elif opt == "-c":
            confirm = True
        elif opt == "-q":
            verbose = False
        elif opt == "-Q":
            confirm = False
        elif opt == "-d":
            dryrun = True
        elif opt == "-h":
            usage()
            return 0
        else:
            raise SystemError("internal error on option '%s'" % opt)
    if topdir is not None:
        if resdir is not None or setprefix is not None:
            usage("-t can't be specified with -r or -s")
            return -1
        setprefix = os.path.join(topdir, "Set")
        resdir = os.path.join(topdir, "reservoir")
    else:
        if setprefix is None:
            setprefix = SETPREFIX
        if resdir is None:
            resdir = RESDIR
    if not os.path.exists(resdir):
        print >> sys.stderr, "reservoir directory %s doesn't exist" % resdir
        return 1
    res = os.listdir(resdir)
    dirs = glob.glob(setprefix + "*")
    if not dirs:
        print >> sys.stderr, "no directories starting with", setprefix, "exist."
        return 1
    stuff = []
    n = len(res)    # total number of all files
    for d in dirs:
        fs = os.listdir(d)
        n += len(fs)
        stuff.append((d, fs))
    if nperdir * len(dirs) > n:
        print >> sys.stderr, "not enough files to go around - use lower -n."
        return 1
    if ((setprefix.find("Ham") >= 0 and resdir.find("Spam") >= 0) or
        (setprefix.find("Spam") >= 0 and resdir.find("Ham") >= 0)):
        yn = raw_input("Reservoir and Set dirs appear not to match. "
                       "Continue? (y/n) ")
        if yn.lower()[0:1] != 'y':
            return 1
    for (d, fs) in stuff:
        if len(fs) <= nperdir:
            continue
        random.shuffle(fs)
        movethese = fs[nperdir:]
        del fs[nperdir:]
        if dryrun:
            print "would move", len(movethese), "files from", d, \
                  "to reservoir", resdir
            res.extend(movethese)
        else:
            for f in movethese:
                newname = migrate(os.path.join(d, f), resdir, verbose)
                res.append(newname)
    random.shuffle(res)
    for (d, fs) in stuff:
        assert len(fs) <= nperdir
        if nperdir == len(fs):
            continue
        numtomove = nperdir - len(fs)
        assert 0 < numtomove <= len(res)
        movethese = res[-numtomove:]
        del res[-numtomove:]
        if dryrun:
            print "would move", len(movethese), "files from reservoir", \
                  resdir, "to", d
        else:
            for f in movethese:
                if confirm:
                    print file(os.path.join(resdir, f)).read()
                    ok = raw_input('good enough? ').lower()
                    if not ok.startswith('y'):
                        continue
                migrate(os.path.join(resdir, f), d, verbose)
    return 0
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
