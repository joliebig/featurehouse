"""Extract false positive and false negative filenames from timcv.py output.
Usage: %(program)s [options] cv_output(s)
Where:
    -h
        Show usage and exit.
    -i
        Prompt for each message, letting the user correct classification
        mistakes, or remove email.
    -u
        Also print unsures.
    -o section:option:value
        set [section, option] in the options database to value
        Requires spambayes package on the PYTHONPATH.
"""
try:
    True, False
except NameError:
    True, False = 1, 0
import os
import re
import sys
import getopt
def cmpf(a, b):
    ma = re.search(r'(\d+)/(\d+)$', a)
    mb = re.search(r'(\d+)/(\d+)$', b)
    if ma and mb:
        xa, ya = map(int, ma.groups())
        xb, yb = map(int, mb.groups())
        return cmp((xa, ya), (xb, yb))
    else:
        return cmp(a, b)
program = sys.argv[0]
def usage(code, msg=''):
    """Print usage message and sys.exit(code)."""
    if msg:
        print >> sys.stderr, msg
        print >> sys.stderr
    print >> sys.stderr, __doc__ % globals()
    sys.exit(code)
def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hiuo:', [])
    except getopt.error, msg:
        usage(1, msg)
    interactive = False
    do_unsures = False
    for opt, arg in opts:
        if opt == '-h':
            usage(0)
        elif opt == '-i':
            interactive = True
        elif opt == '-u':
            do_unsures = True
        elif opt in ('-o', '--option'):
            from spambayes.Options import options
            options.set_from_cmdline(arg, sys.stderr)
    for name in args:
        try:
            f = open(name + ".txt")
        except IOError:
            f = open(name)
        print "===", name, "==="
        fp = []
        fn = []
        unsures = []
        for line in f:
            if line.startswith('    new fp: '):
                fp.extend(eval(line[12:]))
            elif line.startswith('    new fn: '):
                fn.extend(eval(line[12:]))
            elif line.startswith('    new unsure: '):
                unsures.extend(eval(line[16:]))
        fp.sort(cmpf)
        fn.sort(cmpf)
        unsures.sort(cmpf)
        print "--- fp ---"
        for x in fp:
            if interactive and os.path.exists(x):
                print open(x).read()
                answer = raw_input('(S)pam, (R)emove or (L)eave : ').lower()
                if answer == 's':
                    os.rename(x, os.path.join("Data", "Spam", "reservoir",
                                              os.path.basename(x)))
                elif answer == 'r':
                    os.remove(x)
                elif answer == 'l':
                    pass
                else:
                    print "Unknown answer. Left."
            else:
                print x
        print "--- fn ---"
        for x in fn:
            if interactive and os.path.exists(x):
                print open(x).read()
                answer = raw_input('(H)am, (R)emove or (L)eave : ').lower()
                if answer == 'h':
                    os.rename(x, os.path.join("Data", "Ham", "reservoir",
                                              os.path.basename(x)))
                elif answer == 'r':
                    os.remove(x)
                elif answer == 'l':
                    pass
                else:
                    print "Unknown answer. Left."
            else:
                print x
        if do_unsures:
            print "--- unsure ---"
            for x in unsures:
                if interactive and os.path.exists(x):
                    print open(x).read()
                    print x
                    answer = raw_input('(H)am, (S)pam, (R)emove or (L)eave : ').lower()
                    if answer == 'h':
                        os.rename(x, os.path.join("Data", "Ham", "reservoir",
                                                  os.path.basename(x)))
                    elif answer == 's':
                        os.rename(x, os.path.join("Data", "Spam", "reservoir",
                                                  os.path.basename(x)))
                    elif answer == 'r':
                        os.remove(x)
                    elif answer == 'l':
                        pass
                    else:
                        print "Unknown answer. Left."
                else:
                    print x
if __name__ == '__main__':
    main()
