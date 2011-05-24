"""Usage: %(program)s  [options] -c command
Where:
    -h
        Show usage and exit.
    -c command
        The command to be run, with all its options.
        The last line of output from this program should
        match 'YYYYYYY cost: $xxxx.xx'
        (i.e. the third word of the last line should be the value to be
         minimized, preceded by a dollar sign)
        I have used
         "python2.3 timcv.py -n 10 --spam-keep=600 --ham-keep=600 -s 12345"
This program will overwrite bayescustomize.ini!
"""
import sys
def usage(code, msg=''):
    """Print usage message and sys.exit(code)."""
    if msg:
        print >> sys.stderr, msg
        print >> sys.stderr
    print >> sys.stderr, __doc__ % globals()
    sys.exit(code)
program = sys.argv[0]
from spambayes import Options
start = (Options.options["Tokenizer", "unknown_word_prob"],
         Options.options["Tokenzier", "minimum_prob_strength"],
         Options.options["Tokenizer", "unknown_word_strength"])
err = (0.01, 0.01, 0.01)
def mkini(vars):
    f=open('bayescustomize.ini', 'w')
    f.write("""
[Classifier]
unknown_word_prob = %.6f
minimum_prob_strength = %.6f
unknown_word_strength = %.6f
"""%tuple(vars))
    f.close()
def score(vars):
    import os
    mkini(vars)
    status = os.system('%s > loop.out'%command)
    if status != 0:
        print >> sys.stderr, "Error status from subcommand"
        sys.exit(status)
    f = open('loop.out', 'r')
    txt = f.readlines()
    cost = float(txt[-1].split()[2][1:])
    f.close()
    os.rename('loop.out','loop.out.old')
    print ''.join(txt[-20:])[:-1]
    print "x=%.4f p=%.4f s=%.4f %.2f"%(tuple(vars)+(cost,))
    sys.stdout.flush()
    return -cost
def main():
    import spambayes.optimize
    finish=spambayes.optimize.SimplexMaximize(start,err,score)
    mkini(finish)
    print "Best result left in bayescustomize.ini"
if __name__ == "__main__":
    import getopt
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hc:')
    except getopt.error, msg:
        usage(1, msg)
    command = None
    for opt, arg in opts:
        if opt == '-h':
            usage(0)
        elif opt == '-c':
            command = arg
    if args:
        usage(1, "Positional arguments not supported")
    if command is None:
        usage(1, "-c is required")
    main()
