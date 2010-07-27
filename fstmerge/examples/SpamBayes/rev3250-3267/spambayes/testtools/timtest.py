"""Usage: %(program)s  [options] -n nsets
Where:
    -h
        Show usage and exit.
    -n int
        Number of Set directories (Data/Spam/Set1, ... and Data/Ham/Set1, ...).
        This is required.
If you only want to use some of the messages in each set,
    --ham-keep int
        The maximum number of msgs to use from each Ham set.  The msgs are
        chosen randomly.  See also the -s option.
    --spam-keep int
        The maximum number of msgs to use from each Spam set.  The msgs are
        chosen randomly.  See also the -s option.
    -s int
        A seed for the random number generator.  Has no effect unless
        at least one of {--ham-keep, --spam-keep} is specified.  If -s
        isn't specifed, the seed is taken from current time.
In addition, an attempt is made to merge bayescustomize.ini into the options.
If that exists, it can be used to change the settings in Options.options.
"""

import os

import sys

sys.path.insert(-1, os.getcwd())

sys.path.insert(-1, os.path.dirname(os.getcwd()))

from spambayes.Options import options, get_pathname_option

from spambayes import TestDriver

from spambayes import msgs

program = sys.argv[0]

def usage(code, msg=''):

    """Print usage message and sys.exit(code)."""

    if msg:

        print(msg, file=sys.stderr)

        print(file=sys.stderr)

    print(__doc__ % globals(), file=sys.stderr)

    sys.exit(code)
 def drive(nsets):

    print(options.display())

    spamdirs = [get_pathname_option("TestDriver", "spam_directories") % \
                i for i in range(1, nsets+1)]

    hamdirs  = [get_pathname_option("TestDriver", "ham_directories") % \
                i for i in range(1, nsets+1)]

    spamhamdirs = list(zip(spamdirs, hamdirs))

    d = TestDriver.Driver()

    for spamdir, hamdir in spamhamdirs:

        d.new_classifier()

        d.train(msgs.HamStream(hamdir, [hamdir]),
                msgs.SpamStream(spamdir, [spamdir]))

        for sd2, hd2 in spamhamdirs:

            if (sd2, hd2) == (spamdir, hamdir):

                continue

            d.test(msgs.HamStream(hd2, [hd2]),
                   msgs.SpamStream(sd2, [sd2]))

        d.finishtest()

    d.alldone()
 def main():

    import getopt

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'hn:s:',
                                   ['ham-keep=', 'spam-keep='])

    except getopt.error as msg:

        usage(1, msg)

    nsets = seed = hamkeep = spamkeep = None

    for opt, arg in opts:

        if opt == '-h':

            usage(0)

        elif opt == '-n':

            nsets = int(arg)

        elif opt == '-s':

            seed = int(arg)

        elif opt == '--ham-keep':

            hamkeep = int(arg)

        elif opt == '--spam-keep':

            spamkeep = int(arg)

    if args:

        usage(1, "Positional arguments not supported")

    if nsets is None:

        usage(1, "-n is required")

    msgs.setparms(hamkeep, spamkeep, seed=seed)

    drive(nsets)
 if __name__ == "__main__":

    main()

 if __name__ == "__main__":

    main()





