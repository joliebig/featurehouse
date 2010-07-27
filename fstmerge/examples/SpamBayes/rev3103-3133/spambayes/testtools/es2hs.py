"""Usage: %(program)s [OPTIONS] ...
Where OPTIONS is one or more of:
    -h
        show usage and exit
    -e PATH
        directory of all messages (ham and spam).
    -s PATH
        directory of known spam messages.  These should be duplicates
        of messages in the everything folder.  Can be specified more
        than once.
"""

import getopt

import sys

import os

import filecmp

import shutil

program = sys.argv[0]

loud = True

day = 24 * 60 * 60

expire = 4 * 30

grouping = 2

hamdir = "Data/Ham/reservoir"

spamdir = "Data/Spam/reservoir"

def usage(code, msg=''):

    """Print usage message and sys.exit(code)."""

    if msg:

        print >> sys.stderr, msg

        print >> sys.stderr

    print >> sys.stderr, __doc__ % globals()

    sys.exit(code)
 def main():

    """Main program; parse options and go."""

    global loud

    everything = None

    spam = []

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'hs:e:')

    except getopt.error, msg:

        usage(2, msg)

    if opts:

        for opt, arg in opts:

            if opt == '-h':

                usage(0)

            elif opt == '-e':

                everything = arg

            elif opt == '-s':

                spam.append(arg)

        if args:

            usage(2, "Positional arguments not allowed")

    else:

        everything = os.path.expanduser("~/Mail/everything")

        spam = [os.path.expanduser("~/Mail/spam"),
                os.path.expanduser("~/Mail/newspam")]

    spamsizes = {}

    for s in spam:

        if loud: print "Scanning spamdir (%s):" % s

        files = os.listdir(s)

        for f in files:

            if f[0] in ('1', '2', '3', '4', '5', '6', '7', '8', '9'):

                name = os.path.join(s, f)

                size = os.stat(name).st_size

                try:

                    spamsizes[size].append(name)

                except KeyError:

                    spamsizes[size] = [name]

    os.makedirs(spamdir)

    os.makedirs(hamdir)

    if loud: print "Scanning everything"

    for f in os.listdir(everything):

        if f[0] in ('1', '2', '3', '4', '5', '6', '7', '8', '9'):

            name = os.path.join(everything, f)

            size = os.stat(name).st_size

            isspam = False

            try:

                for s in spamsizes[size]:

                    if filecmp.cmp(name, s):

                        isspam = True

            except KeyError:

                pass

            if isspam:

                shutil.copyfile(name, os.path.join(spamdir, f))

            else:

                shutil.copyfile(name, os.path.join(hamdir, f))
 if __name__ == "__main__":

    main()

 if __name__ == "__main__":

    main()



