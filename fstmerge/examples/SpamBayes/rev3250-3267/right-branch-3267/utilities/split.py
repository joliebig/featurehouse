"""Split an mbox into two files based on a given percentage.
This script will troll through a Unix mbox file randomly assigning each
message to one of two bins.  The split is based on a given float percentage.
E.g.
    % split.py sourcembox 20 mbox20 mbox80
yields two mbox files, where mbox20 contains approximately 20% of the messages
and mbox80 contains 80% of the messages.  Messages are assigned to each bin
randomly.
Usage: %(programs)s [options] sourcembox percent file1 file2
Options:
    -h / --help
        Print this help message and exit
file1 and file2 are where the output goes.  Approximately percent % of
messages will go to file1 and (100 - percent) % of messages will go to file2.
percent is a floating point number between 1 and 99.  sourcembox is a Unix
mailbox file.  All arguments except -h/--help are required.
"""
import sys
import random
import mailbox
import getopt
from spambayes import mboxutils
program = sys.argv[0]

def usage(code, msg=''):
    print(__doc__, file=sys.stderr)
    if msg:
        print(msg, file=sys.stderr)
    sys.exit(code)

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'h', ['help'])
    except getopt.error as msg:
        usage(1, msg)
    bin1 = bin2 = percentage = mboxfile = None
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            usage(0)
    try:
        mboxfile = args[0]
        percent = float(args[1])
        if not (0 < percent < 100):
            raise ValueError
        percent /= 100.0
        bin1 = args[2]
        bin2 = args[3]
    except IndexError:
        usage(1, 'Not enough arguments')
    except ValueError:
        usage(1, 'Percent argument must be a float between 1.0 and 99.0')
    bin1out = open(bin1, 'wb')
    bin2out = open(bin2, 'wb')
    infp = open(mboxfile, 'rb')
    mbox = mailbox.PortableUnixMailbox(infp, mboxutils.get_message)
    for msg in mbox:
        if random.random() < percent:
            outfp = bin1out
        else:
            outfp = bin2out
        astext = str(msg)
        assert astext.endswith('\n')
        outfp.write(astext)
    outfp.close()
    bin1out.close()
    bin2out.close()

if __name__ == '__main__':
    main()
