"""
rates.py basename ...
Assuming that file
    basename + '.txt'
or
    basename
contains output from one of the test drivers (timcv, mboxtest, timtest),
scans that file for summary statistics, displays them to stdout, and also
writes them to file
    basename + 's.txt'
(where the 's' means 'summary').  This doesn't need a full output file
from a test run, and will display stuff for as far as the output file
has gotten so far.
Two of these summary files can later be fed to cmp.py.
"""

import sys

"""
-> Training on Data/Ham/Set2-3 & Data/Spam/Set2-3 ... 8000 hams & 5500 spams
-> Predicting Data/Ham/Set1 & Data/Spam/Set1 ...
-> <stat> tested 4000 hams & 2750 spams against 8000 hams & 5500 spams
-> <stat> false positive %: 0.025
-> <stat> false negative %: 0.327272727273
-> <stat> 1 new false positives
"""

def doit(basename):

    if basename.endswith('.txt'):

        basename = basename[:-4]

    try:

        ifile = file(basename + '.txt')

    except IOError:

        ifile = file(basename)

    interesting = [line for line in ifile if line.startswith('-> ')]

    ifile.close()

    oname = basename + 's.txt'

    ofile = file(oname, 'w')

    print(basename, '->', oname)

    def dump(*stuff):

        msg = ' '.join(map(str, stuff))

        print(msg)

        print(msg, file=ofile)

    ntests = nfn = nfp = 0

    sumfnrate = sumfprate = 0.0

    for line in interesting:

        dump(line[:-1])

        fields = line.split()

        if line.startswith('-> <stat> tested '):

            ntests += 1

            continue

        if line.startswith('-> <stat> false '):

            kind = fields[3]

            percent = float(fields[-1])

            if kind == 'positive':

                sumfprate += percent

                lastval = percent

            else:

                sumfnrate += percent

                dump('    %7.3f %7.3f' % (lastval, percent))

            continue

        if len(fields) >= 5 and fields[3] == 'new' and fields[4] == 'false':

            kind = fields[-1]

            count = int(fields[2])

            if kind == 'positives':

                nfp += count

            else:

                nfn += count

    dump('total unique false pos', nfp)

    dump('total unique false neg', nfn)

    dump('average fp %', sumfprate / ntests)

    dump('average fn %', sumfnrate / ntests)
 for name in sys.argv[1:]:

    doit(name)

 for name in sys.argv[1:]:

    doit(name)



