"""
table.py [-m] base1 base2 ... baseN
Combines output from base1.txt, base2.txt, etc., which are created by
the TestDriver (such as timcv.py) output, and displays tabulated
comparison statistics to stdout.  Each input file is represented by
one column in the table.
Optional argument -m shows a final column with the mean value of each
statistic.
"""

def suck(f):

    hamdevall = spamdevall = (0.0, 0.0)

    cost = 0.0

    bestcost = 0.0

    fp = 0

    fn = 0

    un = 0

    fpp = 0.0

    fnp = 0.0

    unp = 0.0

    htest = 0

    stest = 0

    get = f.readline

    while 1:

        line = get()

        if line.startswith('-> <stat> tested'):

            print line,

        elif line.find(' items; mean ') > 0 and line.find('for all runs') > 0:

            vals = line.split(';')

            mean = float(vals[1].split()[-1])

            sdev = float(vals[2].split()[-1])

            val = (mean, sdev)

            ntested = int(vals[0].split()[-2])

            typ = vals[0].split()[2]

            if line.find('for all runs') != -1:

                if typ == 'Ham':

                    hamdevall = val

                    htest = ntested

                else:

                    spamdevall = val

                    stest = ntested

        elif line.startswith('-> best cost for all runs: $'):

            bestcost = float(line.split('$')[-1])

        elif line.startswith('-> <stat> all runs false positives: '):

            fp = int(line.split()[-1])

        elif line.startswith('-> <stat> all runs false negatives: '):

            fn = int(line.split()[-1])

        elif line.startswith('-> <stat> all runs unsure: '):

            un = int(line.split()[-1])

        elif line.startswith('-> <stat> all runs false positive %: '):

            fpp = float(line.split()[-1])

        elif line.startswith('-> <stat> all runs false negative %: '):

            fnp = float(line.split()[-1])

        elif line.startswith('-> <stat> all runs unsure %: '):

            unp = float(line.split()[-1])

        elif line.startswith('-> <stat> all runs cost: '):

            cost = float(line.split('$')[-1])

            break

    return (htest, stest, fp, fn, un, fpp, fnp, unp, cost, bestcost,
            hamdevall, spamdevall)
 def windowsfy(fn):

    import os

    if os.path.exists(fn + '.txt'):

        return fn + '.txt'

    else:

        return fn
 def table():

    import getopt, sys

    showMean = 0

    fname = "filename: "

    fnam2 = "          "

    ratio = "ham:spam: "

    rat2  = "          "

    fptot = "fp total: "

    fpper = "fp %:     "

    fntot = "fn total: "

    fnper = "fn %:     "

    untot = "unsure t: "

    unper = "unsure %: "

    rcost = "real cost:"

    bcost = "best cost:"

    hmean = "h mean:   "

    hsdev = "h sdev:   "

    smean = "s mean:   "

    ssdev = "s sdev:   "

    meand = "mean diff:"

    kval  = "k:        "

    tfptot = tfpper = tfntot = tfnper = tuntot = tunper = trcost = tbcost = \
    thmean = thsdev = tsmean = tssdev = tmeand = tkval =  0

    args, fileargs = getopt.getopt(sys.argv[1:], 'm')

    for arg, val in args:

        if arg == "-m":

            showMean = 1

    for filename in fileargs:

        filename = windowsfy(filename)

        (htest, stest, fp, fn, un, fpp, fnp, unp, cost, bestcost,
         hamdevall, spamdevall) = suck(file(filename))

        if filename.endswith('.txt'):

            filename = filename[:-4]

        filename = filename[filename.rfind('/')+1:]

        filename = filename[filename.rfind("\\")+1:]

        if len(fname) > len(fnam2):

            fname += "            "

            fname = fname[0:(len(fnam2) + 12)]

            fnam2 += " %11s" % filename

        else:

            fnam2 += "            "

            fnam2 = fnam2[0:(len(fname) + 12)]

            fname += " %11s" % filename

        if len(ratio) > len(rat2):

            ratio += "            "

            ratio = ratio[0:(len(rat2) + 12)]

            rat2  += " %11s" % ("%d:%d" % (htest, stest))

        else:

            rat2  += "            "

            rat2  = rat2[0:(len(ratio) + 12)]

            ratio += " %11s" % ("%d:%d" % (htest, stest))

        fptot += "%12d"   % fp

        tfptot += fp

        fpper += "%12.2f" % fpp

        tfpper += fpp

        fntot += "%12d"   % fn

        tfntot += fn

        fnper += "%12.2f" % fnp

        tfnper += fnp

        untot += "%12d"   % un

        tuntot += un

        unper += "%12.2f" % unp

        tunper += unp

        rcost += "%12s"   % ("$%.2f" % cost)

        trcost += cost

        bcost += "%12s"   % ("$%.2f" % bestcost)

        tbcost += bestcost

        hmean += "%12.2f" % hamdevall[0]

        thmean += hamdevall[0]

        hsdev += "%12.2f" % hamdevall[1]

        thsdev += hamdevall[1]

        smean += "%12.2f" % spamdevall[0]

        tsmean += spamdevall[0]

        ssdev += "%12.2f" % spamdevall[1]

        tssdev += spamdevall[1]

        meand += "%12.2f" % (spamdevall[0] - hamdevall[0])

        tmeand += (spamdevall[0] - hamdevall[0])

        k = (spamdevall[0] - hamdevall[0]) / (spamdevall[1] + hamdevall[1])

        kval  += "%12.2f" % k

        tkval  += k

    nfiles = len(fileargs)

    if nfiles and showMean:

        fptot += "%12d"   % (tfptot/nfiles)

        fpper += "%12.2f" % (tfpper/nfiles)

        fntot += "%12d"   % (tfntot/nfiles)

        fnper += "%12.2f" % (tfnper/nfiles)

        untot += "%12d"   % (tuntot/nfiles)

        unper += "%12.2f" % (tunper/nfiles)

        rcost += "%12s"   % ("$%.2f" % (trcost/nfiles))

        bcost += "%12s"   % ("$%.2f" % (tbcost/nfiles))

        hmean += "%12.2f" % (thmean/nfiles)

        hsdev += "%12.2f" % (thsdev/nfiles)

        smean += "%12.2f" % (tsmean/nfiles)

        ssdev += "%12.2f" % (tssdev/nfiles)

        meand += "%12.2f" % (tmeand/nfiles)

        kval  += "%12.2f" % (tkval/nfiles)

    print fname

    if len(fnam2.strip()) > 0:

        print fnam2

    print ratio

    if len(rat2.strip()) > 0:

        print rat2

    print fptot

    print fpper

    print fntot

    print fnper

    print untot

    print unper

    print rcost

    print bcost

    print hmean

    print hsdev

    print smean

    print ssdev

    print meand

    print kval
 if __name__ == "__main__":

    table()

 if __name__ == "__main__":

    table()



