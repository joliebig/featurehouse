"""
cmp.py sbase1 sbase2
Combines output from sbase1.txt and sbase2.txt, which are created by
rates.py from timtest.py output, and displays comparison statistics to
stdout.
"""
import sys
f1n, f2n = sys.argv[1:3]
def suck(f):
    fns = []
    fps = []
    hamdev = []
    spamdev = []
    hamdevall = spamdevall = (0.0, 0.0)
    get = f.readline
    while 1:
        line = get()
        if line.startswith('-> <stat> tested'):
            print line,
        if line.find(' items; mean ') != -1:
            vals = line.split(';')
            mean = float(vals[1].split()[-1])
            sdev = float(vals[2].split()[-1])
            val = (mean, sdev)
            typ = vals[0].split()[2]
            if line.find('for all runs') != -1:
                if typ == 'Ham':
                    hamdevall = val
                else:
                    spamdevall = val
            elif line.find('all in this') != -1:
                if typ == 'Ham':
                    hamdev.append(val)
                else:
                    spamdev.append(val)
            continue
        if line.startswith('-> '):
            continue
        if line.startswith('total'):
            break
        p, n = map(float, line.split())
        fps.append(p)
        fns.append(n)
    fptot = int(line.split()[-1])
    fntot = int(get().split()[-1])
    fpmean = float(get().split()[-1])
    fnmean = float(get().split()[-1])
    return (fps, fns, fptot, fntot, fpmean, fnmean,
            hamdev, spamdev, hamdevall, spamdevall)
def tag(p1, p2):
    if p1 == p2:
        t = "tied          "
    else:
        t = p1 < p2 and "lost " or "won  "
        if p1:
            p = (p2 - p1) * 100.0 / p1
            t += " %+7.2f%%" % p
        else:
            t += " +(was 0)"
    return t
def mtag(m1, m2):
    mean1, dev1 = m1
    mean2, dev2 = m2
    t = "%7.2f %7.2f " % (mean1, mean2)
    if mean1:
        mp = (mean2 - mean1) * 100.0 / mean1
        t += "%+7.2f%%" % mp
    else:
        t += "+(was 0)"
    t += "     %7.2f %7.2f " % (dev1, dev2)
    if dev1:
        dp = (dev2 - dev1) * 100.0 / dev1
        t += "%+7.2f%%" % dp
    else:
        t += "+(was 0)"
    return t
def dump(p1s, p2s):
    alltags = ""
    for p1, p2 in zip(p1s, p2s):
        t = tag(p1, p2)
        print "    %5.3f  %5.3f  %s" % (p1, p2, t)
        alltags += t + " "
    print
    for t in "won", "tied", "lost":
        print "%-4s %2d times" % (t, alltags.count(t))
    print
def dumpdev(meandev1, meandev2):
    for m1, m2 in zip(meandev1, meandev2):
        print mtag(m1, m2)
def windowsfy(fn):
    import os
    if os.path.exists(fn + '.txt'):
        return fn + '.txt'
    else:
        return fn
print f1n, '->', f2n
f1n = windowsfy(f1n)
f2n = windowsfy(f2n)
(fp1, fn1, fptot1, fntot1, fpmean1, fnmean1,
 hamdev1, spamdev1, hamdevall1, spamdevall1) = suck(file(f1n))
(fp2, fn2, fptot2, fntot2, fpmean2, fnmean2,
 hamdev2, spamdev2, hamdevall2, spamdevall2) = suck(file(f2n))
print
print "false positive percentages"
dump(fp1, fp2)
print "total unique fp went from", fptot1, "to", fptot2, tag(fptot1, fptot2)
print "mean fp % went from", fpmean1, "to", fpmean2, tag(fpmean1, fpmean2)
print
print "false negative percentages"
dump(fn1, fn2)
print "total unique fn went from", fntot1, "to", fntot2, tag(fntot1, fntot2)
print "mean fn % went from", fnmean1, "to", fnmean2, tag(fnmean1, fnmean2)
print
if len(hamdev1) == len(hamdev2) and len(spamdev1) == len(spamdev2):
    print "ham mean                     ham sdev"
    dumpdev(hamdev1, hamdev2)
    print
    print "ham mean and sdev for all runs"
    dumpdev([hamdevall1], [hamdevall2])
    print
    print "spam mean                    spam sdev"
    dumpdev(spamdev1, spamdev2)
    print
    print "spam mean and sdev for all runs"
    dumpdev([spamdevall1], [spamdevall2])
    print
    diff1 = spamdevall1[0] - hamdevall1[0]
    diff2 = spamdevall2[0] - hamdevall2[0]
    print "ham/spam mean difference: %2.2f %2.2f %+2.2f" % (diff1,
                                                            diff2,
                                                            diff2 - diff1)
else:
    print "[info about ham & spam means & sdevs not available in both files]"
