"""Usage: %(program)s [OPTIONS] ...
Where OPTIONS is one or more of:
    -h
        show usage and exit
    -d DBNAME
        use the DBM store.  A DBM file is larger than the pickle and
        creating it is slower, but loading it is much faster,
        especially for large word databases.  Recommended for use with
        hammiefilter or any procmail-based filter.
    -D DBNAME
        use the pickle store.  A pickle is smaller and faster to create,
        but much slower to load.  Recommended for use with pop3proxy and
        hammiesrv.
    -e PATH
        directory of all messages (both ham and spam).
    -s PATH
        directory of known spam messages to train on.  These should be
        duplicates of messages in the everything folder.  Can be
        specified more than once.
    -f
        force training, ignoring the trained header.  Use this if you
        need to rebuild your database from scratch.
    -q
        quiet mode; no output
"""
import getopt
import sys
import os
import re
import time
import filecmp
from spambayes import mboxutils, hammie
program = sys.argv[0]
loud = True
day = 24 * 60 * 60
expire = 4 * 30
grouping = 2
def usage(code, msg=''):
    """Print usage message and sys.exit(code)."""
    if msg:
        print >> sys.stderr, msg
        print >> sys.stderr
    print >> sys.stderr, __doc__ % globals()
    sys.exit(code)
def row(value, spamday, hamday, unsureday):
    line = "%5d|" % value
    for j in range(((expire) // grouping) - 1, -1, -1):
        spamv = 0
        hamv = 0
        unsurev = 0
        for k in range(j * grouping, (j + 1) * grouping):
            try:
                spamv += spamday[k]
                hamv += hamday[k]
                unsurev += unsureday[k]
            except:
                pass
        spamv = spamv // grouping
        hamv = hamv // grouping
        unsurev = unsurev // grouping
        count = 0
        char = ' '
        if spamv >= value:
            count += 1
            char = 's'
        if hamv >= value:
            count += 1
            if (char == ' ' or hamv < spamv):
                char = 'h'
        if unsurev >= value:
            count += 1
            if (char == ' ' or
                (char == 's' and unsurev < spamv) or
                (char == 'h' and unsurev < hamv)):
                char = 'u'
        if count > 1:
            char = char.upper()
        line += char
    return line
def legend():
    line = " " * 60
    now = time.mktime(time.strptime(time.strftime("%d %b %Y"), "%d %b %Y"))
    date = time.mktime(time.strptime(time.strftime("1 %b %Y"), "%d %b %Y"))
    age = int(59 - ((now - date) // day // grouping))
    if age >= 55:
        line = line[:age] + time.strftime("| %b")
    else:
        line = line[:(age)] + "|" + line[(age+1):]
        center = int((age + 59) // 2)
        line = line[:center] + time.strftime("%b") + line[center+3:]
    date = time.mktime(time.strptime(time.strftime("1 %b %Y", time.localtime(date - day * 2)), "%d %b %Y"))
    newage = int(59 - ((now - date) // day // grouping))
    while newage >= 0:
        line = line[:newage] + "|" + line[newage+1:]
        center = int((age + newage) // 2)
        line = line[:center] + time.strftime("%b", time.localtime(date)) + line[center+3:]
        age = newage
        date = time.mktime(time.strptime(time.strftime("1 %b %Y", time.localtime(date - day * 2)), "%d %b %Y"))
        newage = int(59 - ((now - date) // day // grouping))
    if age >= 4:
        center = int((age) // 2)
        line = line[:center-2] + time.strftime("%b", time.localtime(date)) + line[center+1:]
    return line
def main():
    """Main program; parse options and go."""
    global loud
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hfqd:D:s:e:')
    except getopt.error, msg:
        usage(2, msg)
    if not opts:
        usage(2, "No options given")
    pck = None
    usedb = None
    force = False
    everything = None
    spam = []
    for opt, arg in opts:
        if opt == '-h':
            usage(0)
        elif opt == "-f":
            force = True
        elif opt == "-q":
            loud = False
        elif opt == '-e':
            everything = arg
        elif opt == '-s':
            spam.append(arg)
        elif opt == "-d":
            usedb = True
            pck = arg
        elif opt == "-D":
            usedb = False
            pck = arg
    if args:
        usage(2, "Positional arguments not allowed")
    if usedb == None:
        usage(2, "Must specify one of -d or -D")
    h = hammie.open(pck, usedb, "c")
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
    skipcount = 0
    spamcount = 0
    hamcount = 0
    spamday = [0] * expire
    hamday = [0] * expire
    unsureday = [0] * expire
    date_re = re.compile(
        r";.* (\d{1,2} (?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \d{2,4})")
    now = time.mktime(time.strptime(time.strftime("%d %b %Y"), "%d %b %Y"))
    if loud: print "Scanning everything"
    for f in os.listdir(everything):
        if f[0] in ('1', '2', '3', '4', '5', '6', '7', '8', '9'):
            name = os.path.join(everything, f)
            fh = file(name, "rb")
            msg = mboxutils.get_message(fh)
            fh.close()
            age = 2 * expire
            try:
                received = (msg.get_all("Received"))[0]
                received = date_re.search(received).group(1)
                date = time.mktime(time.strptime(received, "%d %b %Y"))
                age = (now - date) // day
                if age < 0:
                    age = 2 * expire
            except:
                pass
            if age >= expire:
                skipcount += 1
                if loud and not (skipcount % 100):
                    sys.stdout.write("-")
                    sys.stdout.flush()
                continue
            age = int(age)
            try:
                if msg.get("X-Spambayes-Classification").find("unsure") >= 0:
                    unsureday[age] += 1
            except:
                pass
            size = os.stat(name).st_size
            isspam = False
            try:
                for s in spamsizes[size]:
                    if filecmp.cmp(name, s):
                        isspam = True
            except KeyError:
                pass
            if isspam:
                spamcount += 1
                spamday[age] += 1
                if loud and not (spamcount % 100):
                    sys.stdout.write("s")
                    sys.stdout.flush()
            else:
                hamcount += 1
                hamday[age] += 1
                if loud and not (hamcount % 100):
                    sys.stdout.write("h")
                    sys.stdout.flush()
            h.train(msg, isspam)
    if loud:
        print
        mval = max(max(spamday), max(hamday), max(unsureday))
        scale = (mval + 19) // 20
        print "%5d" % mval
        for j in range(19, -1, -1):
            print row(scale * j, spamday, hamday, unsureday)
        print "     +" + ('-' * 60)
        print "      " + legend()
        print
        print "Total: %d ham, %d spam (%.2f%% spam)" % (
            hamcount, spamcount, spamcount * 100.0 / (hamcount + spamcount))
    h.store()
if __name__ == "__main__":
    main()
