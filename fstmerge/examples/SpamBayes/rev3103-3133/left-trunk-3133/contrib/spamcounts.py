"""
Check spamcounts for one or more tokens or patterns
usage %(prog)s [ options ] token ...
-h    - print this documentation and exit.
-r    - treat tokens as regular expressions - may not be used with -t
-t    - read message from stdin, tokenize it, then display their counts
        may not be used with -r
-o section:option:value
      - set [section, option] in the options database to value
"""
from __future__ import division
import sys
import getopt
import re
import sets
import os
import shelve
import pickle
import csv
from spambayes.Options import options, get_pathname_option
from spambayes.tokenizer import tokenize
from spambayes.storage import STATE_KEY, database_type, open_storage
prog = sys.argv[0]
def usage(msg=None):
    if msg is not None:
        print >> sys.stderr, msg
    print >> sys.stderr, __doc__.strip() % globals()
def print_spamcounts(tokens, db, use_re):
    if use_re:
        s = sets.Set()
        keys = db._wordinfokeys()
        for pat in tokens:
            for k in keys:
                if re.search(pat, k) is not None:
                    s.add(k)
        tokens = list(s)
    writer = csv.writer(sys.stdout)
    writer.writerow(("token", "nspam", "nham", "spam prob"))
    seen = sets.Set()
    for t in tokens:
        if t in seen:
            continue
        seen.add(t)
        sc, hc = db._wordinfoget(t).__getstate__()
        if sc == hc == 0:
            continue
        sp = db.spamprob([t])
        writer.writerow((t, sc, hc, sp))
def main(args):
    try:
        opts, args = getopt.getopt(args, "hrto:",
                                   ["help", "re", "tokenize", "option="])
    except getopt.GetoptError, msg:
        usage(msg)
        return 1
    usere = False
    tokenizestdin = False
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            usage()
            return 0
        elif opt in ("-r", "--re"):
            usere = True
        elif opt in ("-t", "--tokenize"):
            tokenizestdin = True
        elif opt in ('-o', '--option'):
            options.set_from_cmdline(arg, sys.stderr)
    if usere and tokenizestdin:
        usage("-r and -t may not be used at the same time")
        return 1
    dbname, usedb = database_type(opts)
    db = open_storage(dbname, usedb)
    if tokenizestdin:
        args = tokenize(sys.stdin)
    if args:
        print_spamcounts(args, db, usere)
        return 0
    else:
        usage("need tokens on cmd line or -t w/ msg on stdin")
        return 1
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
