"""
Demonstration of n-way classification possibilities.
Usage: %(prog)s [ -h ] tag=db ...
-h - print this message and exit.
All args are of the form 'tag=db' where 'tag' is the tag to be given in the
X-Spambayes-Classification: header.  A single message is read from stdin and
a modified message sent to stdout.  The message is compared against each
database in turn.  If its score exceeds the spam threshold when scored
against a particular database, an X-Spambayes-Classification header is added
and the modified message is written to stdout.  If none of the comparisons
yields a definite classification, the message is written with an
'X-Spambayes-Classification: unsure' header.
Training is left up to the user.  In general, you want to train so that a
message in a particular category will score above your spam threshold when
checked against that category's training database.  For example, suppose you
have the following mbox formatted files: python, music, family, cars.  If
you wanted to create a training database for each of them you could execute
this series of mboxtrain.py commands:
    sb_mboxtrain.py -f -d python.db -s python -g music -g family -g cars
    sb_mboxtrain.py -f -d music.db  -g python -s music -g family -g cars
    sb_mboxtrain.py -f -d family.db -g python -g music -s family -g cars
    sb_mboxtrain.py -f -d cars.db   -g python -g music -g family -s cars
You'd then compare messages using a %(prog)s command like this:
    %(prog)s python=python.db music=music.db family=family.db cars=cars.db
Normal usage (at least as I envisioned it) would be to run the program via
procmail or something similar.  You'd then have a .procmailrc file which
looked something like this:
    :0 fw:sb.lock
    | $(prog)s spam=spam.db python=python.db music=music.db ...
    :0
    * ^X-Spambayes-Classification: spam
    spam
    :0
    * ^X-Spambayes-Classification: python
    python
    :0
    * ^X-Spambayes-Classification: music
    music
    ...
    :0
    * ^X-Spambayes-Classification: unsure
    unsure
Note that I've not tried this (yet).  It should simplify the logic in a
.procmailrc file and probably classify messages better than writing more
convoluted procmail rules.
"""
import getopt
import sys
import os
from spambayes import hammie, mboxutils, Options
prog = os.path.basename(sys.argv[0])
def usage():
    print >> sys.stderr, __doc__ % globals()
def main(args):
    opts, args = getopt.getopt(args, "h")
    for opt, arg in opts:
        if opt == '-h':
            usage()
            return 0
    msg = mboxutils.get_message(sys.stdin)
    try:
        del msg["X-Spambayes-Classification"]
    except KeyError:
        pass
    for pair in args:
        tag, db = pair.split('=', 1)
        h = hammie.open(db, True, 'r')
        score = h.score(msg)
        if score >= Options.options["Categorization", "spam_cutoff"]:
            msg["X-Spambayes-Classification"] = "%s; %.2f" % (tag, score)
            break
    else:
        msg["X-Spambayes-Classification"] = "unsure"
    sys.stdout.write(msg.as_string(unixfrom=(msg.get_unixfrom()
                                             is not None)))
    return 0
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
