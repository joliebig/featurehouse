"""
Train to exhaustion: train repeatedly on a pile of ham and spam until
everything scores properly.
usage %(prog)s [ -h ] -g file -s file [ -d file | -p file ] \
               [ -m N ] [ -r N ] [ -c ext ] [ -o sect:opt:val ] [ -v ]
-h      - Print this usage message and exit.
-g file - Take ham from file.
-s file - Take spam from file.
-d file - Use a database-based classifier named file.
-p file - Use a pickle-based classifier named file.
-m N    - Train on at most N messages (nham == N/2 and nspam == N/2).
-r N    - Run at most N rounds (default %(MAXROUNDS)s), even if not
          all messages score correctly.
-c ext  - Cull all messages which aren't used as training input during any
          run and write to new ham and spam files with ext as an extra file
          extension.  All messages which are never considered (because one
          training set is longer than the other or the -m flag was used to
          reduce the amount of input) are retained.
-C      - Cull all messages which aren't used as training input during any
          run by marking them deleted.  Only works with IMAP folders.
-o sect:opt:val -
          Set [sect, opt] in the options database to val.
-v        Be very verbose, spewing all sorts of stuff out to stderr.
-R        Walk backwards through the mailbox.
--ratio=n Define the ratio of spam:ham messages to be trained at once.
          The default is 1:1, but given the sorry state of the Net's email
          infrastructure these days you'll probably want to raise it (3:2 or
          2:1, etc).  Keep it as close to 1 as you can...
Note: The -c command line argument isn't quite as benign as it might first
appear.  Since the tte protocol trains on the same number of ham and spam
messages, if you use the output of one run as input into a later run you
will almost certainly train on fewer messages than before since the two
files will probably not have the same number of messages.  The extra
messages in the longer file will be ignored in future runs until you add
more messages to the shorter file.
Note: Adding messages which train correctly won't affect anything other than
adding more ham or spam to the respective training pile.  To force such
messages to have an effect you should set your ham_cutoff and spam_cutoff
values closer to 0.0 and 1.0 than your normal settings during scoring.  For
example, if your normal ham_cutoff and spam_cutoff values are 0.2 and 0.8,
you might run %(prog)s like
    %(prog)s -o Categorization:ham_cutoff:0.05 \
        -o Categorization:spam_cutoff:0.95 \
        [ other args ]
For more detail on the notion of training to exhaustion see Gary Robinson's
blog:
    http://www.garyrobinson.net/2004/02/spam_filtering_.html
"""

import sys

import getopt

import os

import datetime

from spambayes import storage

from spambayes import Options

from spambayes import mboxutils

from spambayes.tokenizer import tokenize

prog = os.path.basename(sys.argv[0])

MAXROUNDS = 10

def usage(msg=None):

    if msg is not None:

        print(msg, file=sys.stderr)

    print(__doc__.strip() % globals(), file=sys.stderr)
 def train(store, hambox, spambox, maxmsgs, maxrounds, tdict, reverse, verbose,
          ratio):

    round = 0

    ham_cutoff = Options.options["Categorization", "ham_cutoff"]

    spam_cutoff = Options.options["Categorization", "spam_cutoff"]

    hambone_ = list(mboxutils.getmbox(hambox))

    spamcan_ = list(mboxutils.getmbox(spambox))

    if reverse:

        hambone_ = list(reversed(hambone_))

        spamcan_ = list(reversed(spamcan_))

    nspam, nham = len(spamcan_), len(hambone_)

    if ratio:

        rspam, rham = ratio

        if (rspam > rham) == (rspam * nham > rham * nspam):

            rspam, rham = nspam, nham

    ham = 0

    spam = 1

    name = ('ham','spam')

    misses = [0, 0]

    misclassified = lambda is_spam, score: (
        is_spam and score < spam_cutoff or not is_spam and score > ham_cutoff)

    while round < maxrounds and (misses[ham] or misses[spam] or round == 0):

        round += 1

        if verbose:

            print("*** round", round, "***", file=sys.stderr)

        start = datetime.datetime.now()

        hambone = iter(hambone_)

        spamcan = iter(spamcan_)

        i = [0, 0]

        msgs_processed = 0

        misses = [0, 0]

        training_sets = [hambone, spamcan]

        while not maxmsgs or msgs_processed < maxmsgs:

            train_spam = i[ham] * rspam > i[spam] * rham

            try:

                train_msg = next(training_sets[train_spam])

            except StopIteration:

                break

            i[train_spam] += 1

            msgs_processed += 1

            sys.stdout.write("\r%5d" % msgs_processed)

            sys.stdout.flush()

            tokens = list(tokenize(train_msg))

            score = store.spamprob(tokens)

            selector = train_msg["message-id"] or train_msg["subject"]

            if misclassified(train_spam, score) and selector is not None:

                if verbose:

                    print("\tmiss %s: %.6f %s" % (
                        name[train_spam], score, selector), file=sys.stderr)

                misses[train_spam] += 1

                tdict[train_msg["message-id"]] = True

                store.learn(tokens, train_spam)

        delta = datetime.datetime.now()-start

        seconds = delta.seconds + delta.microseconds/1000000

        print("\rround: %2d, msgs: %4d, ham misses: %3d, spam misses: %3d, %.1fs" % \
              (round, msgs_processed, misses[0], misses[1], seconds))

    training_sets = [hambone, spamcan]

    for is_spam in ham, spam:

        nleft = 0

        try:

            while True:

                msg = next(training_sets[is_spam])

                score = store.spamprob(tokenize(msg))

                if misclassified(is_spam, score):

                    tdict[msg["message-id"]] = True

                    nleft += 1

        except StopIteration:

            if nleft:

                print(nleft, "untrained %ss" % name[is_spam])
 def cull(mbox_name, cullext, designation, tdict):

    print("writing new %s mbox..." % designation)

    n = m = 0

    if cullext:

        culled_mbox = file(mbox_name + cullext, "w")

    for msg in mboxutils.getmbox(mbox_name):

        m += 1

        if msg["message-id"] in tdict:

            if cullext:

                culled_mbox.write(str(msg))

            n += 1

        elif not cullext:

            response = msg.imap_server.uid(
                "STORE", msg.uid, "+FLAGS.SILENT", "(\\Deleted \\Seen)")

            command = "set %s to be deleted and seen" % (msg.uid,)

            msg.imap_server.check_response(command, response)

        sys.stdout.write("\r%5d of %5d" % (n, m))

        sys.stdout.flush()

    sys.stdout.write("\n")

    if cullext:

        culled_mbox.close()
 def main(args):

    try:

        opts, args = getopt.getopt(args, "hg:s:d:p:o:m:r:c:vRuC",
                                   ["help", "good=", "spam=",
                                    "database=", "pickle=", "verbose",
                                    "option=", "max=", "maxrounds=",
                                    "cullext=", "cull", "reverse",
                                    "ratio=", "unbalanced"])

    except getopt.GetoptError as msg:

        usage(msg)

        return 1

    ham = spam = dbname = usedb = cullext = None

    maxmsgs = 0

    maxrounds = MAXROUNDS

    verbose = False

    reverse = False

    sh_ratio = (1, 1)

    for opt, arg in opts:

        if opt in ("-h", "--help"):

            usage()

            return 0

        elif opt in ("-v", "--verbose"):

            verbose = True

        elif opt in ("-g", "--good"):

            ham = arg

        elif opt in ("-s", "--spam"):

            spam = arg

        elif opt in ("-c", "--cullext"):

            cullext = arg

        elif opt in ("-C", "--cull"):

            cullext = ''

        elif opt in ("-m", "--max"):

            maxmsgs = int(arg)

        elif opt in ("-r", "--maxrounds"):

            maxrounds = int(arg)

        elif opt in ("-R", "--reverse"):

            reverse = True

        elif opt in ("-u", "--unbalanced"):

            sh_ratio = None

        elif opt in ('-o', '--option'):

            Options.options.set_from_cmdline(arg, sys.stderr)

        elif opt == '--ratio':

            arg = arg.split(":")

            sh_ratio = (int(arg[0]), int(arg[1]))

    if ham is None or spam is None:

        usage("require both ham and spam piles")

        return 1

    dbname, usedb = storage.database_type(opts)

    try:

        os.unlink(dbname)

    except OSError:

        pass

    store = storage.open_storage(dbname, usedb)

    tdict = {}

    train(store, ham, spam, maxmsgs, maxrounds, tdict, reverse, verbose,
          sh_ratio)

    store.store()

    store.close()

    if cullext is not None:

        cull(ham, cullext, 'ham', tdict)

        cull(spam, cullext, 'spam', tdict)

    return 0
 if __name__ == "__main__":

    sys.exit(main(sys.argv[1:]))

 if __name__ == "__main__":

    sys.exit(main(sys.argv[1:]))





