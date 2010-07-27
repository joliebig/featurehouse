"""
Create mapping from features to message ids
usage %(prog)s [ options ] mailbox ...
-d mapfile - identify file which will hold mapping information (required)
-t ham|spam - identify the type of messages in the input mailbox(es)
-h - print this documentation and exit
One of '-t ham' or '-t spam' must be given, as must one or more message
sources.
"""
import sys
import getopt
import anydbm
import cPickle as pickle
from spambayes.mboxutils import getmbox
from spambayes.tokenizer import tokenize
from spambayes.Options import options
from spambayes.classifier import Classifier
prog = sys.argv[0]
def usage(msg=None):
    if msg is not None:
        print >> sys.stderr, msg
    print >> sys.stderr, __doc__.strip() % globals()
def mapmessages(f, mboxtype, mapdb):
    i = 0
    for msg in getmbox(f):
        i += 1
        sys.stdout.write('\r%s: %d' % (f, i))
        sys.stdout.flush()
        msgid = msg.get("message-id")
        if msgid is None:
            continue
        for t in tokenize(msg):
            ham, spam = mapdb.get(t, ({}, {}))
            if mboxtype == "ham":
                msgids = ham.get(f, set())
                msgids.add(msgid)
                ham[f] = msgids
            else:
                msgids = spam.get(f, set())
                msgids.add(msgid)
                spam[f] = msgids
            mapdb[t] = (ham, spam)
        if options["Classifier", "x-use_bigrams"]:
            for t in Classifier()._enhance_wordstream(tokenize(msg)):
                ham, spam = mapdb.get(t, ({}, {}))
                if mboxtype == "ham":
                    msgids = ham.get(f, set())
                    msgids.add(msgid)
                    ham[f] = msgids
                else:
                    msgids = spam.get(f, set())
                    msgids.add(msgid)
                    spam[f] = msgids
                mapdb[t] = (ham, spam)
    sys.stdout.write("\n")
def main(args):
    try:
        opts, args = getopt.getopt(args, "hd:t:",
                                   ["type=", "help", "database="])
    except getopt.GetoptError, msg:
        usage(msg)
        return 1
    mapfile = None
    mboxtype = None
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            usage()
            return 0
        elif opt in ("-d", "--database"):
            mapfile = arg
        elif opt in ("-t", "--type"):
            mboxtype = arg
    if mapfile is None:
        usage("'-d mapfile' is required")
        return 1
    if mboxtype is None:
        usage("'-t ham|spam' is required")
        return 1
    if mboxtype not in ("ham", "spam"):
        usage("mboxtype must be 'ham' or 'spam'")
        return 1
    try:
        mapd = pickle.load(file(mapfile))
    except IOError:
        mapd = {}
    for f in args:
        mapmessages(f, mboxtype, mapd)
    pickle.dump(mapd, file(mapfile, "w"))
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
