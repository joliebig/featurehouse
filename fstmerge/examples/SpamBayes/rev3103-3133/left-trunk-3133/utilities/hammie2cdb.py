"""
Convert a hammie database to a cdb database.
usage %(prog)s [ -h ] [ -d <file> | -p <file> ] <cdbfile>
-h      - Print this usage message and exit.
-d file - Use a database-based classifier named file.
-p file - Use a pickle-based classifier named file.
"""
import sys
import os
import getopt
from spambayes import cdb
from spambayes import storage
from spambayes.cdb_classifier import CdbClassifier
prog = os.path.basename(sys.argv[0])
def usage(msg=None):
    if msg is not None:
        print >> sys.stderr, msg
    print >> sys.stderr, __doc__.strip() % globals()
def main(args):
    try:
        opts, args = getopt.getopt(args, "hd:p:",
                                   ["help", "database=", "pickle="])
    except getopt.GetoptError, msg:
        usage(msg)
        return 1
    if len(args) != 1:
        usage()
        return 1
    cdbname = args[0]
    dbname = usedb = None
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            usage()
            return 0
    dbname, usedb = storage.database_type(opts)
    store = storage.open_storage(dbname, usedb)
    bayes = CdbClassifier()
    items = []
    for word in store._wordinfokeys():
        record = store._wordinfoget(word)
        prob = store.probability(record)
        items.append((word, str(prob)))
    cdbfile = open(cdbname, "wb")
    cdb.cdb_make(cdbfile, items)
    cdbfile.close()
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
