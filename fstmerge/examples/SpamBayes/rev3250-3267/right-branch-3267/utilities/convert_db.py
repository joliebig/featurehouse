"""convert_db.py
Simplified version of sb_dbimpexp.py to aid with the change from
defaulting to dbm in 1.0.x to zodb in 1.1.x.
Usage:
    sb_dbexpimp [options]
        options:
            -t type   : type of the database to convert
                        (e.g. pickle, dbm, zodb)
            -T type   : type of database to convert to
                        (e.g. pickle, dbm, zodb)
            -n path   : path to the database to convert
            -N path   : path of the resulting database
            -h        : help
To convert the database from dbm to ZODB on Windows, simply running
the script with no options should work.  To convert the database from
dbm to ZODB on linux or OS X, the following should work:
    python convert_db.py -n ~/.hammie.db
"""
__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>"
__credits__ = "Tim Stone; all the SpamBayes folk"
import os
import sys
import getopt
from spambayes import storage
if __name__ == '__main__':
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'ht:T:n:N:')
    except getopt.error as msg:
        print(str(msg) + '\n\n' + __doc__, file=sys.stderr)
        sys.exit()
    old_name = old_type = new_name = new_type = None
    for opt, arg in opts:
        if opt == '-h':
            print(__doc__, file=sys.stderr)
            sys.exit()
        elif opt == '-t':
            old_type = arg
        elif opt == '-T':
            new_type = arg
        elif opt == '-n':
            old_name = os.path.expanduser(arg)
        elif opt == '-N':
            new_name = os.path.expanduser(arg)
    storage.convert(old_name, old_type, new_name, new_type)
