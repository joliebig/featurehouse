RC_DIR = "~/.spambayes"
DB_FILE = RC_DIR + "/wordprobs.cdb"
import sys
import os
DB_FILE = os.path.expanduser(DB_FILE)
from spambayes.cdb import Cdb
def main():
    if len(sys.argv) == 2:
        db_file = sys.argv[1]
    else:
        db_file = os.path.expanduser(DB_FILE)
    db = Cdb(open(db_file, 'rb'))
    items = []
    for k, v in db.iteritems():
        items.append((float(v), k))
    items.sort()
    for v, k in items:
        print k, v
if __name__ == "__main__":
    main()
