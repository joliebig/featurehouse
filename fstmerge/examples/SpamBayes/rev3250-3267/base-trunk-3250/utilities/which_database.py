"""which_database
This little script checks which database is used to save your data, and
also prints out information about which database systems are available.
It will check whichever database you have setup to use in the [Storage]
persistent_storage_file option.
Note that you will end up with extra files after running this utility
that may be safely deleted:
  o dumbdb.dir
  o dumbdb.dat
  o dumbdb.bak
  o bsddb3
  o dbhash
"""
__author__ = "Remi Ricard <papaDoc@videotron.ca>"
__credits__ = "Skip Montanaro, all the Spambayes folk."
import os
import sys
sys.path.insert(-1, os.getcwd())
sys.path.insert(-1, os.path.dirname(os.getcwd()))
from spambayes.Options import options, get_pathname_option
import dumbdbm
import dbhash
import whichdb
try:
    import bsddb
except ImportError:
    bsddb = None
def main():
    print "Pickle is available."
    db = dumbdbm.open("dumbdb", "c")
    db["1"] = "1"
    db.close()
    dbstr = whichdb.whichdb("dumbdb")
    if dbstr:
        print "Dumbdbm is available."
    else:
        print "Dumbdbm is not available."
    db = dbhash.open("dbhash", "c")
    db["1"] = "1"
    db.close()
    dbstr = whichdb.whichdb("dbhash")
    if dbstr == "dbhash":
        print "Dbhash is available."
    else:
        print "Dbhash is not available."
    if bsddb is None:
        dbstr = ""
    else:
        db = bsddb.hashopen("bsddb3", "c")
        db["1"] = "1"
        db.close()
        dbstr = whichdb.whichdb("bsddb3")
    if dbstr == "dbhash":
        print "Bsddb[3] is available."
    else:
        print "Bsddb[3] is not available."
    print
    hammie = get_pathname_option("Storage", "persistent_storage_file")
    use_dbm = options["Storage", "persistent_use_database"]
    if not use_dbm:
        print "Your storage %s is a: pickle" % (hammie,)
        return
    if not os.path.exists(hammie):
        print "Your storage file does not exist yet."
        return
    db_type = whichdb.whichdb(hammie)
    if db_type == "dbhash":
        if hasattr(bsddb, '__version__'):
            try:
                db = bsddb.hashopen(hammie, "r")
            except bsddb.error:
                pass
            else:
                db.close()
                print "Your storage", hammie, "is a: bsddb[3]"
                return
    elif db_type is None:
        print "Your storage %s is unreadable." % (hammie,)
    print "Your storage %s is a: %s" % (hammie, db_type)
if __name__ == "__main__":
    main()
