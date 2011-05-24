"""sb_dbexpimp.py - Bayes database export/import
    This utility has the primary function of exporting and importing
    a spambayes database into/from a CSV file.  This is useful in a number
    of scenarios.
    Platform portability of database - CSV files can be exported and
    imported across platforms (Windows and Linux, for example).
    Database implementation changes - databases can survive database
    implementation upgrades or new database implementations.  For example,
    if a dbm implementation changes between python x.y and python x.y+1...
    Database reorganization - an export followed by an import reorgs an
    existing database, <theoretically> improving performance, at least in
    some database implementations.
    Database sharing - it is possible to distribute particular databases
    for research purposes, database sharing purposes, or for new users to
    have a 'seed' database to start with.
    Database merging - multiple databases can be merged into one quite
    easily by specifying -m on an import.  This will add the two database
    nham and nspams together and for wordinfo conflicts, will add spamcount
    and hamcount together.
Usage:
    sb_dbexpimp [options]
        options:
            -e     : export
            -i     : import
            -f: FN : flat file to export to or import from
            -p: FN : name of pickled database file to use
            -d: FN : name of dbm database file to use
            -m     : merge import into an existing database file.  This is
                     meaningful only for import. If omitted, a new database
                     file will be created.  If specified, the imported
                     wordinfo will be merged into an existing database.
                     Run dbExpImp -h for more information.
            -o: section:option:value :
                     set [section, option] in the options database to value
            -h     : help
If neither -p nor -d is specified, then the values in your configuration
file (or failing that, the defaults) will be used.  In this way, you may
convert to and from storage formats other than pickle and dbm.
Examples:
    Export pickled mybayes.db into mybayes.db.export as a CSV file
        sb_dbexpimp -e -p mybayes.db -f mybayes.db.export
    Import mybayes.db.export into a new DBM mybayes.db
        sb_dbexpimp -i -d mybayes.db -f mybayes.db.export
    Convert a bayes database from pickle to DBM
        sb_dbexpimp -e -p abayes.db -f abayes.export
        sb_dbexpimp -i -d abayes.db -f abayes.export
    Create a new DBM database (newbayes.db) from two
        DBM databases (abayes.db, bbayes.db)
        sb_dbexpimp -e -d abayes.db -f abayes.export
        sb_dbexpimp -e -d bbayes.db -f bbayes.export
        sb_dbexpimp -i -d newbayes.db -f abayes.export
        sb_dbexpimp -i -m -d newbayes.db -f bbayes.export
"""

__author__ = "Tim Stone <tim@fourstonesExpressions.com>"

import csv

import spambayes.storage

from spambayes.Options import options

import sys, os, getopt, errno

from types import UnicodeType

def uquote(s):

    if isinstance(s, UnicodeType):

        s = s.encode('utf-8')

    return s
 def uunquote(s):

    for encoding in ("utf-8", "cp1252", "iso-8859-1"):

        try:

            return str(s, encoding)

        except UnicodeDecodeError:

            pass

    return s
 def runExport(dbFN, useDBM, outFN):

    bayes = spambayes.storage.open_storage(dbFN, useDBM)

    if useDBM == "dbm":

        words = list(bayes.db.keys())

        words.remove(bayes.statekey)

    else:

        words = list(bayes.wordinfo.keys())

    try:

        fp = open(outFN, 'wb')

    except IOError as e:

        if e.errno != errno.ENOENT:

            raise

    writer = csv.writer(fp)

    nham = bayes.nham

    nspam = bayes.nspam

    print("Exporting database %s to file %s" % (dbFN, outFN))

    print("Database has %s ham, %s spam, and %s words" \
            % (nham, nspam, len(words)))

    writer.writerow([nham, nspam])

    for word in words:

        wi = bayes._wordinfoget(word)

        hamcount = wi.hamcount

        spamcount = wi.spamcount

        word = uquote(word)

        writer.writerow([word, hamcount, spamcount])
 def runImport(dbFN, useDBM, newDBM, inFN):

    if newDBM:

        try:

            os.unlink(dbFN)

        except OSError:

            pass

    bayes = spambayes.storage.open_storage(dbFN, useDBM)

    fp = open(inFN, 'rb')

    rdr = csv.reader(fp)

    (nham, nspam) = next(rdr)

    if newDBM:

        bayes.nham = int(nham)

        bayes.nspam = int(nspam)

    else:

        bayes.nham += int(nham)

        bayes.nspam += int(nspam)

    if newDBM:

        impType = "Importing"

    else:

        impType = "Merging"

    print("%s file %s into database %s" % (impType, inFN, dbFN))

    for (word, hamcount, spamcount) in rdr:

        word = uunquote(word)

        wi = bayes._wordinfoget(word)

        if wi is None:

            wi = bayes.WordInfoClass()

        wi.hamcount += int(hamcount)

        wi.spamcount += int(spamcount)

        bayes._wordinfoset(word, wi)

    print("Storing database, please be patient.  Even moderately sized")

    print("databases may take a very long time to store.")

    bayes.store()

    print("Finished storing database")

    if useDBM == "dbm" or useDBM == True:

        words = list(bayes.db.keys())

        words.remove(bayes.statekey)

    else:

        words = list(bayes.wordinfo.keys())

    print("Database has %s ham, %s spam, and %s words" \
           % (bayes.nham, bayes.nspam, len(words)))
 if __name__ == '__main__':

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'iehmvd:p:f:o:')

    except getopt.error as msg:

        print(str(msg) + '\n\n' + __doc__, file=sys.stderr)

        sys.exit()

    useDBM = "pickle"

    newDBM = True

    dbFN = None

    flatFN = None

    exp = False

    imp = False

    for opt, arg in opts:

        if opt == '-h':

            print(__doc__, file=sys.stderr)

            sys.exit()

        elif opt == '-f':

            flatFN = arg

        elif opt == '-e':

            exp = True

        elif opt == '-i':

            imp = True

        elif opt == '-m':

            newDBM = False

        elif opt in ('-o', '--option'):

            options.set_from_cmdline(arg, sys.stderr)

    dbFN, useDBM = spambayes.storage.database_type(opts)

    if (dbFN and flatFN):

        if exp:

            runExport(dbFN, useDBM, flatFN)

        if imp:

            runImport(dbFN, useDBM, newDBM, flatFN)

    else:

        print(__doc__, file=sys.stderr)

 if __name__ == '__main__':

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'iehmvd:p:f:o:')

    except getopt.error, msg:

        print >> sys.stderr, str(msg) + '\n\n' + __doc__

        sys.exit()

    useDBM = "pickle"

    newDBM = True

    dbFN = None

    flatFN = None

    exp = False

    imp = False

    for opt, arg in opts:

        if opt == '-h':

            print >> sys.stderr, __doc__

            sys.exit()

        elif opt == '-f':

            flatFN = arg

        elif opt == '-e':

            exp = True

        elif opt == '-i':

            imp = True

        elif opt == '-m':

            newDBM = False

        elif opt in ('-o', '--option'):

            options.set_from_cmdline(arg, sys.stderr)

    dbFN, useDBM = spambayes.storage.database_type(opts)

    if (dbFN and flatFN):

        if exp:

            runExport(dbFN, useDBM, flatFN)

        if imp:

            runImport(dbFN, useDBM, newDBM, flatFN)

    else:

        print >> sys.stderr, __doc__





