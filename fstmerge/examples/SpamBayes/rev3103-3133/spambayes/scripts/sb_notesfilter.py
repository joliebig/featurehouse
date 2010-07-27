'''sb_notesfilter.py - Lotus Notes SpamBayes interface.
    This module uses SpamBayes as a filter against a Lotus Notes mail
    database.  The Notes client must be running when this process is
    executed.
    It requires a Notes folder, named as a parameter, with four
    subfolders:
        Spam
        Ham
        Train as Spam
        Train as Ham
    Depending on the execution parameters, it will do any or all of the
    following steps, in the order given.
    1. Train Spam from the Train as Spam folder (-t option)
    2. Train Ham from the Train as Ham folder (-t option)
    3. Replicate (-r option)
    4. Classify the inbox (-c option)
    Mail that is to be trained as spam should be manually moved to
    that folder by the user. Likewise mail that is to be trained as
    ham.  After training, spam is moved to the Spam folder and ham is
    moved to the Ham folder.
    Replication takes place if a remote server has been specified.
    This step may take a long time, depending on replication
    parameters and how much information there is to download, as well
    as line speed and server load.  Please be patient if you run with
    replication.  There is currently no progress bar or anything like
    that to tell you that it's working, but it is and will complete
    eventually.  There is also no mechanism for notifying you that the
    replication failed.  If it did, there is no harm done, and the program
    will continue execution.
    Mail that is classified as Spam is moved from the inbox to the
    Train as Spam folder.  You should occasionally review your Spam
    folder for Ham that has mistakenly been classified as Spam.  If
    there is any there, move it to the Train as Ham folder, so
    SpamBayes will be less likely to make this mistake again.
    Mail that is classified as Ham or Unsure is left in the inbox.
    There is currently no means of telling if a mail was classified as
    Ham or Unsure.
    You should occasionally select some Ham and move it to the Train
    as Ham folder, so Spambayes can tell the difference between Spam
    and Ham. The goal is to maintain an approximate balance between the
    number of Spam and the number of Ham that have been trained into
    the database. These numbers are reported every time this program
    executes.  However, if the amount of Spam you receive far exceeds
    the amount of Ham you receive, it may be very difficult to
    maintain this balance.  This is not a matter of great concern.
    SpamBayes will still make very few mistakes in this circumstance.
    But, if this is the case, you should review your Spam folder for
    falsely classified Ham, and retrain those that you find, on a
    regular basis.  This will prevent statistical error accumulation,
    which if allowed to continue, would cause SpamBayes to tend to
    classify everything as Spam.
    Because there is no programmatic way to determine if a particular
    mail has been previously processed by this classification program,
    it keeps a pickled dictionary of notes mail ids, so that once a
    mail has been classified, it will not be classified again.  The
    non-existence of this index file, named <local database>.sbindex,
    indicates to the system that this is an initialization execution.
    Rather than classify the inbox in this case, the contents of the
    inbox are placed in the index to note the 'starting point' of the
    system.  After that, any new messages in the inbox are eligible
    for classification.
Usage:
    sb_notesfilter [options]
        note: option values with spaces in them must be enclosed
              in double quotes
        options:
            -p  dbname  : pickled training database filename
            -d  dbname  : dbm training database filename
            -l  dbname  : database filename of local mail replica
                            e.g. localmail.nsf
            -r  server  : server address of the server mail database
                            e.g. d27ml602/27/M/IBM
                          if specified, will initiate a replication
            -f  folder  : Name of SpamBayes folder
                            must have subfolders: Spam
                                                  Ham
                                                  Train as Spam
                                                  Train as Ham
            -t          : train contents of Train as Spam and Train as Ham
            -c          : classify inbox
            -h          : help
            -P          : prompt "Press Enter to end" before ending
                          This is useful for automated executions where the
                          statistics output would otherwise be lost when the
                          window closes.
            -i filename : index file name
            -W          : password
            -L dbname   : log to database (template alog4.ntf)
            -o section:option:value :
                          set [section, option] in the options database
                          to value
Examples:
    Replicate and classify inbox
        sb_notesfilter -c -d notesbayes -r mynoteserv -l mail.nsf -f Spambayes
    Train Spam and Ham, then classify inbox
        sb_notesfilter -t -c -d notesbayes -l mail.nsf -f Spambayes
    Replicate, then classify inbox
        sb_notesfilter -c -d test7 -l mail.nsf -r nynoteserv -f Spambayes
To Do:
    o Dump/purge notesindex file
    o Create correct folders if they do not exist
    o Options for some of this stuff?
    o sb_server style training/configuration interface?
    o parameter to retrain?
    o Use spambayes.message MessageInfo db's rather than own database.
    o Suggestions?
'''

__author__ = "Tim Stone <tim@fourstonesExpressions.com>"

__credits__ = "Mark Hammond, for his remarkable win32 modules."

from __future__ import generators

try:

    True, False

except NameError:

    True, False = 1, 0

    def bool(val):

        return not not val



import sys

from spambayes import tokenizer, storage

from spambayes.Options import options

import cPickle as pickle

import errno

import win32com.client

import pywintypes

import getopt

def classifyInbox(v, vmoveto, bayes, ldbname, notesindex, log):

    if len(notesindex.keys()) == 0:

        firsttime = 1

    else:

        firsttime = 0

    docstomove = []

    numham = 0

    numspam = 0

    numuns = 0

    numdocs = 0

    doc = v.GetFirstDocument()

    while doc:

        nid = doc.NOTEID

        if firsttime:

            notesindex[nid] = 'never classified'

        else:

            if not notesindex.has_key(nid):

                numdocs += 1

                message = getMessage(doc)

                options["Tokenizer", "generate_long_skips"] = False

                tokens = tokenizer.tokenize(message)

                prob, clues = bayes.spamprob(tokens, evidence=True)

                if prob < options["Categorization", "ham_cutoff"]:

                    disposition = options["Headers", "header_ham_string"]

                    numham += 1

                elif prob > options["Categorization", "spam_cutoff"]:

                    disposition = options["Headers", "header_spam_string"]

                    docstomove += [doc]

                    numspam += 1

                else:

                    disposition = options["Headers", "header_unsure_string"]

                    numuns += 1

                notesindex[nid] = 'classified'

                try:

                    print "%s spamprob is %s" % (subj[:30], prob)

                    if log:

                        log.LogAction("%s spamprob is %s" % (subj[:30],
                                                             prob))

                except UnicodeError:

                    print "<subject not printed> spamprob is %s" % (prob)

                    if log:

                        log.LogAction("<subject not printed> spamprob " \
                                      "is %s" % (prob,))

                item = doc.ReplaceItemValue("Spam", prob)

                item.IsSummary = True

                doc.save(False, True, False)

        doc = v.GetNextDocument(doc)

    for doc in docstomove:

        doc.RemoveFromFolder(v.Name)

        doc.PutInFolder(vmoveto.Name)

    print "%s documents processed" % (numdocs,)

    print "   %s classified as spam" % (numspam,)

    print "   %s classified as ham" % (numham,)

    print "   %s classified as unsure" % (numuns,)

    if log:

        log.LogAction("%s documents processed" % (numdocs,))

        log.LogAction("   %s classified as spam" % (numspam,))

        log.LogAction("   %s classified as ham" % (numham,))

        log.LogAction("   %s classified as unsure" % (numuns,))
 def getMessage(doc):

    try:

        subj = doc.GetItemValue('Subject')[0]

    except:

        subj = 'No Subject'

    try:

        body  = doc.GetItemValue('Body')[0]

    except:

        body = 'No Body'

    hdrs = ''

    for item in doc.Items:

        if item.Name == "From" or item.Name == "Sender" or \
           item.Name == "Received" or item.Name == "ReplyTo":

            try:

                hdrs = hdrs + ( "%s: %s\r\n" % (item.Name, item.Text) )

            except:

                hdrs = ''

    message = "%sSubject: %s\r\n\r\n%s" % (hdrs, subj, body)

    return message
 def processAndTrain(v, vmoveto, bayes, is_spam, notesindex, log):

    if is_spam:

        header_str = options["Headers", "header_spam_string"]

    else:

        header_str = options["Headers", "header_ham_string"]

    print "Training %s" % (header_str,)

    docstomove = []

    doc = v.GetFirstDocument()

    while doc:

        message = getMessage(doc)

        options["Tokenizer", "generate_long_skips"] = False

        tokens = tokenizer.tokenize(message)

        nid = doc.NOTEID

        if notesindex.has_key(nid):

            trainedas = notesindex[nid]

            if trainedas == options["Headers", "header_spam_string"] and \
               not is_spam:

                bayes.unlearn(tokens, True)

            elif trainedas == options["Headers", "header_ham_string"] and \
                 is_spam:

                bayes.unlearn(tokens, False)

        bayes.learn(tokens, is_spam)

        notesindex[nid] = header_str

        docstomove += [doc]

        doc = v.GetNextDocument(doc)

    for doc in docstomove:

        doc.RemoveFromFolder(v.Name)

        doc.PutInFolder(vmoveto.Name)

    print "%s documents trained" % (len(docstomove),)

    if log:

        log.LogAction("%s documents trained" % (len(docstomove),))
 def run(bdbname, useDBM, ldbname, rdbname, foldname, doTrain, doClassify,
        pwd, idxname, logname):

    bayes = storage.open_storage(bdbname, useDBM)

    try:

        fp = open(idxname, 'rb')

    except IOError, e:

        if e.errno != errno.ENOENT:

            raise

        notesindex = {}

        print "%s file not found, this is a first time run" % (idxname,)

        print "No classification will be performed"

    else:

        notesindex = pickle.load(fp)

        fp.close()

    need_replicate = False

    sess = win32com.client.Dispatch("Lotus.NotesSession")

    try:

        if pwd:

            sess.initialize(pwd)

        else:

            sess.initialize()

    except pywintypes.com_error:

        print "Session aborted"

        sys.exit()

    try:

        db = sess.GetDatabase(rdbname, ldbname)

    except pywintypes.com_error:

        if rdbname:

            print "Could not open database remotely, trying locally"

            try:

                db = sess.GetDatabase("", ldbname)

                need_replicate = True

            except pywintypes.com_error:

                print "Could not open database"

                sys.exit()

        else:

            raise

    log = sess.CreateLog("SpambayesAgentLog")

    try:

        log.OpenNotesLog("", logname)

    except pywintypes.com_error:

        print "Could not open log"

        log = None

    if log:

        log.LogAction("Running spambayes")

    vinbox = db.getView('($Inbox)')

    vspam = db.getView("%s\Spam" % (foldname,))

    vham = db.getView("%s\Ham" % (foldname,))

    vtrainspam = db.getView("%s\Train as Spam" % (foldname,))

    vtrainham = db.getView("%s\Train as Ham" % (foldname,))

    if doTrain:

        processAndTrain(vtrainspam, vspam, bayes, True, notesindex, log)

        processAndTrain(vtrainham, vham, bayes, False, notesindex, log)

    if need_replicate:

        try:

            print "Replicating..."

            db.Replicate(rdbname)

            print "Done"

        except pywintypes.com_error:

            print "Could not replicate"

    if doClassify:

        classifyInbox(vinbox, vtrainspam, bayes, ldbname, notesindex, log)

    print "The Spambayes database currently has %s Spam and %s Ham" \
          % (bayes.nspam, bayes.nham)

    bayes.store()

    fp = open(idxname, 'wb')

    pickle.dump(notesindex, fp)

    fp.close()

    if log:

        log.LogAction("Finished running spambayes")
 if __name__ == '__main__':

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'htcPd:p:l:r:f:o:i:W:L:')

    except getopt.error, msg:

        print >>sys.stderr, str(msg) + '\n\n' + __doc__

        sys.exit()

    ldbname = None  

    rdbname = None  

    sbfname = None  

    idxname = None  

    logname = None  

    pwd = None 

    doTrain = False

    doClassify = False

    doPrompt = False

    for opt, arg in opts:

        if opt == '-h':

            print >>sys.stderr, __doc__

            sys.exit()

        elif opt == '-l':

            ldbname = arg

        elif opt == '-r':

            rdbname = arg

        elif opt == '-f':

            sbfname = arg

        elif opt == '-t':

            doTrain = True

        elif opt == '-c':

            doClassify = True

        elif opt == '-P':

            doPrompt = True

        elif opt == '-i':

            idxname = arg

        elif opt == '-L':

            logname = arg

        elif opt == '-W':

            pwd = arg

        elif opt == '-o':

            options.set_from_cmdline(arg, sys.stderr)

    bdbname, useDBM = storage.database_type(opts)

    if not idxname:

        idxname = "%s.sbindex" % (ldbname)

    if (bdbname and ldbname and sbfname and (doTrain or doClassify)):

        run(bdbname, useDBM, ldbname, rdbname, \
            sbfname, doTrain, doClassify, pwd, idxname, logname)

        if doPrompt:

            try:

                key = input("Press Enter to end")

            except SyntaxError:

                pass

    else:

        print >>sys.stderr, __doc__

 if __name__ == '__main__':

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'htcPd:p:l:r:f:o:i:W:L:')

    except getopt.error, msg:

        print >>sys.stderr, str(msg) + '\n\n' + __doc__

        sys.exit()

    ldbname = None  

    rdbname = None  

    sbfname = None  

    idxname = None  

    logname = None  

    pwd = None 

    doTrain = False

    doClassify = False

    doPrompt = False

    for opt, arg in opts:

        if opt == '-h':

            print >>sys.stderr, __doc__

            sys.exit()

        elif opt == '-l':

            ldbname = arg

        elif opt == '-r':

            rdbname = arg

        elif opt == '-f':

            sbfname = arg

        elif opt == '-t':

            doTrain = True

        elif opt == '-c':

            doClassify = True

        elif opt == '-P':

            doPrompt = True

        elif opt == '-i':

            idxname = arg

        elif opt == '-L':

            logname = arg

        elif opt == '-W':

            pwd = arg

        elif opt == '-o':

            options.set_from_cmdline(arg, sys.stderr)

    bdbname, useDBM = storage.database_type(opts)

    if not idxname:

        idxname = "%s.sbindex" % (ldbname)

    if (bdbname and ldbname and sbfname and (doTrain or doClassify)):

        run(bdbname, useDBM, ldbname, rdbname, \
            sbfname, doTrain, doClassify, pwd, idxname, logname)

        if doPrompt:

            try:

                key = input("Press Enter to end")

            except SyntaxError:

                pass

    else:

        print >>sys.stderr, __doc__



try:

    True, False

except NameError:

    True, False = 1, 0

    def bool(val):

        return not not val



