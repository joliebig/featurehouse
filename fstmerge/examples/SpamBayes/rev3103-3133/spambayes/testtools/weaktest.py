"""Usage: %(program)s  [options] -n nsets
Where:
    -h
        Show usage and exit.
    -n int
        Number of Set directories (Data/Spam/Set1, ... and Data/Ham/Set1, ...).
        This is required.
    -d decider
        Name of the decider. One of %(decisionkeys)s
    -m min
        Minimal number of messages to train on before involving the decider.
In addition, an attempt is made to merge bayescustomize.ini into the options.
If that exists, it can be used to change the settings in Options.options.
"""

from __future__ import generators

import sys,os

from spambayes.Options import options, get_pathname_option

from spambayes import hammie, msgs, CostCounter

program = sys.argv[0]

debug = 0

def usage(code, msg=''):

    """Print usage message and sys.exit(code)."""

    if msg:

        print >> sys.stderr, msg

        print >> sys.stderr

    print >> sys.stderr, __doc__ % globals()

    sys.exit(code)


DONT_TRAIN = None

TRAIN_AS_HAM = 1

TRAIN_AS_SPAM = 2

class  TrainDecision :
	def __call__(self,scr,is_spam):

        if is_spam:

            return self.spamtrain(scr)

        else:

            return self.hamtrain(scr)

class  UnsureAndFalses (TrainDecision) :
	def spamtrain(self,scr):

        if scr < options["Categorization", "spam_cutoff"]:

            return TRAIN_AS_SPAM
 def hamtrain(self,scr):

        if scr > options["Categorization", "ham_cutoff"]:

            return TRAIN_AS_HAM

class  UnsureOnly (TrainDecision) :
	def spamtrain(self,scr):

        if options["Categorization", "ham_cutoff"] < scr < \
           options["Categorization", "spam_cutoff"]:

            return TRAIN_AS_SPAM
 def hamtrain(self,scr):

        if options["Categorization", "ham_cutoff"] < scr < \
           options["Categorization", "spam_cutoff"]:

            return TRAIN_AS_HAM

class  All (TrainDecision) :
	def spamtrain(self,scr):

        return TRAIN_AS_SPAM
 def hamtrain(self,scr):

        return TRAIN_AS_HAM

class  AllBut0and100 (TrainDecision) :
	def spamtrain(self,scr):

        if scr < 0.995:

            return TRAIN_AS_SPAM
 def hamtrain(self,scr):

        if scr > 0.005:

            return TRAIN_AS_HAM

class  OwnDecision (TrainDecision) :
	def hamtrain(self,scr):

        if scr < options["Categorization", "ham_cutoff"]:

            return TRAIN_AS_HAM

        elif scr > options["Categorization", "spam_cutoff"]:

            return TRAIN_AS_SPAM

	spamtrain = hamtrain
class  OwnDecisionFNCorrection (OwnDecision) :
	def spamtrain(self,scr):

        return TRAIN_AS_SPAM

decisions={'all': All,
           'allbut0and100': AllBut0and100,
           'unsureonly': UnsureOnly,
           'unsureandfalses': UnsureAndFalses,
           'owndecision': OwnDecision,
           'owndecision+fn': OwnDecisionFNCorrection,
          } decisionkeys=decisions.keys() decisionkeys.sort() class  FirstN :
	def __init__(self,n,client):

        self.client = client

        self.x = 0

        self.n = n
 def __call__(self,scr,is_spam):

        self.x += 1

        if self.tooearly():

            if is_spam:

                return TRAIN_AS_SPAM

            else:

                return TRAIN_AS_HAM

        else:

            return self.client(scr,is_spam)
 def tooearly(self):

        return self.x < self.n

class  Updater :
	def __init__(self,d=None):

        self.setd(d)
 def setd(self,d):

        self.d=d

def drive(nsets,decision):

    print options.display()

    spamdirs = [get_pathname_option("TestDriver", "spam_directories") % \
                i for i in range(1, nsets+1)]

    hamdirs  = [get_pathname_option("TestDriver", "ham_directories") % \
                i for i in range(1, nsets+1)]

    spamfns = [(x,y,1) for x in spamdirs for y in os.listdir(x)]

    hamfns = [(x,y,0) for x in hamdirs for y in os.listdir(x)]

    nham = len(hamfns)

    nspam = len(spamfns)

    cc = CostCounter.nodelay()

    allfns = {}

    for fn in spamfns+hamfns:

        allfns[fn] = None

    d = hammie.open('weaktest.db', False)

    hamtrain = 0

    spamtrain = 0

    n = 0

    for dir,name, is_spam in allfns.iterkeys():

        n += 1

        m=msgs.Msg(dir, name).guts

        if debug > 1:

            print "trained:%dH+%dS"%(hamtrain,spamtrain)

        scr=d.score(m)

        if debug > 1:

            print "score:%.3f"%scr

        if not decision.tooearly():

            if is_spam:

                if debug > 0:

                    print "Spam with score %.2f"%scr

                cc.spam(scr)

            else:

                if debug > 0:

                    print "Ham with score %.2f"%scr

                cc.ham(scr)

        de = decision(scr,is_spam)

        if de == TRAIN_AS_SPAM:

            d.train_spam(m)

            spamtrain += 1

        elif de == TRAIN_AS_HAM:

            d.train_ham(m)

            hamtrain += 1

        if n % 100 == 0:

            print "%5d trained:%dH+%dS wrds:%d"%(
                n, hamtrain, spamtrain, len(d.bayes.wordinfo))

            print cc

    print "="*70

    print "%5d trained:%dH+%dS wrds:%d"%(
        n, hamtrain, spamtrain, len(d.bayes.wordinfo))

    print cc
 def main():

    global debug

    import getopt

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'vd:hn:m:')

    except getopt.error, msg:

        usage(1, msg)

    nsets = None

    decision = decisions['unsureonly']

    m = 10

    for opt, arg in opts:

        if opt == '-h':

            usage(0)

        elif opt == '-n':

            nsets = int(arg)

        elif opt == '-v':

            debug += 1

        elif opt == '-m':

            m = int(arg)

        elif opt == '-d':

            if not decisions.has_key(arg):

                usage(1,'Unknown decisionmaker')

            decision = decisions[arg]

    if args:

        usage(1, "Positional arguments not supported")

    if nsets is None:

        usage(1, "-n is required")

    drive(nsets,decision=FirstN(m,decision()))
 if __name__ == "__main__":

    main()

 if __name__ == "__main__":

    main()



