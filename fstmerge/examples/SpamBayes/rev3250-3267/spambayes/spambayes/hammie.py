import math

from spambayes import mboxutils

from spambayes import storage

from spambayes.Options import options

from spambayes.tokenizer import tokenize

class  Hammie :
	"""A spambayes mail filter.
    This implements the basic functionality needed to score, filter, or
    train.
    """
	    def __init__(self, bayes, mode):

        self.bayes = bayes

        self.mode = mode
 def _scoremsg(self, msg, evidence=False):

        """Score a Message.
        msg can be a string, a file object, or a Message object.
        Returns the probability the message is spam.  If evidence is
        true, returns a tuple: (probability, clues), where clues is a
        list of the words which contributed to the score.
        """

        return self.bayes.spamprob(tokenize(msg), evidence)
 def formatclues(self, clues, sep="; "):

        """Format the clues into something readable."""

        return sep.join(["%r: %.2f" % (word, prob)
                         for word, prob in clues
                         if (word[0] == '*' or
                             prob <= options["Headers",
                                             "clue_mailheader_cutoff"] or
                             prob >= 1.0 - options["Headers",
                                                   "clue_mailheader_cutoff"])])
 def score(self, msg, evidence=False):

        """Score (judge) a message.
        msg can be a string, a file object, or a Message object.
        Returns the probability the message is spam.  If evidence is
        true, returns a tuple: (probability, clues), where clues is a
        list of the words which contributed to the score.
        """

        return self._scoremsg(msg, evidence)
 def score_and_filter(self, msg, header=None, spam_cutoff=None,
                         ham_cutoff=None, debugheader=None,
                         debug=None, train=None):

        """Score (judge) a message and add a disposition header.
        msg can be a string, a file object, or a Message object.
        Optionally, set header to the name of the header to add, and/or
        spam_cutoff/ham_cutoff to the probability values which must be met
        or exceeded for a message to get a 'Spam' or 'Ham' classification.
        An extra debugging header can be added if 'debug' is set to True.
        The name of the debugging header is given as 'debugheader'.
        If 'train' is True, also train on the result of scoring the
        message (ie. train as ham if it's ham, train as spam if it's
        spam).  If the message already has a trained header, it will be
        untrained first.  You'll want to be very dilligent about
        retraining mistakes if you use this option.
        All defaults for optional parameters come from the Options file.
        Returns the score and same message with a new disposition header.
        """

        if header == None:

            header = options["Headers", "classification_header_name"]

        if spam_cutoff == None:

            spam_cutoff = options["Categorization", "spam_cutoff"]

        if ham_cutoff == None:

            ham_cutoff = options["Categorization", "ham_cutoff"]

        if debugheader == None:

            debugheader = options["Headers", "evidence_header_name"]

        if debug == None:

            debug = options["Headers", "include_evidence"]

        if train == None:

            train = options["Hammie", "train_on_filter"]

        msg = mboxutils.get_message(msg)

        try:

            del msg[header]

        except KeyError:

            pass

        if train:

            self.untrain_from_header(msg)

        prob, clues = self._scoremsg(msg, True)

        if prob < ham_cutoff:

            is_spam = False

            disp = options["Headers", "header_ham_string"]

        elif prob > spam_cutoff:

            is_spam = True

            disp = options["Headers", "header_spam_string"]

        else:

            is_spam = False

            disp = options["Headers", "header_unsure_string"]

        if train:

            self.train(msg, is_spam, True)

        basic_disp = disp

        disp += "; %.*f" % (options["Headers", "header_score_digits"], prob)

        if options["Headers", "header_score_logarithm"]:

            if prob <= 0.005 and prob > 0.0:

                import math

                x = -math.log10(prob)

                disp += " (%d)" % x

            if prob >= 0.995 and prob < 1.0:

                x = -math.log10(1.0-prob)

                disp += " (%d)" % x

        del msg[header]

        msg.add_header(header, disp)

        for header in ('to', 'subject'):

            if basic_disp in options["Headers", "notate_"+header]:

                orig = msg[header]

                del msg[header]

                msg[header] = "%s,%s" % (basic_disp, orig)

        if debug:

            disp = self.formatclues(clues)

            del msg[debugheader]

            msg.add_header(debugheader, disp)

        result = mboxutils.as_string(msg, unixfrom=(msg.get_unixfrom()
                                                    is not None))

        return prob, result
 def filter(self, msg, header=None, spam_cutoff=None,
               ham_cutoff=None, debugheader=None,
               debug=None, train=None):

        _prob, result = self.score_and_filter(
            msg, header, spam_cutoff, ham_cutoff, debugheader,
            debug, train)

        return result
 def train(self, msg, is_spam, add_header=False):

        """Train bayes with a message.
        msg can be a string, a file object, or a Message object.
        is_spam should be 1 if the message is spam, 0 if not.
        If add_header is True, add a header with how it was trained (in
        case we need to untrain later)
        """

        self.bayes.learn(tokenize(msg), is_spam)

        if add_header:

            if is_spam:

                trained = options["Headers", "header_spam_string"]

            else:

                trained = options["Headers", "header_ham_string"]

            del msg[options["Headers", "trained_header_name"]]

            msg.add_header(options["Headers", "trained_header_name"], trained)
 def untrain(self, msg, is_spam):

        """Untrain bayes with a message.
        msg can be a string, a file object, or a Message object.
        is_spam should be True if the message is spam, False if not.
        """

        self.bayes.unlearn(tokenize(msg), is_spam)
 def untrain_from_header(self, msg):

        """Untrain bayes based on X-Spambayes-Trained header.
        msg can be a string, a file object, or a Message object.
        If no such header is present, nothing happens.
        If add_header is True, add a header with how it was trained (in
        case we need to untrain later)
        """

        msg = mboxutils.get_message(msg)

        trained = msg.get(options["Headers", "trained_header_name"])

        if not trained:

            return

        del msg[options["Headers", "trained_header_name"]]

        if trained == options["Headers", "header_ham_string"]:

            self.untrain_ham(msg)

        elif trained == options["Headers", "header_spam_string"]:

            self.untrain_spam(msg)

        else:

            raise ValueError('%s header value unrecognized'
                             % options["Headers", "trained_header_name"])
 def train_ham(self, msg, add_header=False):

        """Train bayes with ham.
        msg can be a string, a file object, or a Message object.
        If add_header is True, add a header with how it was trained (in
        case we need to untrain later)
        """

        self.train(msg, False, add_header)
 def train_spam(self, msg, add_header=False):

        """Train bayes with spam.
        msg can be a string, a file object, or a Message object.
        If add_header is True, add a header with how it was trained (in
        case we need to untrain later)
        """

        self.train(msg, True, add_header)
 def untrain_ham(self, msg):

        """Untrain bayes with a message previously trained as ham.
        msg can be a string, a file object, or a Message object.
        """

        self.untrain(msg, False)
 def untrain_spam(self, msg):

        """Untrain bayes with a message previously traned as spam.
        msg can be a string, a file object, or a Message object.
        """

        self.untrain(msg, True)
 def store(self):

        """Write out the persistent store.
        This makes sure the persistent store reflects what is currently
        in memory.  You would want to do this after a write and before
        exiting.
        """

        self.bayes.store()
 def close(self):

        if self.mode != 'r':

            self.store()

def open(filename, useDB="dbm", mode='r'):

    """Open a file, returning a Hammie instance.
    mode is used as the flag to open DBDict objects.  'c' for read-write
    (create if needed), 'r' for read-only, 'w' for read-write.
    """

    return Hammie(storage.open_storage(filename, useDB, mode), mode)
 if __name__ == "__main__":

    from spambayes import hammiebulk

    hammiebulk.main()

 if __name__ == "__main__":

    from spambayes import hammiebulk

    hammiebulk.main()



