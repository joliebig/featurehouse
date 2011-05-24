"""A classifier that uses a CDB database.
A CDB wordinfo database is quite small and fast but is slow to update.
It is appropriate if training is done rarely (e.g. monthly or weekly using
archived ham and spam).  See mailsort.py for an example application that
uses this classifier.
"""

from spambayes import cdb

from spambayes.classifier import Classifier

class  CdbClassifier (Classifier) :
	def __init__(self, cdbfile=None):

        Classifier.__init__(self)

        if cdbfile is not None:

            self.wordinfo = cdb.Cdb(cdbfile)
 def probability(self, record):

        return float(record)
 def save_wordinfo(self, db_file):

        items = []

        for word, record in self.wordinfo.items():

            prob = Classifier.probability(self, record)

            items.append((word, str(prob)))

        cdb.cdb_make(db_file, items)



