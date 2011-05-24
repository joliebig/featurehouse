"""Spam/ham profile for a single VM user."""
import ZODB
from ZODB.PersistentList import PersistentList
from Persistence import Persistent
from BTrees.OOBTree import OOBTree
from spambayes import classifier
from spambayes.tokenizer import tokenize
from pspam.folder import Folder
from spambayes.Options import options
import os
def open_folders(dir, names, klass):
    L = []
    for name in names:
        path = os.path.join(dir, name)
        L.append(klass(path))
    return L
import time
_start = None
def log(s):
    global _start
    if _start is None:
        _start = time.time()
    print(round(time.time() - _start, 2), s)
class IterOOBTree(OOBTree):
    def iteritems(self):
        return list(self.items())
class WordInfo(Persistent):
    def __init__(self):
        self.spamcount = self.hamcount = 0
    def __repr__(self):
        return "WordInfo(%r, %r)" % (self.spamcount, self.hamcount)
class PMetaInfo(Persistent):
    pass
class PBayes(classifier.Bayes, Persistent):
    WordInfoClass = WordInfo
    def __init__(self):
        classifier.Bayes.__init__(self)
        self.wordinfo = IterOOBTree()
        self.meta = PMetaInfo()
class Profile(Persistent):
    FolderClass = Folder
    def __init__(self, folder_dir):
        self._dir = folder_dir
        self.classifier = PBayes()
        self.hams = PersistentList()
        self.spams = PersistentList()
    def add_ham(self, folder):
        p = os.path.join(self._dir, folder)
        f = self.FolderClass(p)
        self.hams.append(f)
    def add_spam(self, folder):
        p = os.path.join(self._dir, folder)
        f = self.FolderClass(p)
        self.spams.append(f)
    def update(self):
        """Update classifier from current folder contents."""
        changed1 = self._update(self.hams, False)
        changed2 = self._update(self.spams, True)
        get_transaction().commit()
        log("updated probabilities")
    def _update(self, folders, is_spam):
        changed = False
        for f in folders:
            log("update from %s" % f.path)
            added, removed = f.read()
            if added:
                log("added %d" % len(added))
            if removed:
                log("removed %d" % len(removed))
            get_transaction().commit()
            if not (added or removed):
                continue
            changed = True
            for msg in list(added.keys()):
                self.classifier.learn(tokenize(msg), is_spam)
            del added
            get_transaction().commit(1)
            log("learned")
            for msg in list(removed.keys()):
                self.classifier.unlearn(tokenize(msg), is_spam)
            if removed:
                log("unlearned")
            del removed
            get_transaction().commit(1)
        return changed
