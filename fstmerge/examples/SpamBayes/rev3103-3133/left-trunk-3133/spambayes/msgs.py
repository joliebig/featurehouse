from __future__ import generators
import os
import random
from spambayes.tokenizer import tokenize
HAMTEST  = None
SPAMTEST = None
HAMTRAIN  = None
SPAMTRAIN = None
SEED = random.randrange(2000000000)
class Msg(object):
    __slots__ = 'tag', 'guts'
    def __init__(self, dir, name):
        path = dir + "/" + name
        self.tag = path
        f = open(path, 'rb')
        self.guts = f.read()
        f.close()
    def __iter__(self):
        return tokenize(self.guts)
    def __hash__(self):
        return hash(self.tag)
    def __eq__(self, other):
        return self.tag == other.tag
    def __str__(self):
        return self.guts
    def __getstate__(self):
        return self.tag, self.guts
    def __setstate__(self, s):
        self.tag, self.guts = s
class MsgStream(object):
    __slots__ = 'tag', 'directories', 'keep'
    def __init__(self, tag, directories, keep=None):
        self.tag = tag
        self.directories = directories
        self.keep = keep
    def __str__(self):
        return self.tag
    def produce(self):
        if self.keep is None:
            for directory in self.directories:
                for fname in os.listdir(directory):
                    yield Msg(directory, fname)
            return
        for directory in self.directories:
            all = os.listdir(directory)
            random.seed(hash(max(all)) ^ SEED) 
            random.shuffle(all)
            del all[self.keep:]
            all.sort()  
            for fname in all:
                yield Msg(directory, fname)
    def __iter__(self):
        return self.produce()
class HamStream(MsgStream):
    def __init__(self, tag, directories, train=0):
        if train:
            MsgStream.__init__(self, tag, directories, HAMTRAIN)
        else:
            MsgStream.__init__(self, tag, directories, HAMTEST)
class SpamStream(MsgStream):
    def __init__(self, tag, directories, train=0):
        if train:
            MsgStream.__init__(self, tag, directories, SPAMTRAIN)
        else:
            MsgStream.__init__(self, tag, directories, SPAMTEST)
def setparms(hamtrain, spamtrain, hamtest=None, spamtest=None, seed=None):
    """Set HAMTEST/TRAIN and SPAMTEST/TRAIN.
       If seed is not None, also set SEED.
       If (ham|spam)test are not set, set to the same as the (ham|spam)train
       numbers (backwards compat option).
    """
    global HAMTEST, SPAMTEST, HAMTRAIN, SPAMTRAIN, SEED
    HAMTRAIN, SPAMTRAIN = hamtrain, spamtrain
    if hamtest is None:
        HAMTEST = HAMTRAIN
    else:
        HAMTEST = hamtest
    if spamtest is None:
        SPAMTEST = SPAMTRAIN
    else:
        SPAMTEST = spamtest
    if seed is not None:
        SEED = seed
