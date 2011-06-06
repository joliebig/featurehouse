'''Corpus.py - Spambayes corpus management framework.
Classes:
    Corpus - a collection of Messages
    ExpiryCorpus - a "young" Corpus
    MessageFactory - creates a Message
Abstract:
    A corpus is defined as a set of messages that share some common
    characteristic relative to spamness.  Examples might be spam, ham,
    unsure, or untrained, or "bayes rating between .4 and .6".  A
    corpus is a collection of messages.  Corpus is a dictionary that
    is keyed by the keys of the messages within it.  It is iterable,
    and observable.  Observers are notified when a message is added
    to or removed from the corpus.
    Corpus is designed to cache message objects.  By default, it will
    only engage in lazy creation of message objects, keeping those
    objects in memory until the corpus instance itself is destroyed.
    In large corpora, this could consume a large amount of memory.  A
    cacheSize operand is implemented on the constructor, which is used
    to limit the *number* of messages currently loaded into memory.
    The instance variable that implements this cache is
    Corpus.Corpus.msgs, a dictionary.  Access to this variable should
    be through keys(), [key], or using an iterator.  Direct access
    should not be used, as subclasses that manage their cache may use
    this variable very differently.
    Iterating Corpus objects is potentially very expensive, as each
    message in the corpus will be brought into memory.  For large
    corpora, this could consume a lot of system resources.
    ExpiryCorpus is designed to keep a corpus of file messages that
    are guaranteed to be younger than a given age.  The age is
    specified on the constructor, as a number of seconds in the past.
    If a message file was created before that point in time, the a
    message is deemed to be "old" and thus ignored.  Access to a
    message that is deemed to be old will raise KeyError, which should
    be handled by the corpus user as appropriate.  While iterating,
    KeyError is handled by the iterator, and messages that raise
    KeyError are ignored.
    As messages pass their "expiration date," they are eligible for
    removal from the corpus. To remove them properly,
    removeExpiredMessages() should be called.  As messages are removed,
    observers are notified.
    ExpiryCorpus function is included into a concrete Corpus through
    multiple inheritance. It must be inherited before any inheritance
    that derives from Corpus.  For example:
        class RealCorpus(Corpus)
           ...
        class ExpiryRealCorpus(Corpus.ExpiryCorpus, RealCorpus)
           ...
    Messages have substance, which is is the textual content of the
    message. They also have a key, which uniquely defines them within
    the corpus.  This framework makes no assumptions about how or if
    messages persist.
    MessageFactory is a required factory class, because Corpus is
    designed to do lazy initialization of messages and, as an abstract
    class, must know how to create concrete instances of the correct
    class.
To Do:
    o Suggestions?
'''
__author__ = "Tim Stone <tim@fourstonesExpressions.com>"
__credits__ = "Richie Hindle, Tim Peters, all the spambayes contributors."
import sys           # for output of docstring
import time
from spambayes.Options import options
SPAM = True
HAM = False
class Corpus:
    '''An observable dictionary of Messages'''
    def __init__(self, factory, cacheSize=-1):
        '''Constructor(MessageFactory)'''
        self.msgs = {}            # dict of all messages in corpus
        self.keysInMemory = []    # keys of messages currently loaded
        self.cacheSize = cacheSize  # max number of messages in memory
        self.observers = []       # observers of this corpus
        self.factory = factory    # factory for the correct Message subclass
    def addObserver(self, observer):
        '''Register an observer, which should implement
        onAddMessage, onRemoveMessage'''
        self.observers.append(observer)
    def addMessage(self, message, observer_flags=0):
        '''Add a Message to this corpus'''
        if options["globals", "verbose"]:
            print('adding message %s to corpus' % (message.key()))
        self.cacheMessage(message)
        for obs in self.observers:
            if hasattr(obs, "onAddMessage"):
                obs.onAddMessage(message, observer_flags)
    def removeMessage(self, message, observer_flags=0):
        '''Remove a Message from this corpus'''
        key = message.key()
        if options["globals", "verbose"]:
            print('removing message %s from corpus' % (key,))
        self.unCacheMessage(key)
        del self.msgs[key]
        for obs in self.observers:
            if hasattr(obs, "onRemoveMessage"):
                obs.onRemoveMessage(message, observer_flags)
    def cacheMessage(self, message):
        '''Add a message to the in-memory cache'''
        key = message.key()
        if options["globals", "verbose"]:
            print('placing %s in corpus cache' % (key,))
        self.msgs[key] = message
        self.keysInMemory.append(key)
        if self.cacheSize > 0:       # performance optimization
            if len(self.keysInMemory) > self.cacheSize:
                keyToFlush = self.keysInMemory[0]
                self.unCacheMessage(keyToFlush)
    def unCacheMessage(self, key):
        '''Remove a message from the in-memory cache'''
        if options["globals", "verbose"]:
            print('Flushing %s from corpus cache' % (key,))
        try:
            ki = self.keysInMemory.index(key)
        except ValueError:
            pass
        else:
            del self.keysInMemory[ki]
        self.msgs[key] = None
    def takeMessage(self, key, fromcorpus, fromCache=False):
        '''Move a Message from another corpus to this corpus'''
        msg = fromcorpus[key]
        msg.load() # ensure that the substance has been loaded
        fromcorpus.removeMessage(msg)
        self.addMessage(msg)
    def get(self, key, default=None):
        if self.msgs.get(key, "") == "":
            return default
        else:
            return self[key]
    def __getitem__(self, key):
        '''Corpus is a dictionary'''
        amsg = self.msgs.get(key, "")
        if amsg == "":
            raise KeyError(key)
        if amsg is None:
            amsg = self.makeMessage(key)     # lazy init, saves memory
            self.cacheMessage(amsg)
        return amsg
    def keys(self):
        '''Message keys in the Corpus'''
        return list(self.msgs.keys())
    def __contains__(self, other):
        return other in list(self.msgs.values())
    def __iter__(self):
        '''Corpus is iterable'''
        for key in list(self.keys()):
            yield self[key]
    def __str__(self):
        '''Instance as a printable string'''
        return self.__repr__()
    def __repr__(self):
        '''Instance as a representative string'''
        raise NotImplementedError
    def makeMessage(self, key, content=None):
        '''Call the factory to make a message'''
        msg = self.factory.create(key, content)
        return msg
class ExpiryCorpus:
    '''Mixin Class - Corpus of "young" file system artifacts'''
    def __init__(self, expireBefore):
        self.expireBefore = expireBefore
        self.expiry_due = time.time()
    def removeExpiredMessages(self):
        '''Kill expired messages'''
        if time.time() < self.expiry_due:
            return
        self.expiry_due = time.time() + self.expireBefore
        for key in list(self.keys())[:]:
            msg = self[key]
            timestamp = msg.createTimestamp()
            if timestamp < time.time() - self.expireBefore:
                if options["globals", "verbose"]:
                    print('message %s has expired' % (msg.key(),))
                from spambayes.storage import NO_TRAINING_FLAG
                self.removeMessage(msg, observer_flags=NO_TRAINING_FLAG)
            elif timestamp + self.expireBefore < self.expiry_due:
                self.expiry_due = timestamp + self.expireBefore
class MessageFactory(object):
    '''Abstract Message Factory'''
    def create(self, key, content=None):
        '''Create a message instance'''
        raise NotImplementedError
if __name__ == '__main__':
    print(__doc__, file=sys.stderr)
