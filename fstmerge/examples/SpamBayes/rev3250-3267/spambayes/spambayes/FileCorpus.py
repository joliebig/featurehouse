"""FileCorpus.py - Corpus composed of file system artifacts
Classes:
    FileCorpus - an observable dictionary of FileMessages
    ExpiryFileCorpus - a FileCorpus of young files
    FileMessage - a subject of Spambayes training
    FileMessageFactory - a factory to create FileMessage objects
    GzipFileMessage - A FileMessage zipped for less storage
    GzipFileMessageFactory - factory to create GzipFileMessage objects
Abstract:
    These classes are concrete implementations of the Corpus framework.
    FileCorpus is designed to manage corpora that are directories of
    message files.
    ExpiryFileCorpus is an ExpiryCorpus of file messages.
    FileMessage manages messages that are files in the file system.
    FileMessageFactory is responsible for the creation of FileMessages,
    in response to requests to a corpus for messages.
    GzipFileMessage and GzipFileMessageFactory are used to persist messages
    as zipped files.  This can save a bit of persistent storage, though the
    ability of the compresser to do very much deflation is limited due to the
    relatively small size of the average textual message.  Still, for a large
    corpus, this could amount to a significant space savings.
    See Corpus.__doc__ for more information.
To Do:
    o Suggestions?
"""

__author__ = "Tim Stone <tim@fourstonesExpressions.com>"

__credits__ = "Richie Hindle, Tim Peters, all the spambayes contributors."

import email

from spambayes import Corpus

from spambayes import message

import os, gzip, fnmatch, time, stat

from spambayes.Options import options

class  FileCorpus (Corpus.Corpus) :
	def __init__(self, factory, directory, filter='*', cacheSize=250):

        '''Constructor(FileMessageFactory, corpus directory name, fnmatch
filter'''

        Corpus.Corpus.__init__(self, factory, cacheSize)

        self.directory = directory

        self.filter = filter

        for filename in os.listdir(directory):

            if fnmatch.fnmatch(filename, filter):

                self.msgs[filename] = None
 def makeMessage(self, key, content=None):

        '''Ask our factory to make a Message'''

        msg = self.factory.create(key, self.directory, content)

        return msg
 def addMessage(self, message, observer_flags=0):

        '''Add a Message to this corpus'''

        if not fnmatch.fnmatch(message.key(), self.filter):

            raise ValueError

        if options["globals", "verbose"]:

            print('adding', message.key(), 'to corpus')

        message.directory = self.directory

        message.store()

        Corpus.Corpus.addMessage(self, message, observer_flags)
 def removeMessage(self, message, observer_flags=0):

        '''Remove a Message from this corpus'''

        if options["globals", "verbose"]:

            print('removing', message.key(), 'from corpus')

        message.remove()

        Corpus.Corpus.removeMessage(self, message, observer_flags)
 def __repr__(self):

        '''Instance as a representative string'''

        nummsgs = len(self.msgs)

        if nummsgs != 1:

            s = 's'

        else:

            s = ''

        if options["globals", "verbose"] and nummsgs > 0:

            lst = ', ' + '%s' % (list(self.keys()))

        else:

            lst = ''

        return "<%s object at %8.8x, directory: %s, %s message%s%s>" % \
            (self.__class__.__name__, \
            id(self), \
            self.directory, \
            nummsgs, s, lst)

class  ExpiryFileCorpus (Corpus.ExpiryCorpus, FileCorpus) :
	'''FileCorpus of "young" file system artifacts'''
	    def __init__(self, expireBefore, factory, directory, filter='*', cacheSize=250):

        '''Constructor(FileMessageFactory, corpus directory name, fnmatch
filter'''

        Corpus.ExpiryCorpus.__init__(self, expireBefore)

        FileCorpus.__init__(self, factory, directory, filter, cacheSize)

class  FileMessage (object) :
	'''Message that persists as a file system artifact.'''
	    message_class = message.SBHeaderMessage
	    def __init__(self, file_name=None, directory=None):

        '''Constructor(message file name, corpus directory name)'''

        self.file_name = file_name

        self.directory = directory

        self.loaded = False

        self._msg = self.message_class()
 def __getattr__(self, att):

        """Pretend we are a subclass of message.SBHeaderMessage."""

        if hasattr(self, "_msg") and hasattr(self._msg, att):

            return getattr(self._msg, att)

        raise AttributeError()
 def __getitem__(self, k):

        """Pretend we are a subclass of message.SBHeaderMessage."""

        if hasattr(self, "_msg"):

            return self._msg[k]

        raise TypeError()
 def __setitem__(self, k, v):

        """Pretend we are a subclass of message.SBHeaderMessage."""

        if hasattr(self, "_msg"):

            self._msg[k] = v

            return

        raise TypeError()
 def as_string(self, unixfrom=False):

        self.load() 

        return self._msg.as_string(unixfrom)
 def pathname(self):

        '''Derive the pathname of the message file'''

        assert self.file_name is not None, \
               "Must set filename before using FileMessage instances."

        assert self.directory is not None, \
               "Must set directory before using FileMessage instances."

        return os.path.join(self.directory, self.file_name)
 def load(self):

        '''Read the Message substance from the file'''

        if self.loaded:

            return

        assert self.file_name is not None, \
               "Must set filename before using FileMessage instances."

        if options["globals", "verbose"]:

            print('loading', self.file_name)

        pn = self.pathname()

        fp = gzip.open(pn, 'rb')

        try:

            self._msg = email.message_from_string(\
                fp.read(), _class = self.message_class)

        except IOError as e:

            if str(e) == 'Not a gzipped file' or \
               str(e) == 'Unknown compression method':

                fp.close()

                fp = open(self.pathname(), 'rb')

                self._msg = email.message_from_string(\
                    fp.read(), _class = self.message_class)

                fp.close()

            else:

                raise

        else:

            fp.close()

        self.loaded = True
 def store(self):

        '''Write the Message substance to the file'''

        assert self.file_name is not None, \
               "Must set filename before using FileMessage instances."

        if options["globals", "verbose"]:

            print('storing', self.file_name)

        fp = open(self.pathname(), 'wb')

        fp.write(self.as_string())

        fp.close()
 def remove(self):

        '''Message hara-kiri'''

        if options["globals", "verbose"]:

            print('physically deleting file', self.pathname())

        try:

            os.unlink(self.pathname())

        except OSError:

            if options["globals", "verbose"]:

                print('file', self.pathname(), 'can not be deleted')
 def name(self):

        '''A unique name for the message'''

        assert self.file_name is not None, \
               "Must set filename before using FileMessage instances."

        return self.file_name
 def key(self):

        '''The key of this message in the msgs dictionary'''

        assert self.file_name is not None, \
               "Must set filename before using FileMessage instances."

        return self.file_name
 def __repr__(self):

        '''Instance as a representative string'''

        sub = self.as_string()

        if not options["globals", "verbose"]:

            if len(sub) > 20:

                if len(sub) > 40:

                    sub = sub[:20] + '...' + sub[-20:]

                else:

                    sub = sub[:20]

        return "<%s object at %8.8x, file: %s, %s>" % \
            (self.__class__.__name__, \
            id(self), self.pathname(), sub)
 def __str__(self):

        '''Instance as a printable string'''

        return self.__repr__()
 def createTimestamp(self):

        '''Return the create timestamp for the file'''

        try:

            stats = os.stat(self.pathname())

        except OSError:

            ctime = time.time()

        else:

            ctime = stats[stat.ST_CTIME]

        return ctime

class  MessageFactory (Corpus.MessageFactory) :
	klass = None
	    def create(self, key, directory, content=None):

        '''Create a message object from a filename in a directory'''

        if content:

            msg = email.message_from_string(content,
                                            _class=self.klass)

            msg.file_name = key

            msg.directory = directory

            msg.loaded = True

            return msg

        return self.klass(key, directory)

class  FileMessageFactory (MessageFactory) :
	'''MessageFactory for FileMessage objects'''
	    klass = FileMessage
class  GzipFileMessage (FileMessage) :
	'''Message that persists as a zipped file system artifact.'''
	    def store(self):

        '''Write the Message substance to the file'''

        assert self.file_name is not None, \
               "Must set filename before using FileMessage instances."

        if options["globals", "verbose"]:

            print('storing', self.file_name)

        pn = self.pathname()

        gz = gzip.open(pn, 'wb')

        gz.write(self.as_string())

        gz.flush()

        gz.close()

class  GzipFileMessageFactory (MessageFactory) :
	'''MessageFactory for FileMessage objects'''
	    klass = GzipFileMessage




