'''storage.py - Spambayes database management framework.
Classes:
    PickledClassifier - Classifier that uses a pickle db
    DBDictClassifier - Classifier that uses a shelve db
    PGClassifier - Classifier that uses postgres
    mySQLClassifier - Classifier that uses mySQL
    CBDClassifier - Classifier that uses CDB
    ZODBClassifier - Classifier that uses ZODB
    ZEOClassifier - Classifier that uses ZEO
    Trainer - Classifier training observer
    SpamTrainer - Trainer for spam
    HamTrainer - Trainer for ham
Abstract:
    *Classifier are subclasses of Classifier (classifier.Classifier)
    that add automatic state store/restore function to the Classifier class.
    All SQL based classifiers are subclasses of SQLClassifier, which is a
    subclass of Classifier.
    PickledClassifier is a Classifier class that uses a cPickle
    datastore.  This database is relatively small, but slower than other
    databases.
    DBDictClassifier is a Classifier class that uses a database
    store.
    Trainer is concrete class that observes a Corpus and trains a
    Classifier object based upon movement of messages between corpora  When
    an add message notification is received, the trainer trains the
    database with the message, as spam or ham as appropriate given the
    type of trainer (spam or ham).  When a remove message notification
    is received, the trainer untrains the database as appropriate.
    SpamTrainer and HamTrainer are convenience subclasses of Trainer, that
    initialize as the appropriate type of Trainer
To Do:
    o Suggestions?
    '''
__author__ = "Neale Pickett <neale@woozle.org>, \
Tim Stone <tim@fourstonesExpressions.com>"
__credits__ = "All the spambayes contributors."
try:
    True, False
except NameError:
    True, False = 1, 0
    def bool(val):
        return not not val
import os
import sys
import time
import types
import tempfile
from spambayes import classifier
from spambayes.Options import options, get_pathname_option
import cPickle as pickle
import errno
import shelve
from spambayes import cdb
from spambayes import dbmstorage
oldShelvePickler = shelve.Pickler
def binaryDefaultPickler(f, binary=1):
    return oldShelvePickler(f, binary)
shelve.Pickler = binaryDefaultPickler
PICKLE_TYPE = 1
NO_UPDATEPROBS = False   
UPDATEPROBS = True       
class PickledClassifier(classifier.Classifier):
    '''Classifier object persisted in a pickle'''
    def __init__(self, db_name):
        classifier.Classifier.__init__(self)
        self.db_name = db_name
        self.load()
    def load(self):
        '''Load this instance from the pickle.'''
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Loading state from',self.db_name,'pickle'
        tempbayes = None
        try:
            fp = open(self.db_name, 'rb')
        except IOError, e:
            if e.errno != errno.ENOENT: raise
        else:
            tempbayes = pickle.load(fp)
            fp.close()
        if tempbayes:
            classifier.Classifier.__setstate__(self,
                                               tempbayes.__getstate__())
            if options["globals", "verbose"]:
                print >> sys.stderr, ('%s is an existing pickle,'
                                      ' with %d ham and %d spam') \
                      % (self.db_name, self.nham, self.nspam)
        else:
            if options["globals", "verbose"]:
                print >> sys.stderr, self.db_name,'is a new pickle'
            self.wordinfo = {}
            self.nham = 0
            self.nspam = 0
    def store(self):
        '''Store self as a pickle'''
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Persisting',self.db_name,'as a pickle'
        tmp = self.db_name + '.tmp'
        try: 
            fp = open(tmp, 'wb') 
            pickle.dump(self, fp, PICKLE_TYPE) 
            fp.close() 
        except IOError, e: 
            if options["globals", "verbose"]: 
                print >> sys.stderr, 'Failed update: ' + str(e)
            if fp is not None: 
                os.remove(tmp) 
            raise
        try:
            os.rename(tmp, self.db_name)
        except OSError:
            os.rename(self.db_name, self.db_name + '.bak')
            os.rename(tmp, self.db_name)
            os.remove(self.db_name + '.bak')
    def close(self):
        pass
WORD_DELETED = "D"
WORD_CHANGED = "C"
STATE_KEY = 'saved state'
class DBDictClassifier(classifier.Classifier):
    '''Classifier object persisted in a caching database'''
    def __init__(self, db_name, mode='c'):
        '''Constructor(database name)'''
        classifier.Classifier.__init__(self)
        self.statekey = STATE_KEY
        self.mode = mode
        self.db_name = db_name
        self.load()
    def close(self):
        def noop(): pass
        getattr(self.db, "close", noop)()
        getattr(self.dbm, "close", noop)()
        if hasattr(self, "db"):
            del self.db
        if hasattr(self, "dbm"):
            del self.dbm
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Closed',self.db_name,'database'
    def load(self):
        '''Load state from database'''
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Loading state from',self.db_name,'database'
        self.dbm = dbmstorage.open(self.db_name, self.mode)
        self.db = shelve.Shelf(self.dbm)
        if self.db.has_key(self.statekey):
            t = self.db[self.statekey]
            if t[0] != classifier.PICKLE_VERSION:
                raise ValueError("Can't unpickle -- version %s unknown" % t[0])
            (self.nspam, self.nham) = t[1:]
            if options["globals", "verbose"]:
                print >> sys.stderr, ('%s is an existing database,'
                                      ' with %d spam and %d ham') \
                      % (self.db_name, self.nspam, self.nham)
        else:
            if options["globals", "verbose"]:
                print >> sys.stderr, self.db_name,'is a new database'
            self.nspam = 0
            self.nham = 0
        self.wordinfo = {}
        self.changed_words = {} 
    def store(self):
        '''Place state into persistent store'''
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Persisting',self.db_name,'state in database'
        for key, flag in self.changed_words.iteritems():
            if flag is WORD_CHANGED:
                val = self.wordinfo[key]
                self.db[key] = val.__getstate__()
            elif flag is WORD_DELETED:
                assert key not in self.wordinfo, \
                       "Should not have a wordinfo for words flagged for delete"
                try:
                    del self.db[key]
                except KeyError:
                    pass
            else:
                raise RuntimeError, "Unknown flag value"
        self.changed_words = {}
        self._write_state_key()
        self.db.sync()
    def _write_state_key(self):
        self.db[self.statekey] = (classifier.PICKLE_VERSION,
                                  self.nspam, self.nham)
    def _post_training(self):
        """This is called after training on a wordstream.  We ensure that the
        database is in a consistent state at this point by writing the state
        key."""
        self._write_state_key()
    def _wordinfoget(self, word):
        if isinstance(word, unicode):
            word = word.encode("utf-8")
        try:
            return self.wordinfo[word]
        except KeyError:
            ret = None
            if self.changed_words.get(word) is not WORD_DELETED:
                r = self.db.get(word)
                if r:
                    ret = self.WordInfoClass()
                    ret.__setstate__(r)
                    self.wordinfo[word] = ret
            return ret
    def _wordinfoset(self, word, record):
        if isinstance(word, unicode):
            word = word.encode("utf-8")
        if record.spamcount + record.hamcount <= 1:
            self.db[word] = record.__getstate__()
            try:
                del self.changed_words[word]
            except KeyError:
                pass
            try:
                del self.wordinfo[word]
            except KeyError:
                pass
        else:
            self.wordinfo[word] = record
            self.changed_words[word] = WORD_CHANGED
    def _wordinfodel(self, word):
        if isinstance(word, unicode):
            word = word.encode("utf-8")
        del self.wordinfo[word]
        self.changed_words[word] = WORD_DELETED
    def _wordinfokeys(self):
        wordinfokeys = self.db.keys()
        del wordinfokeys[wordinfokeys.index(self.statekey)]
        return wordinfokeys
class SQLClassifier(classifier.Classifier):
    def __init__(self, db_name):
        '''Constructor(database name)'''
        classifier.Classifier.__init__(self)
        self.statekey = STATE_KEY
        self.db_name = db_name
        self.load()
    def close(self):
        '''Release all database resources'''
        pass
    def load(self):
        '''Load state from the database'''
        raise NotImplementedError, "must be implemented in subclass"
    def store(self):
        '''Save state to the database'''
        self._set_row(self.statekey, self.nspam, self.nham)
    def cursor(self):
        '''Return a new db cursor'''
        raise NotImplementedError, "must be implemented in subclass"
    def fetchall(self, c):
        '''Return all rows as a dict'''
        raise NotImplementedError, "must be implemented in subclass"
    def commit(self, c):
        '''Commit the current transaction - may commit at db or cursor'''
        raise NotImplementedError, "must be implemented in subclass"
    def create_bayes(self):
        '''Create a new bayes table'''
        c = self.cursor()
        c.execute(self.table_definition)
        self.commit(c)
    def _get_row(self, word):
        '''Return row matching word'''
        try:
            c = self.cursor()
            c.execute("select * from bayes"
                      "  where word=%s",
                      (word,))
        except Exception, e:
            print >> sys.stderr, "error:", (e, word)
            raise
        rows = self.fetchall(c)
        if rows:
            return rows[0]
        else:
            return {}
    def _set_row(self, word, nspam, nham):
        c = self.cursor()
        if self._has_key(word):
            c.execute("update bayes"
                      "  set nspam=%s,nham=%s"
                      "  where word=%s",
                      (nspam, nham, word))
        else:
            c.execute("insert into bayes"
                      "  (nspam, nham, word)"
                      "  values (%s, %s, %s)",
                      (nspam, nham, word))
        self.commit(c)
    def _delete_row(self, word):
        c = self.cursor()
        c.execute("delete from bayes"
                  "  where word=%s",
                  (word,))
        self.commit(c)
    def _has_key(self, key):
        c = self.cursor()
        c.execute("select word from bayes"
                  "  where word=%s",
                  (key,))
        return len(self.fetchall(c)) > 0
    def _wordinfoget(self, word):
        if isinstance(word, unicode):
            word = word.encode("utf-8")
        row = self._get_row(word)
        if row:
            item = self.WordInfoClass()
            item.__setstate__((row["nspam"], row["nham"]))
            return item
        else:
            return self.WordInfoClass()
    def _wordinfoset(self, word, record):
        if isinstance(word, unicode):
            word = word.encode("utf-8")
        self._set_row(word, record.spamcount, record.hamcount)
    def _wordinfodel(self, word):
        if isinstance(word, unicode):
            word = word.encode("utf-8")
        self._delete_row(word)
    def _wordinfokeys(self):
        c = self.cursor()
        c.execute("select word from bayes")
        rows = self.fetchall(c)
        return [r[0] for r in rows]
class PGClassifier(SQLClassifier):
    '''Classifier object persisted in a Postgres database'''
    def __init__(self, db_name):
        self.table_definition = ("create table bayes ("
                                 "  word bytea not null default '',"
                                 "  nspam integer not null default 0,"
                                 "  nham integer not null default 0,"
                                 "  primary key(word)"
                                 ")")
        SQLClassifier.__init__(self, db_name)
    def cursor(self):
        return self.db.cursor()
    def fetchall(self, c):
        return c.dictfetchall()
    def commit(self, c):
        self.db.commit()
    def load(self):
        '''Load state from database'''
        import psycopg
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Loading state from',self.db_name,'database'
        self.db = psycopg.connect('dbname=' + self.db_name)
        c = self.cursor()
        try:
            c.execute("select count(*) from bayes")
        except psycopg.ProgrammingError:
            self.db.rollback()
            self.create_bayes()
        if self._has_key(self.statekey):
            row = self._get_row(self.statekey)
            self.nspam = row["nspam"]
            self.nham = row["nham"]
            if options["globals", "verbose"]:
                print >> sys.stderr, ('%s is an existing database,'
                                      ' with %d spam and %d ham') \
                      % (self.db_name, self.nspam, self.nham)
        else:
            if options["globals", "verbose"]:
                print >> sys.stderr, self.db_name,'is a new database'
            self.nspam = 0
            self.nham = 0
class mySQLClassifier(SQLClassifier):
    '''Classifier object persisted in a mySQL database
    It is assumed that the database already exists, and that the mySQL
    server is currently running.'''
    def __init__(self, data_source_name):
        self.table_definition = ("create table bayes ("
                                 "  word varchar(255) not null default '',"
                                 "  nspam integer not null default 0,"
                                 "  nham integer not null default 0,"
                                 "  primary key(word)"
                                 ");")
        self.host = "localhost"
        self.username = "root"
        self.password = ""
        db_name = "spambayes"
        self.charset = None
        source_info = data_source_name.split()
        for info in source_info:
            if info.startswith("host"):
                self.host = info[5:]
            elif info.startswith("user"):
                self.username = info[5:]
            elif info.startswith("pass"):
                self.password = info[5:]
            elif info.startswith("dbname"):
                db_name = info[7:]
            elif info.startswith("charset"):
                self.charset = info[8:]
        SQLClassifier.__init__(self, db_name)
    def cursor(self):
        return self.db.cursor()
    def fetchall(self, c):
        return c.fetchall()
    def commit(self, c):
        self.db.commit()
    def load(self):
        '''Load state from database'''
        import MySQLdb
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Loading state from',self.db_name,'database'
        params = {
          'host': self.host, 'db': self.db_name,
          'user': self.username, 'passwd': self.password,
          'charset': self.charset
        }
        self.db = MySQLdb.connect(**params)
        c = self.cursor()
        try:
            c.execute("select count(*) from bayes")
        except MySQLdb.ProgrammingError:
            try:
                self.db.rollback()
            except MySQLdb.NotSupportedError:
                pass
            self.create_bayes()
        if self._has_key(self.statekey):
            row = self._get_row(self.statekey)
            self.nspam = int(row[1])
            self.nham = int(row[2])
            if options["globals", "verbose"]:
                print >> sys.stderr, ('%s is an existing database,'
                                      ' with %d spam and %d ham') \
                      % (self.db_name, self.nspam, self.nham)
        else:
            if options["globals", "verbose"]:
                print >> sys.stderr, self.db_name,'is a new database'
            self.nspam = 0
            self.nham = 0
    def _wordinfoget(self, word):
        if isinstance(word, unicode):
            word = word.encode("utf-8")
        row = self._get_row(word)
        if row:
            item = self.WordInfoClass()
            item.__setstate__((row[1], row[2]))
            return item
        else:
            return None
class CDBClassifier(classifier.Classifier):
    """A classifier that uses a CDB database.
    A CDB wordinfo database is quite small and fast but is slow to update.
    It is appropriate if training is done rarely (e.g. monthly or weekly
    using archived ham and spam).
    """
    def __init__(self, db_name):
        classifier.Classifier.__init__(self)
        self.db_name = db_name
        self.statekey = STATE_KEY
        self.load()
    def _WordInfoFactory(self, counts):
        ham, spam = counts.split(',')
        wi = classifier.WordInfo()
        wi.hamcount = int(ham)
        wi.spamcount = int(spam)
        return wi
    def uunquote(self, s):
        for encoding in ("utf-8", "cp1252", "iso-8859-1"):
            try:
                return unicode(s, encoding)
            except UnicodeDecodeError:
                pass
        return s
    def load(self):
        if os.path.exists(self.db_name):
            db = open(self.db_name, "rb")
            data = dict(cdb.Cdb(db))
            db.close()
            self.nham, self.nspam = [int(i) for i in \
                                     data[self.statekey].split(',')]
            self.wordinfo = dict([(self.uunquote(k),
                                   self._WordInfoFactory(v)) \
                                  for k, v in data.iteritems() \
                                      if k != self.statekey])
            if options["globals", "verbose"]:
                print >> sys.stderr, ('%s is an existing CDB,'
                                      ' with %d ham and %d spam') \
                                      % (self.db_name, self.nham,
                                         self.nspam)
        else:
            if options["globals", "verbose"]:
                print >> sys.stderr, self.db_name, 'is a new CDB'
            self.wordinfo = {}
            self.nham = 0
            self.nspam = 0
    def store(self):
        items = [(self.statekey, "%d,%d" % (self.nham, self.nspam))]
        for word, wi in self.wordinfo.iteritems():
            if isinstance(word, types.UnicodeType):
                word = word.encode("utf-8")
            items.append((word, "%d,%d" % (wi.hamcount, wi.spamcount)))
        db = open(self.db_name, "wb")
        cdb.cdb_make(db, items)
        db.close()
    def close(self):
        pass
try:
    from persistent import Persistent
except ImportError:
    try:
        from ZODB import Persistent
    except ImportError:
        Persistent = object
class _PersistentClassifier(classifier.Classifier, Persistent):
    def __init__(self):
        import ZODB
        from BTrees.OOBTree import OOBTree
        classifier.Classifier.__init__(self)
        self.wordinfo = OOBTree()
class ZODBClassifier(object):
    ClassifierClass = _PersistentClassifier
    def __init__(self, db_name, mode='c'):
        self.db_filename = db_name
        self.db_name = os.path.basename(db_name)
        self.closed = True
        self.mode = mode
        self.load()
    def __getattr__(self, att):
        if hasattr(self, "classifier") and hasattr(self.classifier, att):
            return getattr(self.classifier, att)
        raise AttributeError("ZODBClassifier object has no attribute '%s'"
                             % (att,))
    def __setattr__(self, att, value):
        if att in ("nham", "nspam") and hasattr(self, "classifier"):
            setattr(self.classifier, att, value)
        else:
            object.__setattr__(self, att, value)
    def create_storage(self):
        import ZODB
        from ZODB.FileStorage import FileStorage
        try:
            self.storage = FileStorage(self.db_filename,
                                       read_only=self.mode=='r')
        except IOError, msg:
            print >> sys.stderr, ("Could not create FileStorage from",
                                  self.db_filename)
            raise
    def load(self):
        '''Load state from database'''
        import ZODB
        if options["globals", "verbose"]:
            print >> sys.stderr, "Loading state from %s (%s) database" % \
                  (self.db_filename, self.db_name)
        if not self.closed:
            self.close()
        self.create_storage()
        self.DB = ZODB.DB(self.storage, cache_size=10000)
        self.conn = self.DB.open()
        root = self.conn.root()
        self.classifier = root.get(self.db_name)
        if self.classifier is None:
            if options["globals", "verbose"]:
                print >> sys.stderr, self.db_name, 'is a new ZODB'
            self.classifier = root[self.db_name] = self.ClassifierClass()
        else:
            if options["globals", "verbose"]:
                print >> sys.stderr, '%s is an existing ZODB, with %d ' \
                      'ham and %d spam' % (self.db_name, self.nham,
                                           self.nspam)
        self.closed = False
    def store(self):
        '''Place state into persistent store'''
        try:
            import ZODB
            import ZODB.Transaction
        except ImportError:
            import transaction
            commit = transaction.commit
            abort = transaction.abort
        else:
            commit = ZODB.Transaction.get_transaction().commit
            abort = ZODB.Transaction.get_transaction().abort
        from ZODB.POSException import ConflictError
        try:
            from ZODB.POSException import TransactionFailedError
        except:
            from ZODB.POSException import TransactionError as TransactionFailedError
        from ZODB.POSException import ReadOnlyError
        assert not self.closed, "Can't store a closed database"
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Persisting', self.db_name, 'state in database'
        try:
            commit()
        except ConflictError:
            if options["globals", "verbose"]:
                print >> sys.stderr, "Conflict on commit", self.db_name
            abort()
        except TransactionFailedError:
            print >> sys.stderr, "Storing failed.  Need to restart.", \
                  self.db_name
            abort()
        except ReadOnlyError:
            print >> sys.stderr, "Can't store transaction to read-only db."
            abort()
    def close(self, pack=True, retain_backup=True):
        if self.mode != 'r':
            self.store()
        if pack and self.mode != 'r':
            self.pack(time.time()-60*60*24, retain_backup)
        self.DB.close()
        self.storage.close()
        delattr(self, "classifier")
        self.closed = True
        if options["globals", "verbose"]:
            print >> sys.stderr, 'Closed', self.db_name, 'database'
    def pack(self, t, retain_backup=True):
        """Like FileStorage pack(), but optionally remove the .old
        backup file that is created.  Often for our purposes we do
        not care about being able to recover from this.  Also
        ignore the referencesf parameter, which appears to not do
        anything."""
        if hasattr(self.storage, "pack"):
            self.storage.pack(t, None)
        if not retain_backup:
            old_name = self.db_filename + ".old"
            if os.path.exists(old_name):
                os.remove(old_name)
class ZEOClassifier(ZODBClassifier):
    def __init__(self, data_source_name):
        source_info = data_source_name.split()
        self.host = "localhost"
        self.port = None
        db_name = "SpamBayes"
        self.username = ''
        self.password = ''
        self.storage_name = '1'
        self.wait = None
        self.wait_timeout = None
        for info in source_info:
            if info.startswith("host"):
                try:
                    self.host = str(info[5:])
                except UnicodeDecodeError, e:
                    print >> sys.stderr, "Couldn't set host", \
                          info[5:], str(e)
            elif info.startswith("port"):
                self.port = int(info[5:])
            elif info.startswith("dbname"):
                db_name = info[7:]
            elif info.startswith("user"):
                self.username = info[5:]
            elif info.startswith("pass"):
                self.password = info[5:]
            elif info.startswith("storage_name"):
                self.storage_name = info[13:]
            elif info.startswith("wait_timeout"):
                self.wait_timeout = int(info[13:])
            elif info.startswith("wait"):
                self.wait = info[5:] == "True"
        ZODBClassifier.__init__(self, db_name)
    def create_storage(self):
        from ZEO.ClientStorage import ClientStorage
        if self.port:
            addr = self.host, self.port
        else:
            addr = self.host
        if options["globals", "verbose"]:
            print >> sys.stderr, "Connecting to ZEO server", addr, \
                  self.username, self.password
        try:
            self.storage = ClientStorage(addr, name=self.db_name,
                                         read_only=self.mode=='r',
                                         username=self.username,
                                         client=self.db_name,
                                         wait=self.wait,
                                         wait_timeout=self.wait_timeout,
                                         storage=self.storage_name,
                                         var=tempfile.gettempdir(),
                                         password=self.password)
        except ValueError:
            try:
                os.remove(os.path.join(tempfile.gettempdir(),
                                       self.db_name + \
                                       self.storage_name + ".zec"))
            except OSError:
                pass
            self.storage = ClientStorage(addr, name=self.db_name,
                                         read_only=self.mode=='r',
                                         username=self.username,
                                         wait=self.wait,
                                         wait_timeout=self.wait_timeout,
                                         storage=self.storage_name,
                                         password=self.password)
    def is_connected(self):
        return self.storage.is_connected()
NO_TRAINING_FLAG = 1
class Trainer(object):
    '''Associates a Classifier object and one or more Corpora, \
    is an observer of the corpora'''
    def __init__(self, bayes, is_spam, updateprobs=NO_UPDATEPROBS):
        '''Constructor(Classifier, is_spam(True|False),
        updateprobs(True|False)'''
        self.bayes = bayes
        self.is_spam = is_spam
        self.updateprobs = updateprobs
    def onAddMessage(self, message, flags=0):
        '''A message is being added to an observed corpus.'''
        if not (flags & NO_TRAINING_FLAG):
            self.train(message)
    def train(self, message):
        '''Train the database with the message'''
        if options["globals", "verbose"]:
            print >> sys.stderr, 'training with ', message.key()
        self.bayes.learn(message.tokenize(), self.is_spam)
        message.setId(message.key())
        message.RememberTrained(self.is_spam)
    def onRemoveMessage(self, message, flags=0):
        '''A message is being removed from an observed corpus.'''
        if not (flags & NO_TRAINING_FLAG):
            self.untrain(message)
    def untrain(self, message):
        '''Untrain the database with the message'''
        if options["globals", "verbose"]:
            print >> sys.stderr, 'untraining with',message.key()
        self.bayes.unlearn(message.tokenize(), self.is_spam)
        message.RememberTrained(None)
    def trainAll(self, corpus):
        '''Train all the messages in the corpus'''
        for msg in corpus:
            self.train(msg)
    def untrainAll(self, corpus):
        '''Untrain all the messages in the corpus'''
        for msg in corpus:
            self.untrain(msg)
class SpamTrainer(Trainer):
    '''Trainer for spam'''
    def __init__(self, bayes, updateprobs=NO_UPDATEPROBS):
        '''Constructor'''
        Trainer.__init__(self, bayes, True, updateprobs)
class HamTrainer(Trainer):
    '''Trainer for ham'''
    def __init__(self, bayes, updateprobs=NO_UPDATEPROBS):
        '''Constructor'''
        Trainer.__init__(self, bayes, False, updateprobs)
class NoSuchClassifierError(Exception):
    def __init__(self, invalid_name):
        self.invalid_name = invalid_name
    def __str__(self):
        return repr(self.invalid_name)
class MutuallyExclusiveError(Exception):
    def __str__(self):
        return "Only one type of database can be specified"
_storage_types = {"dbm" : (DBDictClassifier, True, True),
                  "pickle" : (PickledClassifier, False, True),
                  "pgsql" : (PGClassifier, False, False),
                  "mysql" : (mySQLClassifier, False, False),
                  "cdb" : (CDBClassifier, False, True),
                  "zodb" : (ZODBClassifier, True, True),
                  "zeo" : (ZEOClassifier, False, False),
                  }
def open_storage(data_source_name, db_type="dbm", mode=None):
    """Return a storage object appropriate to the given parameters.
    By centralizing this code here, all the applications will behave
    the same given the same options.
    """
    try:
        klass, supports_mode, unused = _storage_types[db_type]
    except KeyError:
        raise NoSuchClassifierError(db_type)
    try:
        if supports_mode and mode is not None:
            return klass(data_source_name, mode)
        else:
            return klass(data_source_name)
    except dbmstorage.error, e:
        if str(e) == "No dbm modules available!":
            print >> sys.stderr, "\nYou do not have a dbm module available " \
                  "to use.  You need to either use a pickle (see the FAQ)" \
                  ", use Python 2.3 (or above), or install a dbm module " \
                  "such as bsddb (see http://sf.net/projects/pybsddb)."
            sys.exit()
        raise
_storage_options = { "-p" : "pickle",
                     "-d" : "dbm",
                     }
def database_type(opts, default_type=("Storage", "persistent_use_database"),
                  default_name=("Storage", "persistent_storage_file")):
    """Return the name of the database and the type to use.  The output of
    this function can be used as the db_type parameter for the open_storage
    function, for example:
        [standard getopts code]
        db_name, db_type = database_type(opts)
        storage = open_storage(db_name, db_type)
    The selection is made based on the options passed, or, if the
    appropriate options are not present, the options in the global
    options object.
    Currently supports:
       -p  :  pickle
       -d  :  dbm
    """
    nm, typ = None, None
    for opt, arg in opts:
        if _storage_options.has_key(opt):
            if nm is None and typ is None:
                nm, typ = arg, _storage_options[opt]
            else:
                raise MutuallyExclusiveError()
    if nm is None and typ is None:
        typ = options[default_type]
        try:
            unused, unused, is_path = _storage_types[typ]
        except KeyError:
            raise NoSuchClassifierError(db_type)
        if is_path:
            nm = get_pathname_option(*default_name)
        else:
            nm = options[default_name]
    return nm, typ
def convert(old_name=None, old_type=None, new_name=None, new_type=None):
    if old_name is None:
        old_name = "hammie.db"
    if old_type is None:
        old_type = "dbm"
    if new_name is None or new_type is None:
        auto_name, auto_type = database_type({})
        if new_name is None:
            new_name = auto_name
        if new_type is None:
            new_type = auto_type
    old_bayes = open_storage(old_name, old_type, 'r')
    new_bayes = open_storage(new_name, new_type)
    words = old_bayes._wordinfokeys()
    try:
        new_bayes.nham = old_bayes.nham
    except AttributeError:
        new_bayes.nham = 0
    try:
        new_bayes.nspam = old_bayes.nspam
    except AttributeError:
        new_bayes.nspam = 0
    print >> sys.stderr, "Converting %s (%s database) to " \
          "%s (%s database)." % (old_name, old_type, new_name, new_type)
    print >> sys.stderr, "Database has %s ham, %s spam, and %s words." % \
          (new_bayes.nham, new_bayes.nspam, len(words))
    for word in words:
        new_bayes._wordinfoset(word, old_bayes._wordinfoget(word))
    old_bayes.close()
    print >> sys.stderr, "Storing database, please be patient..."
    new_bayes.store()
    print >> sys.stderr, "Conversion complete."
    new_bayes.close()
def ensureDir(dirname):
    """Ensure that the given directory exists - in other words, if it
    does not exist, attempt to create it."""
    try:
        os.mkdir(dirname)
        if options["globals", "verbose"]:
            print >>sys.stderr, "Creating directory", dirname
    except OSError, e:
        if e.errno != errno.EEXIST:
            raise
if __name__ == '__main__':
    print >> sys.stderr, __doc__
