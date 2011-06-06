from __future__ import generators
from twisted.python.runtime import platform
import os
import errno
import base64
import random
import sha
from os.path import isabs, exists, normpath, abspath, splitext
from os.path import basename, dirname
from os.path import join as joinpath
from os import sep as slash
from os import listdir, utime, stat
from os import remove
from stat import ST_MODE, ST_MTIME, ST_ATIME, ST_CTIME, ST_SIZE
from stat import S_ISREG, S_ISDIR, S_ISLNK
try:
    from os.path import islink
except ImportError:
    def islink(path):
        return False
try:
    from os import urandom as randomBytes
except ImportError:
    def randomBytes(n):
        randomData = [random.randrange(256) for n in xrange(n)]
        return ''.join(map(chr, randomData))
try:
    from base64 import urlsafe_b64encode as armor
except ImportError:
    def armor(s):
        return s.encode('hex')
class InsecurePath(Exception):
    pass
def _secureEnoughString():
    """
    Create a pseudorandom, 16-character string for use in secure filenames.
    """
    return armor(sha.new(randomBytes(64)).digest())[:16]
class FilePath:
    """I am a path on the filesystem that only permits 'downwards' access.
    Instantiate me with a pathname (for example,
    FilePath('/home/myuser/public_html')) and I will attempt to only provide
    access to files which reside inside that path.  I may be a path to a file,
    a directory, or a file which does not exist.
    The correct way to use me is to instantiate me, and then do ALL filesystem
    access through me.  In other words, do not import the 'os' module; if you
    need to open a file, call my 'open' method.  If you need to list a
    directory, call my 'path' method.
    Even if you pass me a relative path, I will convert that to an absolute
    path internally.
    @type alwaysCreate: C{bool}
    @ivar alwaysCreate: When opening this file, only succeed if the file does not
    already exist.
    """
    statinfo = None
    def __init__(self, path, alwaysCreate=False):
        self.path = abspath(path)
        self.alwaysCreate = alwaysCreate
    def __getstate__(self):
        d = self.__dict__.copy()
        if d.has_key('statinfo'):
            del d['statinfo']
        return d
    def child(self, path):
        if platform.isWindows() and path.count(":"):
            raise InsecurePath("%r contains a colon." % (path,))
        norm = normpath(path)
        if slash in norm:
            raise InsecurePath("%r contains one or more directory separators" % (path,))
        newpath = abspath(joinpath(self.path, norm))
        if not newpath.startswith(self.path):
            raise InsecurePath("%r is not a child of %s" % (newpath, self.path))
        return self.clonePath(newpath)
    def preauthChild(self, path):
        """
        Use me if `path' might have slashes in it, but you know they're safe.
        (NOT slashes at the beginning. It still needs to be a _child_).
        """
        newpath = abspath(joinpath(self.path, normpath(path)))
        if not newpath.startswith(self.path):
            raise InsecurePath("%s is not a child of %s" % (newpath, self.path))
        return self.clonePath(newpath)
    def childSearchPreauth(self, *paths):
        """Return my first existing child with a name in 'paths'.
        paths is expected to be a list of *pre-secured* path fragments; in most
        cases this will be specified by a system administrator and not an
        arbitrary user.
        If no appropriately-named children exist, this will return None.
        """
        p = self.path
        for child in paths:
            jp = joinpath(p, child)
            if exists(jp):
                return self.clonePath(jp)
    def siblingExtensionSearch(self, *exts):
        """Attempt to return a path with my name, given multiple possible
        extensions.
        Each extension in exts will be tested and the first path which exists
        will be returned.  If no path exists, None will be returned.  If '' is
        in exts, then if the file referred to by this path exists, 'self' will
        be returned.
        The extension '*' has a magic meaning, which means "any path that
        begins with self.path+'.' is acceptable".
        """
        p = self.path
        for ext in exts:
            if not ext and self.exists():
                return self
            if ext == '*':
                basedot = basename(p)+'.'
                for fn in listdir(dirname(p)):
                    if fn.startswith(basedot):
                        return self.clonePath(joinpath(dirname(p), fn))
            p2 = p + ext
            if exists(p2):
                return self.clonePath(p2)
    def siblingExtension(self, ext):
        return self.clonePath(self.path+ext)
    def open(self, mode='r'):
        if self.alwaysCreate:
            assert 'a' not in mode, "Appending not supported when alwaysCreate == True"
            return self.create()
        return open(self.path, mode+'b')
    def restat(self, reraise=True):
        try:
            self.statinfo = stat(self.path)
        except OSError:
            self.statinfo = 0
            if reraise:
                raise
    def getsize(self):
        st = self.statinfo
        if not st:
            self.restat()
            st = self.statinfo
        return st[ST_SIZE]
    def getmtime(self):
        st = self.statinfo
        if not st:
            self.restat()
            st = self.statinfo
        return st[ST_MTIME]
    def getctime(self):
        st = self.statinfo
        if not st:
            self.restat()
            st = self.statinfo
        return st[ST_CTIME]
    def getatime(self):
        st = self.statinfo
        if not st:
            self.restat()
            st = self.statinfo
        return st[ST_ATIME]
    def exists(self):
        if self.statinfo:
            return True
        elif self.statinfo is None:
            self.restat(False)
            return self.exists()
        else:
            return False
    def isdir(self):
        st = self.statinfo
        if not st:
            self.restat(False)
            st = self.statinfo
            if not st:
                return False
        return S_ISDIR(st[ST_MODE])
    def isfile(self):
        st = self.statinfo
        if not st:
            self.restat(False)
            st = self.statinfo
            if not st:
                return False
        return S_ISREG(st[ST_MODE])
    def islink(self):
        st = self.statinfo
        if not st:
            self.restat(False)
            st = self.statinfo
            if not st:
                return False
        return S_ISLNK(st[ST_MODE])
    def isabs(self):
        return isabs(self.path)
    def listdir(self):
        return listdir(self.path)
    def splitext(self):
        return splitext(self.path)
    def __repr__(self):
        return 'FilePath(%r)' % self.path
    def touch(self):
        try:
            self.open('a').close()
        except IOError:
            pass
        utime(self.path, None)
    def remove(self):
        if self.isdir():
            for child in self.children():
                child.remove()
            os.rmdir(self.path)
        else:
            os.remove(self.path)
        self.restat(False)
    def makedirs(self):
        return os.makedirs(self.path)
    def globChildren(self, pattern):
        """
        Assuming I am representing a directory, return a list of
        FilePaths representing my children that match the given
        pattern.
        """
        import glob
        path = self.path[-1] == '/' and self.path + pattern or slash.join([self.path, pattern])
        return map(self.clonePath, glob.glob(path))
    def basename(self):
        return basename(self.path)
    def dirname(self):
        return dirname(self.path)
    def parent(self):
        return self.clonePath(self.dirname())
    def setContent(self, content, ext='.new'):
        sib = self.siblingExtension(ext)
        sib.open('w').write(content)
        os.rename(sib.path, self.path)
    def getContent(self):
        return self.open().read()
    def __cmp__(self, other):
        if not isinstance(other, FilePath):
            return NotImplemented
        return cmp(self.path, other.path)
    def createDirectory(self):
        os.mkdir(self.path)
    def requireCreate(self, val=1):
        self.alwaysCreate = val
    def create(self):
        """Exclusively create a file, only if this file previously did not exist.
        """
        fdint = os.open(self.path, (os.O_EXCL |
                                    os.O_CREAT |
                                    os.O_RDWR))
        return os.fdopen(fdint, 'w+b')
    def sibling(self, path):
        return self.parent().child(path)
    def temporarySibling(self):
        """
        Create a path naming a temporary sibling of this path in a secure fashion.
        """
        sib = self.sibling(_secureEnoughString() + self.basename())
        sib.requireCreate()
        return sib
    def children(self):
        return map(self.child, self.listdir())
    def walk(self):
        yield self
        if self.isdir():
            for c in self.children():
                for subc in c.walk():
                    yield subc
    _chunkSize = 2 ** 2 ** 2 ** 2
    def copyTo(self, destination):
        if self.isdir():
            if not destination.exists():
                destination.createDirectory()
            for child in self.children():
                destChild = destination.child(child.basename())
                child.copyTo(destChild)
        elif self.isfile():
            writefile = destination.open('w')
            readfile = self.open()
            while 1:
                chunk = readfile.read(self._chunkSize)
                writefile.write(chunk)
                if len(chunk) < self._chunkSize:
                    break
            writefile.close()
            readfile.close()
        else:
            raise NotImplementedError(
                "Only copying of files and directories supported")
    def moveTo(self, destination):
        try:
            os.rename(self.path, destination.path)
            self.restat(False)
        except OSError, ose:
            if ose.errno == errno.EXDEV:
                secsib = destination.temporarySibling()
                self.copyTo(secsib) # slow
                secsib.moveTo(destination) # visible
                mysecsib = self.temporarySibling()
                self.moveTo(mysecsib) # visible
                mysecsib.remove() # slow
            else:
                raise
FilePath.clonePath = FilePath
