"""Address objects for network connections."""
import warnings, os
from zope.interface import implements
from twisted.python import components
from twisted.internet.interfaces import IAddress
class IPv4Address(object):
    """
    Object representing an IPv4 socket endpoint.
    @ivar type: A string describing the type of transport, either 'TCP' or 'UDP'.
    @ivar host: A string containing the dotted-quad IP address.
    @ivar port: An integer representing the port number.
    """
    implements(IAddress)
    def __init__(self, type, host, port, _bwHack = None):
        assert type in ('TCP', 'UDP')
        self.type = type
        self.host = host
        self.port = port
        self._bwHack = _bwHack
    def __getitem__(self, index):
        warnings.warn("IPv4Address.__getitem__ is deprecated.  Use attributes instead.",
                      category=DeprecationWarning, stacklevel=2)
        return (self._bwHack or self.type, self.host, self.port).__getitem__(index)
    def __getslice__(self, start, stop):
        warnings.warn("IPv4Address.__getitem__ is deprecated.  Use attributes instead.",
                      category=DeprecationWarning, stacklevel=2)
        return (self._bwHack or self.type, self.host, self.port)[start:stop]
    def __eq__(self, other):
        if isinstance(other, tuple):
            return tuple(self) == other
        elif isinstance(other, IPv4Address):
            a = (self.type, self.host, self.port)
            b = (other.type, other.host, other.port)
            return a == b
        return False
    def __str__(self):
        return 'IPv4Address(%s, %r, %d)' % (self.type, self.host, self.port)
class UNIXAddress(object):
    """
    Object representing a UNIX socket endpoint.
    @ivar name: The filename associated with this socket.
    @type name: C{str}
    """
    implements(IAddress)
    def __init__(self, name, _bwHack='UNIX'):
        self.name = name
        self._bwHack = _bwHack
    def __getitem__(self, index):
        warnings.warn("UNIXAddress.__getitem__ is deprecated.  Use attributes instead.",
                      category=DeprecationWarning, stacklevel=2)
        return (self._bwHack, self.name).__getitem__(index)
    def __getslice__(self, start, stop):
        warnings.warn("UNIXAddress.__getitem__ is deprecated.  Use attributes instead.",
                      category=DeprecationWarning, stacklevel=2)
        return (self._bwHack, self.name)[start:stop]
    def __eq__(self, other):
        if isinstance(other, tuple):
            return tuple(self) == other
        elif isinstance(other, UNIXAddress):
            try:
                return os.path.samefile(self.name, other.name)
            except OSError:
                pass
        return False
    def __str__(self):
        return 'UNIXSocket(%r)' % (self.name,)
components.backwardsCompatImplements(IPv4Address)
components.backwardsCompatImplements(UNIXAddress)
class _ServerFactoryIPv4Address(IPv4Address):
    """Backwards compatability hack. Just like IPv4Address in practice."""
    def __eq__(self, other):
        if isinstance(other, tuple):
            warnings.warn("IPv4Address.__getitem__ is deprecated.  Use attributes instead.",
                          category=DeprecationWarning, stacklevel=2)
            return (self.host, self.port) == other
        elif isinstance(other, IPv4Address):
            a = (self.type, self.host, self.port)
            b = (other.type, other.host, other.port)
            return a == b
        return False
