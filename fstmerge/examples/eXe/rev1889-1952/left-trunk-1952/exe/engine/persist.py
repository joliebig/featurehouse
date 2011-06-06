"""
Functions for handling persistance in eXe
"""
import logging
import cStringIO
from twisted.persisted.styles import Versioned, doUpgrade
from twisted.spread  import jelly
from twisted.spread  import banana
log = logging.getLogger(__name__)
class Persistable(object, jelly.Jellyable, jelly.Unjellyable, Versioned):
    """
    Base class for persistent classes
    """
    nonpersistant = []
    def getStateFor(self, jellier):
        """
        Call Versioned.__getstate__ to store
        persistenceVersion etc...
        """
        return self.__getstate__()
    def __getstate__(self):
        """
        Return which variables we should persist
        """
        toPersist = dict([(key, value) for key, value in self.__dict__.items()
                          if key not in self.nonpersistant])
        return Versioned.__getstate__(self, toPersist)
def encodeObject(toEncode):
    """
    Take a object and turn it into an string
    """
    log.debug(u"encodeObject")
    encoder = banana.Banana()
    encoder.connectionMade()
    encoder._selectDialect(u"none")
    strBuffer = cStringIO.StringIO()
    encoder.transport = strBuffer
    encoder.sendEncoded(jelly.jelly(toEncode))
    return strBuffer.getvalue()
def decodeObjectRaw(toDecode):
    """
    Decodes the object the same as decodeObject but doesn't upgrade it.
    """
    log.debug(u"decodeObject")
    decoder = banana.Banana()
    decoder.connectionMade()
    decoder._selectDialect(u"none")
    data = []
    decoder.expressionReceived = data.append
    decoder.dataReceived(toDecode)
    decoded = jelly.unjelly(data[0])
    return decoded
def decodeObject(toDecode):
    """
    Take a string and turn it into an object
    """
    decoded = decodeObjectRaw(toDecode)
    doUpgrade()
    return decoded
