import weakref
from zope.interface import interface
from zope.interface import implements, providedBy
from twisted.python.components import registerAdapter
Interface = interface.Interface
from twisted.internet import defer, error
from twisted.python import failure, log
from twisted.pb import ipb, schema, slicer, tokens, call
BananaError = tokens.BananaError
Violation = tokens.Violation
from twisted.pb.remoteinterface import getRemoteInterface, getRemoteInterfaceByName
from twisted.pb.copyable import Copyable, RemoteCopy
class OnlyReferenceable(object):
    implements(ipb.IReferenceable)
    def processUniqueID(self):
        return id(self)
class Referenceable(OnlyReferenceable):
    implements(ipb.IReferenceable, ipb.IRemotelyCallable)
    _interface = None
    _interfaceName = None
    def getInterface(self):
        if not self._interface:
            self._interface = getRemoteInterface(self)
            if self._interface:
                self._interfaceName = self._interface.__remote_name__
            else:
                self._interfaceName = None
        return self._interface
    def getInterfaceName(self):
        self.getInterface()
        return self._interfaceName
    def doRemoteCall(self, methodname, kwargs):
        meth = getattr(self, "remote_%s" % methodname)
        res = meth(**kwargs)
        return res
class ReferenceableTracker:
    """I hold the data which tracks a local Referenceable that is in used by
    a remote Broker.
    @ivar obj: the actual object
    @ivar refcount: the number of times this reference has been sent to the
                    remote end, minus the number of DECREF messages which it
                    has sent back. When it goes to zero, the remote end has
                    forgotten the RemoteReference, and is prepared to forget
                    the RemoteReferenceData as soon as the DECREF message is
                    acknowledged.
    @ivar clid: the connection-local ID used to represent this object on the
                wire.
    """
    def __init__(self, tub, obj, puid, clid):
        self.tub = tub
        self.obj = obj
        self.clid = clid
        self.puid = puid
        self.refcount = 0
    def send(self):
        """Increment the refcount.
        @return: True if this is the first transmission of the reference.
        """
        self.refcount += 1
        if self.refcount == 1:
            return True
    def getURL(self):
        if self.tub:
            return self.tub.getURLForReference(self.obj)
        return None
    def decref(self, count):
        """Call this in response to a DECREF message from the other end.
        @return: True if the refcount went to zero, meaning this clid should
        be retired.
        """
        assert self.refcount >= count, "decref(%d) but refcount was %d" % (count, self.refcount)
        self.refcount -= count
        if self.refcount == 0:
            return True
        return False
class ReferenceableSlicer(slicer.BaseSlicer):
    """I handle pb.Referenceable objects (things with remotely invokable
    methods, which are copied by reference).
    """
    opentype = ('my-reference',)
    def sliceBody(self, streamable, broker):
        puid = ipb.IReferenceable(self.obj).processUniqueID()
        tracker = broker.getTrackerForMyReference(puid, self.obj)
        yield tracker.clid
        firstTime = tracker.send()
        if firstTime:
            iname = ipb.IRemotelyCallable(self.obj).getInterfaceName()
            if iname:
                yield iname
            else:
                yield ""
            url = tracker.getURL()
            if url:
                yield url
registerAdapter(ReferenceableSlicer, Referenceable, ipb.ISlicer)
class CallableSlicer(slicer.BaseSlicer):
    """Bound methods are serialized as my-reference sequences with negative
    clid values."""
    opentype = ('my-reference',)
    def sliceBody(self, streamable, broker):
        puid = id(self.obj)
        tracker = broker.getTrackerForMyCall(puid, self.obj)
        yield tracker.clid
        firstTime = tracker.send()
        if firstTime:
            schema = self.getSchema()
            if schema:
                yield schema
            else:
                yield ""
            url = tracker.getURL()
            if url:
                yield url
    def getSchema(self):
        return None # TODO: not quite ready yet
        s = ipb.IReferenceable(self.obj.im_self, None)
        if s:
            return s.getSchemaForMethodNamed(self.obj.im_func.__name__)
        return getattr(self.obj, "schema", None)
class ReferenceUnslicer(slicer.BaseUnslicer):
    """I turn an incoming 'my-reference' sequence into a RemoteReference or a
    RemoteMethodReference."""
    state = 0
    clid = None
    interfaceName = None
    url = None
    inameConstraint = schema.StringConstraint(200) # TODO: only known RI names?
    urlConstraint = schema.StringConstraint(200)
    def checkToken(self, typebyte, size):
        if self.state == 0:
            if typebyte not in (tokens.INT, tokens.NEG):
                raise BananaError("reference ID must be an INT or NEG")
        elif self.state == 1:
            self.inameConstraint.checkToken(typebyte, size)
        elif self.state == 2:
            self.urlConstraint.checkToken(typebyte, size)
        else:
            raise Violation("too many parameters in my-reference")
    def receiveChild(self, obj, ready_deferred=None):
        assert not isinstance(obj, defer.Deferred)
        assert ready_deferred is None
        if self.state == 0:
            self.clid = obj
            self.state = 1
        elif self.state == 1:
            self.interfaceName = obj
            if obj == "":
                self.interfaceName = None
            self.state = 2
        elif self.state == 2:
            self.url = obj
            self.state = 3
        else:
            raise BananaError("Too many my-reference parameters")
    def receiveClose(self):
        if self.clid is None:
            raise BananaError("sequence ended too early")
        tracker = self.broker.getTrackerForYourReference(self.clid,
                                                         self.interfaceName,
                                                         self.url)
        return tracker.getRef(), None
    def describe(self):
        if self.clid is None:
            return "<ref-?>"
        return "<ref-%s>" % self.clid
class RemoteReferenceTracker:
    """I hold the data necessary to locate (or create) a RemoteReference.
    @ivar url: the target Referenceable's global URL
    @ivar broker: the Broker which holds this RemoteReference
    @ivar clid: for that Broker, the your-reference CLID for the
                RemoteReference
    @ivar interfaceName: the name of a RemoteInterface object that the
                         RemoteReference claims to implement
    @ivar interface: our version of a RemoteInterface object that corresponds
                     to interfaceName
    @ivar received_count: the number of times the remote end has send us this
                          object. We must send back decref() calls to match.
    @ivar ref: a weakref to the RemoteReference itself
    """
    def __init__(self, parent, clid, url, interfaceName):
        self.broker = parent
        self.clid = clid
        self.url = url
        self.interfaceName = interfaceName
        self.interface = getRemoteInterfaceByName(interfaceName)
        self.received_count = 0
        self.ref = None
    def __repr__(self):
        s = "<RemoteReferenceTracker(clid=%d,url=%s)>" % (self.clid, self.url)
        return s
    def getRef(self):
        """Return the actual RemoteReference that we hold, creating it if
        necessary."""
        if self.ref is None:
            ref = RemoteReference(self)
            self.ref = weakref.ref(ref, self._refLost)
        self.received_count += 1
        return self.ref()
    def _refLost(self, wref):
        count, self.received_count = self.received_count, 0
        self.broker.freeYourReference(self, count)
class RemoteReferenceOnly(object):
    def __init__(self, tracker):
        """@param tracker: the RemoteReferenceTracker which points to us"""
        self.tracker = tracker
    def getSturdyRef(self):
        return self.tracker.sturdy
    def notifyOnDisconnect(self, callback):
        self.tracker.broker.notifyOnDisconnect(callback)
    def dontNotifyOnDisconnect(self, callback):
        self.tracker.broker.dontNotifyOnDisconnect(callback)
    def __repr__(self):
        r = "<%s at 0x%x" % (self.__class__.__name__, abs(id(self)))
        if self.tracker.url:
            r += " [%s]" % self.tracker.url
        r += ">"
        return r
class RemoteReference(RemoteReferenceOnly):
    def callRemote(self, _name, *args, **kwargs):
        req = None
        broker = self.tracker.broker
        methodConstraintOverride = kwargs.get("_methodConstraint", "none")
        resultConstraint = kwargs.get("_resultConstraint", "none")
        useSchema = kwargs.get("_useSchema", True)
        if "_methodConstraint" in kwargs:
            del kwargs["_methodConstraint"]
        if "_resultConstraint" in kwargs:
            del kwargs["_resultConstraint"]
        if "_useSchema" in kwargs:
            del kwargs["_useSchema"]
        try:
            reqID = broker.newRequestID()
        except:
            return defer.fail()
        try:
            req = call.PendingRequest(reqID, self)
            (methodName, methodSchema) = self._getMethodInfo(_name)
            req.methodName = methodName # for debugging
            if methodConstraintOverride != "none":
                methodSchema = methodConstraintOverride
            if useSchema and methodSchema:
                argsdict = methodSchema.mapArguments(args, kwargs)
                methodSchema.checkAllArgs(kwargs)
                req.setConstraint(methodSchema.getResponseConstraint())
            else:
                if args:
                    why = "positional arguments require a RemoteInterface"
                    why += " for %s.%s()" % (self, methodName)
                    raise tokens.BananaError(why)
                argsdict = kwargs
            if resultConstraint != "none":
                req.setConstraint(schema.makeConstraint(resultConstraint))
        except: # TODO: merge this with the next try/except clause
            req.fail(failure.Failure())
            return req.deferred
        try:
            self.tracker.broker.addRequest(req)
            slicer = call.CallSlicer(reqID, self.tracker.clid,
                                     methodName, argsdict)
            d = broker.send(slicer)
        except:
            req.fail(failure.Failure())
            return req.deferred
        d.addErrback(req.fail)
        return req.deferred
    def _getMethodInfo(self, name):
        assert type(name) is str
        methodName = name
        methodSchema = None
        iface = self.tracker.interface
        if iface:
            interfaceName = iface.__remote_name__
            try:
                methodSchema = iface[name]
            except KeyError:
                raise Violation("%s(%s) does not offer %s" % \
                                (interfaceName, self, name))
        return methodName, methodSchema
class RemoteMethodReferenceTracker(RemoteReferenceTracker):
    def getRef(self):
        if self.ref is None:
            ref = RemoteMethodReference(self)
            self.ref = weakref.ref(ref, self._refLost)
        self.received_count += 1
        return self.ref()
class RemoteMethodReference(RemoteReference):
    def callRemote(self, *args, **kwargs):
        assert args == ()
        return RemoteReference.callRemote(self, "", *args, **kwargs)
    def _getMethodInfo(self, name):
        methodName = ""
        methodSchema = None
        return methodName, methodSchema
class YourReferenceSlicer(slicer.BaseSlicer):
    """I handle pb.RemoteReference objects (being sent back home to the
    original pb.Referenceable-holder)
    """
    def slice(self, streamable, broker):
        self.streamable = streamable
        tracker = self.obj.tracker
        if tracker.broker == broker:
            yield 'your-reference'
            yield tracker.clid
        else:
            giftID = broker.makeGift(self.obj)
            yield 'their-reference'
            yield giftID
            yield tracker.url
    def describe(self):
        return "<your-ref-%s>" % self.obj.tracker.clid
registerAdapter(YourReferenceSlicer, RemoteReference, ipb.ISlicer)
class YourReferenceUnslicer(slicer.LeafUnslicer):
    """I accept incoming (integer) your-reference sequences and try to turn
    them back into the original Referenceable. I also accept (string)
    your-reference sequences and try to turn them into a published
    Referenceable that they did not have access to before."""
    clid = None
    def checkToken(self, typebyte, size):
        if typebyte != tokens.INT:
            raise BananaError("your-reference ID must be an INT")
    def receiveChild(self, obj, ready_deferred=None):
        assert not isinstance(obj, defer.Deferred)
        assert ready_deferred is None
        self.clid = obj
    def receiveClose(self):
        if self.clid is None:
            raise BananaError("sequence ended too early")
        obj = self.broker.getMyReferenceByCLID(self.clid)
        if not obj:
            raise Violation("unknown clid '%s'" % self.clid)
        return obj, None
    def describe(self):
        return "<your-ref-%s>" % self.obj.refID
class TheirReferenceUnslicer(slicer.LeafUnslicer):
    """I accept gifts of third-party references. This is turned into a live
    reference upon receipt."""
    state = 0
    giftID = None
    url = None
    urlConstraint = schema.StringConstraint(200)
    def checkToken(self, typebyte, size):
        if self.state == 0:
            if typebyte != tokens.INT:
                raise BananaError("their-reference giftID must be an INT")
        elif self.state == 1:
            self.urlConstraint.checkToken(typebyte, size)
        else:
            raise Violation("too many parameters in their-reference")
    def receiveChild(self, obj, ready_deferred=None):
        assert not isinstance(obj, defer.Deferred)
        assert ready_deferred is None
        if self.state == 0:
            self.giftID = obj
            self.state = 1
        elif self.state == 1:
            self.url = obj
            self.state = 2
        else:
            raise BananaError("Too many their-reference parameters")
    def receiveClose(self):
        if self.giftID is None or self.url is None:
            raise BananaError("sequence ended too early")
        d = self.broker.tub.getReference(self.url)
        d.addBoth(self.ackGift)
        return d,d
    def ackGift(self, rref):
        d = self.broker.remote_broker.callRemote("decgift",
                                                 giftID=self.giftID, count=1)
        d.addErrback(lambda f: f.trap(ipb.DeadReferenceError))
        d.addErrback(lambda f: f.trap(error.ConnectionLost))
        d.addErrback(lambda f: f.trap(error.ConnectionDone))
        return rref
    def describe(self):
        if self.giftID is None:
            return "<gift-?>"
        return "<gift-%s>" % self.giftID
class SturdyRef(Copyable, RemoteCopy):
    """I am a pointer to a Referenceable that lives in some (probably remote)
    Tub. This pointer is long-lived, however you cannot send messages with it
    directly. To use it, you must ask your Tub to turn it into a
    RemoteReference with tub.getReference(sturdyref).
    The SturdyRef is associated with a URL: you can create a SturdyRef out of
    a URL that you obtain from some other source, and you can ask the
    SturdyRef for its URL.
    SturdyRefs are serialized by copying their URL, and create an identical
    SturdyRef on the receiving side."""
    encrypted = False
    tubID = None
    location = None
    locationHints = []
    name = None
    def __init__(self, url=None):
        if url:
            if url.startswith("pb://"):
                self.encrypted = True
                url = url[len("pb://"):]
                slash = url.rfind("/")
                self.name = url[slash+1:]
                at = url.find("@")
                if at != -1:
                    self.tubID = url[:at]
                self.locationHints = url[at+1:slash].split(",")
            elif url.startswith("pbu://"):
                self.encrypted = False
                url = url[len("pbu://"):]
                slash = url.rfind("/")
                self.name = url[slash+1:]
                self.tubID = None
                self.location = url[:slash]
            else:
                raise ValueError("unknown PB-URL prefix in '%s'" % url)
    def getTubRef(self):
        if self.encrypted:
            return TubRef(self.tubID, self.locationHints)
        return NoAuthTubRef(self.location)
    def getURL(self):
        if self.encrypted:
            return ("pb://" + self.tubID + "@" +
                    ",".join(self.locationHints) +
                    "/" + self.name)
        return "pbu://" + self.location + "/" + self.name
    def __str__(self):
        return self.getURL()
    def _distinguishers(self):
        """Two SturdyRefs are equivalent if they point to the same object.
        SturdyRefs to encrypted Tubs only pay attention to the TubID and the
        reference name. SturdyRefs to unencrypted Tubs must use the location
        hint instead of the (missing) TubID. This method makes it easier to
        compare a pair of SturdyRefs."""
        if self.encrypted:
            return (True, self.tubID, self.name)
        return (False, self.location, self.name)
    def __hash__(self):
        return hash(self._distinguishers())
    def __cmp__(self, them):
        return (cmp(type(self), type(them)) or
                cmp(self.__class__, them.__class__) or
                cmp(self._distinguishers(), them._distinguishers()))
    def asLiveRef(self):
        """Return an object that can be sent over the wire and unserialized
        as a live RemoteReference on the far end. Use this when you have a
        SturdyRef and want to give someone a reference to its target, but
        when you haven't bothered to acquire your own live reference to it."""
        return _AsLiveRef(self)
class _AsLiveRef:
    implements(ipb.ISlicer)
    def __init__(self, sturdy):
        self.target = sturdy
    def slice(self, streamable, banana):
        yield 'their-reference'
        yield giftID
        yield self.target.getURL()
        yield [] # interfacenames
class TubRef:
    """This is a little helper class which provides a comparable identifier
    for Tubs. TubRefs can be used as keys in dictionaries that track
    connections to remote Tubs."""
    encrypted = True
    def __init__(self, tubID, locationHints=None):
        self.tubID = tubID
        self.locationHints = locationHints
    def getLocations(self):
        return self.locationHints
    def __str__(self):
        return "pb://" + self.tubID
    def _distinguishers(self):
        """This serves the same purpose as SturdyRef._distinguishers."""
        return (self.tubID,)
    def __hash__(self):
        return hash(self._distinguishers())
    def __cmp__(self, them):
        return (cmp(type(self), type(them)) or
                cmp(self.__class__, them.__class__) or
                cmp(self._distinguishers(), them._distinguishers()))
class NoAuthTubRef(TubRef):
    encrypted = False
    def __init__(self, location):
        self.location = location
    def getLocations(self):
        return [self.location]
    def __str__(self):
        return "pbu://" + self.location
    def _distinguishers(self):
        """This serves the same purpose as SturdyRef._distinguishers."""
        return (self.location,)
