import types
from itertools import count
from zope.interface import implements
from twisted.python import failure, log
from twisted.internet import defer, error, reactor
from twisted.pb import schema, banana, tokens, ipb
from twisted.pb import call, slicer, referenceable, copyable, remoteinterface
from twisted.pb.tokens import Violation, BananaError
from twisted.pb.ipb import DeadReferenceError
try:
    from twisted.pb import crypto
except ImportError:
    crypto = None
if crypto and not crypto.available:
    crypto = None
PBTopRegistry = {
    ("call",): call.CallUnslicer,
    ("answer",): call.AnswerUnslicer,
    ("error",): call.ErrorUnslicer,
    }
PBOpenRegistry = {
    ('my-reference',): referenceable.ReferenceUnslicer,
    ('your-reference',): referenceable.YourReferenceUnslicer,
    ('their-reference',): referenceable.TheirReferenceUnslicer,
    }
class PBRootUnslicer(slicer.RootUnslicer):
    topRegistry = [PBTopRegistry]
    openRegistry = [slicer.UnslicerRegistry, PBOpenRegistry]
    logViolations = False
    def checkToken(self, typebyte, size):
        if typebyte != tokens.OPEN:
            raise BananaError("top-level must be OPEN")
    def openerCheckToken(self, typebyte, size, opentype):
        if typebyte == tokens.STRING:
            if len(opentype) == 0:
                if size > self.maxIndexLength:
                    why = "first opentype STRING token is too long, %d>%d" % \
                          (size, self.maxIndexLength)
                    raise Violation(why)
            if opentype == ("copyable",):
                maxlen = reduce(max,
                                [len(cname) \
                                 for cname in copyable.CopyableRegistry.keys()]
                                )
                if size > maxlen:
                    why = "copyable-classname token is too long, %d>%d" % \
                          (size, maxlen)
                    raise Violation(why)
        elif typebyte == tokens.VOCAB:
            return
        else:
            raise Violation("index token 0x%02x not STRING or VOCAB" % \
                              ord(typebyte))
            raise BananaError("index token 0x%02x not STRING or VOCAB" % \
                              ord(typebyte))
    def open(self, opentype):
        assert len(self.protocol.receiveStack) > 1
        if opentype[0] == 'copyable':
            if len(opentype) > 1:
                classname = opentype[1]
                try:
                    factory = copyable.CopyableRegistry[classname]
                except KeyError:
                    raise Violation("unknown RemoteCopy class '%s'" \
                                    % classname)
                child = factory()
                child.broker = self.broker
                return child
            else:
                return None # still need classname
        for reg in self.openRegistry:
            opener = reg.get(opentype)
            if opener is not None:
                child = opener()
                break
        else:
            raise Violation("unknown OPEN type %s" % (opentype,))
        child.broker = self.broker
        return child
    def doOpen(self, opentype):
        child = slicer.RootUnslicer.doOpen(self, opentype)
        if child:
            child.broker = self.broker
        return child
    def reportViolation(self, f):
        if self.logViolations:
            print "hey, something failed:", f
        return None # absorb the failure
    def receiveChild(self, token, ready_deferred=None):
        pass
class PBRootSlicer(slicer.RootSlicer):
    slicerTable = {types.MethodType: referenceable.CallableSlicer,
                   types.FunctionType: referenceable.CallableSlicer,
                   }
    def registerReference(self, refid, obj):
        assert 0
    def slicerForObject(self, obj):
        s = tokens.ISlicer(obj, None)
        if s:
            return s
        copier = copyable.ICopyable(obj, None)
        if copier:
            s = tokens.ISlicer(copier)
            return s
        return slicer.RootSlicer.slicerForObject(self, obj)
class RIBroker(remoteinterface.RemoteInterface):
    def getReferenceByName(name=str):
        """If I have published an object by that name, return a reference to
        it."""
        return schema.Any()
    def decref(clid=int, count=int):
        """Release some references to my-reference 'clid'. I will return an
        ack when the operation has completed."""
        return schema.Nothing()
    def decgift(giftID=int, count=int):
        """Release some reference to a their-reference 'giftID' that was
        sent earlier."""
        return schema.Nothing()
class Broker(banana.Banana, referenceable.Referenceable):
    """I manage a connection to a remote Broker.
    @ivar tub: the L{PBService} which contains us
    @ivar yourReferenceByCLID: maps your CLID to a RemoteReferenceData
    @ivar yourReferenceByURL: maps a global URL to a RemoteReferenceData
    """
    implements(RIBroker)
    slicerClass = PBRootSlicer
    unslicerClass = PBRootUnslicer
    unsafeTracebacks = True
    requireSchema = False
    disconnected = False
    factory = None
    tub = None
    remote_broker = None
    startingTLS = False
    startedTLS = False
    def __init__(self, params={}):
        banana.Banana.__init__(self, params)
        self.initBroker()
    def initBroker(self):
        self.rootSlicer.broker = self
        self.rootUnslicer.broker = self
        self.nextCLID = count(1).next # 0 is for the broker
        self.myReferenceByPUID = {} # maps ref.processUniqueID to a tracker
        self.myReferenceByCLID = {} # maps CLID to a tracker
        self.yourReferenceByCLID = {}
        self.yourReferenceByURL = {}
        self.nextGiftID = count().next
        self.myGifts = {} # maps (broker,clid) to (rref, giftID, count)
        self.myGiftsByGiftID = {} # maps giftID to (broker,clid)
        self.nextReqID = count().next
        self.waitingForAnswers = {} # we wait for the other side to answer
        self.disconnectWatchers = []
        self.activeLocalCalls = {} # the other side wants an answer from us
    def setTub(self, tub):
        from twisted.pb import pb
        assert isinstance(tub, pb.PBService)
        self.tub = tub
    def connectionMade(self):
        banana.Banana.connectionMade(self)
        tracker = referenceable.RemoteReferenceTracker(self, 0, None,
                                                       "RIBroker")
        self.remote_broker = referenceable.RemoteReference(tracker)
    def connectionLost(self, why):
        self.disconnected = True
        self.remote_broker = None
        self.abandonAllRequests(why)
        self.myReferenceByPUID = {}
        self.myReferenceByCLID = {}
        self.yourReferenceByCLID = {}
        self.yourReferenceByURL = {}
        self.myGifts = {}
        self.myGiftsByGiftID = {}
        dw, self.disconnectWatchers = self.disconnectWatchers, []
        for d in dw:
            d()
        banana.Banana.connectionLost(self, why)
        if self.tub:
            self.tub.brokerDetached(self, why)
    def notifyOnDisconnect(self, callback):
        self.disconnectWatchers.append(callback)
    def dontNotifyOnDisconnect(self, callback):
        self.disconnectWatchers.remove(callback)
    def getRemoteInterfaceByName(self, name):
        return remoteinterfaces.RemoteInterfaceRegistry[name]
    def getTrackerForMyReference(self, puid, obj):
        tracker = self.myReferenceByPUID.get(puid)
        if not tracker:
            clid = self.nextCLID()
            tracker = referenceable.ReferenceableTracker(self.tub,
                                                         obj, puid, clid)
            self.myReferenceByPUID[puid] = tracker
            self.myReferenceByCLID[clid] = tracker
        return tracker
    def getTrackerForMyCall(self, puid, obj):
        tracker = self.myReferenceByPUID.get(puid)
        if not tracker:
            clid = self.nextCLID()
            clid = -clid
            tracker = referenceable.ReferenceableTracker(self.tub,
                                                         obj, puid, clid)
            self.myReferenceByPUID[puid] = tracker
            self.myReferenceByCLID[clid] = tracker
        return tracker
    def getTrackerForYourReference(self, clid, interfaceName=None, url=None):
        """The far end holds a Referenceable and has just sent us a reference
        to it (expressed as a small integer). If this is a new reference,
        they will give us an interface name too, and possibly a global URL
        for it. Obtain a RemoteReference object (creating it if necessary) to
        give to the local recipient.
        The sender remembers that we hold a reference to their object. When
        our RemoteReference goes away, we send a decref message to them, so
        they can possibly free their object. """
        assert type(interfaceName) is str or interfaceName is None
        if url is not None:
            assert type(url) is str
        tracker = self.yourReferenceByCLID.get(clid)
        if not tracker:
            if clid >= 0:
                trackerclass = referenceable.RemoteReferenceTracker
            else:
                trackerclass = referenceable.RemoteMethodReferenceTracker
            tracker = trackerclass(self, clid, url, interfaceName)
            self.yourReferenceByCLID[clid] = tracker
            if url:
                self.yourReferenceByURL[url] = tracker
        return tracker
    def freeYourReference(self, tracker, count):
        if not self.remote_broker: # tests do not set this up
            self.freeYourReferenceTracker(None, tracker)
            return
        try:
            d = self.remote_broker.callRemote("decref",
                                              clid=tracker.clid, count=count)
            d.addErrback(lambda f: f.trap(DeadReferenceError))
            d.addErrback(lambda f: f.trap(error.ConnectionLost))
            d.addErrback(lambda f: f.trap(error.ConnectionDone))
            d.addCallback(self.freeYourReferenceTracker, tracker)
        except:
            log.msg("failure during freeRemoteReference")
            log.err()
    def freeYourReferenceTracker(self, res, tracker):
        if tracker.received_count != 0:
            return
        if self.yourReferenceByCLID.has_key(tracker.clid):
            del self.yourReferenceByCLID[tracker.clid]
        if tracker.url and self.yourReferenceByURL.has_key(tracker.url):
            del self.yourReferenceByURL[tracker.url]
    def getMyReferenceByCLID(self, clid):
        """clid is the connection-local ID of the Referenceable the other
        end is trying to invoke or point to. If it is a number, they want an
        implicitly-created per-connection object that we sent to them at
        some point in the past. If it is a string, they want an object that
        was registered with our Factory.
        """
        obj = None
        assert type(clid) is int
        if clid == 0:
            return self
        return self.myReferenceByCLID[clid].obj
    def remote_decref(self, clid, count):
        assert type(clid) is int
        assert clid != 0
        tracker = self.myReferenceByCLID[clid]
        done = tracker.decref(count)
        if done:
            del self.myReferenceByPUID[tracker.puid]
            del self.myReferenceByCLID[clid]
    def makeGift(self, rref):
        broker, clid = rref.tracker.broker, rref.tracker.clid
        i = (broker, clid)
        old = self.myGifts.get(i)
        if old:
            rref, giftID, count = old
            self.myGifts[i] = (rref, giftID, count+1)
        else:
            giftID = self.nextGiftID()
            self.myGiftsByGiftID[giftID] = i
            self.myGifts[i] = (rref, giftID, 1)
        return giftID
    def remote_decgift(self, giftID, count):
        broker, clid = self.myGiftsByGiftID[giftID]
        rref, giftID, gift_count = self.myGifts[(broker, clid)]
        gift_count -= count
        if gift_count == 0:
            del self.myGiftsByGiftID[giftID]
            del self.myGifts[(broker, clid)]
        else:
            self.myGifts[(broker, clid)] = (rref, giftID, gift_count)
    def getYourReferenceByName(self, name):
        d = self.remote_broker.callRemote("getReferenceByName", name=name)
        return d
    def remote_getReferenceByName(self, name):
        return self.tub.getReferenceForName(name)
    def newRequestID(self):
        if self.disconnected:
            raise DeadReferenceError("Calling Stale Broker")
        return self.nextReqID()
    def addRequest(self, req):
        req.broker = self
        self.waitingForAnswers[req.reqID] = req
    def removeRequest(self, req):
        del self.waitingForAnswers[req.reqID]
    def getRequest(self, reqID):
        try:
            return self.waitingForAnswers[reqID]
        except KeyError:
            raise Violation("non-existent reqID '%d'" % reqID)
    def abandonAllRequests(self, why):
        for req in self.waitingForAnswers.values():
            req.fail(why)
        self.waitingForAnswers = {}
    def getRemoteInterfaceByName(self, riname):
        return remoteinterface.RemoteInterfaceRegistry[riname]
    def getSchemaForMethod(self, rifaces, methodname):
        for ri in rifaces:
            m = ri.get(methodname)
            if m:
                return m
        return None
    def doCall(self, reqID, obj, methodname, kwargs, methodSchema):
        if methodname is None:
            assert callable(obj)
            d = defer.maybeDeferred(obj, **kwargs)
        else:
            obj = ipb.IRemotelyCallable(obj)
            d = defer.maybeDeferred(obj.doRemoteCall, methodname, kwargs)
        d.addCallback(self._callFinished, reqID, methodSchema)
        d.addErrback(self.callFailed, reqID)
    def _callFinished(self, res, reqID, methodSchema):
        assert self.activeLocalCalls[reqID]
        if methodSchema:
            methodSchema.checkResults(res) # may raise Violation
        answer = call.AnswerSlicer(reqID, res)
        try:
            self.send(answer)
        except:
            log.err()
        del self.activeLocalCalls[reqID]
    def callFailed(self, f, reqID):
        assert self.activeLocalCalls[reqID]
        self.send(call.ErrorSlicer(reqID, f))
        del self.activeLocalCalls[reqID]
import debug
class LoggingBroker(debug.LoggingBananaMixin, Broker):
    pass
