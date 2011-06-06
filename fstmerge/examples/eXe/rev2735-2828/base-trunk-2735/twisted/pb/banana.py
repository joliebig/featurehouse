import types, struct
import rfc822 # for version negotiation headers
from cStringIO import StringIO
from twisted.internet import protocol, error, defer
from twisted.python.failure import Failure
from twisted.python import log
import slicer, tokens
from tokens import SIZE_LIMIT, STRING, LIST, INT, NEG, \
     LONGINT, LONGNEG, VOCAB, FLOAT, OPEN, CLOSE, ABORT, ERROR, \
     BananaError, BananaFailure, Violation
def int2b128(integer, stream):
    if integer == 0:
        stream(chr(0))
        return
    assert integer > 0, "can only encode positive integers"
    while integer:
        stream(chr(integer & 0x7f))
        integer = integer >> 7
def b1282int(st):
    oneHundredAndTwentyEight = 128
    i = 0
    place = 0
    for char in st:
        num = ord(char)
        i = i + (num * (oneHundredAndTwentyEight ** place))
        place = place + 1
    return i
def long_to_bytes(n, blocksize=0):
    """long_to_bytes(n:long, blocksize:int) : string
    Convert a long integer to a byte string.
    If optional blocksize is given and greater than zero, pad the front of
    the byte string with binary zeros so that the length is a multiple of
    blocksize.
    """
    s = ''
    n = long(n)
    pack = struct.pack
    while n > 0:
        s = pack('>I', n & 0xffffffffL) + s
        n = n >> 32
    for i in range(len(s)):
        if s[i] != '\000':
            break
    else:
        s = '\000'
        i = 0
    s = s[i:]
    if blocksize > 0 and len(s) % blocksize:
        s = (blocksize - len(s) % blocksize) * '\000' + s
    return s
def bytes_to_long(s):
    """bytes_to_long(string) : long
    Convert a byte string to a long integer.
    This is (essentially) the inverse of long_to_bytes().
    """
    acc = 0L
    unpack = struct.unpack
    length = len(s)
    if length % 4:
        extra = (4 - length % 4)
        s = '\000' * extra + s
        length = length + extra
    for i in range(0, length, 4):
        acc = (acc << 32) + unpack('>I', s[i:i+4])[0]
    return acc
HIGH_BIT_SET = chr(0x80)
class Banana(protocol.Protocol):
    def __init__(self, features={}):
        """
        @param features: a dictionary of negotiated connection features
        """
        self.initSend()
        self.initReceive()
    def connectionMade(self):
        if self.debugSend:
            print "Banana.connectionMade"
        self.produce()
    slicerClass = slicer.RootSlicer
    paused = False
    streamable = True # this is only checked during __init__
    debugSend = False
    def initSend(self):
        self.rootSlicer = self.slicerClass(self)
        self.rootSlicer.allowStreaming(self.streamable)
        assert tokens.ISlicer.providedBy(self.rootSlicer)
        assert tokens.IRootSlicer.providedBy(self.rootSlicer)
        itr = self.rootSlicer.slice()
        next = iter(itr).next
        top = (self.rootSlicer, next, None)
        self.slicerStack = [top]
        self.openCount = 0
        self.outgoingVocabulary = {}
    def send(self, obj):
        if self.debugSend: print "Banana.send(%s)" % obj
        return self.rootSlicer.send(obj)
    def produce(self, dummy=None):
        while self.slicerStack and not self.paused:
            if self.debugSend: print "produce.loop"
            try:
                slicer, next, openID = self.slicerStack[-1]
                obj = next()
                if self.debugSend: print " produce.obj=%s" % (obj,)
                if isinstance(obj, defer.Deferred):
                    for s,n,o in self.slicerStack:
                        if not s.streamable:
                            raise Violation("parent not streamable")
                    obj.addCallback(self.produce)
                    obj.addErrback(self.sendFailed) # what could cause this?
                    break
                elif type(obj) in (int, long, float, str):
                    self.sendToken(obj)
                else:
                    try:
                        slicer = self.newSlicerFor(obj)
                        self.pushSlicer(slicer, obj)
                    except Violation, v:
                        f = BananaFailure()
                        if self.debugSend:
                            print " violation in newSlicerFor:", f
                        self.handleSendViolation(f,
                                                 doPop=False, sendAbort=False)
            except StopIteration:
                if self.debugSend: print "StopIteration"
                self.popSlicer()
            except Violation, v:
                if self.debugSend: print " violation in .next:", v
                f = BananaFailure()
                self.handleSendViolation(f, doPop=True, sendAbort=True)
            except:
                print "exception in produce"
                self.sendFailed(Failure())
                return
        assert self.slicerStack # should never be empty
    def handleSendViolation(self, f, doPop, sendAbort):
        f.value.setLocation(self.describeSend())
        while True:
            top = self.slicerStack[-1][0]
            if self.debugSend:
                print " handleSendViolation.loop, top=%s" % top
            if sendAbort:
                lastOpenID = self.slicerStack[-1][2]
                if lastOpenID is not None:
                    if self.debugSend:
                        print "  sending ABORT(%s)" % lastOpenID
                    self.sendAbort(lastOpenID)
            if doPop:
                if self.debugSend: print "  popping %s" % top
                self.popSlicer()
                if not self.slicerStack:
                    if self.debugSend: print "RootSlicer died!"
                    raise BananaError("Hey! You killed the RootSlicer!")
                top = self.slicerStack[-1][0]
            if self.debugSend:
                print "  notifying parent", top
            f = top.childAborted(f)
            if f:
                doPop = True
                sendAbort = True
                continue
            else:
                break
    def newSlicerFor(self, obj):
        if tokens.ISlicer.providedBy(obj):
            return obj
        topSlicer = self.slicerStack[-1][0]
        return topSlicer.slicerForObject(obj)
    def pushSlicer(self, slicer, obj):
        if self.debugSend: print "push", slicer
        assert len(self.slicerStack) < 10000 # failsafe
        topSlicer = self.slicerStack[-1][0]
        slicer.parent = topSlicer
        itr = slicer.slice(topSlicer.streamable, self)
        next = iter(itr).next
        openID = None
        if slicer.sendOpen:
            openID = self.sendOpen()
            if slicer.trackReferences:
                topSlicer.registerReference(openID, obj)
        slicertuple = (slicer, next, openID)
        self.slicerStack.append(slicertuple)
    def popSlicer(self):
        slicertuple = self.slicerStack.pop()
        openID = slicertuple[2]
        if openID is not None:
            self.sendClose(openID)
        if self.debugSend: print "pop", slicertuple[0]
    def describeSend(self):
        where = []
        for i in self.slicerStack:
            try:
                piece = i[0].describe()
            except:
                log.msg("Banana.describeSend")
                log.err()
                piece = "???"
            where.append(piece)
        return ".".join(where)
    def setOutgoingVocabulary(self, vocabDict):
        for key,value in vocabDict.items():
            assert(isinstance(key, types.IntType))
            assert(isinstance(value, types.StringType))
        s = slicer.VocabSlicer(vocabDict)
        self.outgoingVocabulary = {}
        self.send(s)
        self.outgoingVocabulary = dict(zip(vocabDict.values(),
                                           vocabDict.keys()))
    def sendOpen(self):
        openID = self.openCount
        self.openCount += 1
        int2b128(openID, self.transport.write)
        self.transport.write(OPEN)
        return openID
    def sendToken(self, obj):
        write = self.transport.write
        if isinstance(obj, types.IntType) or isinstance(obj, types.LongType):
            if obj >= 2**31:
                s = long_to_bytes(obj)
                int2b128(len(s), write)
                write(LONGINT)
                write(s)
            elif obj >= 0:
                int2b128(obj, write)
                write(INT)
            elif -obj > 2**31: # NEG is [-2**31, 0)
                s = long_to_bytes(-obj)
                int2b128(len(s), write)
                write(LONGNEG)
                write(s)
            else:
                int2b128(-obj, write)
                write(NEG)
        elif isinstance(obj, types.FloatType):
            write(FLOAT)
            write(struct.pack("!d", obj))
        elif isinstance(obj, types.StringType):
            if self.outgoingVocabulary.has_key(obj):
                symbolID = self.outgoingVocabulary[obj]
                int2b128(symbolID, write)
                write(VOCAB)
            else:
                int2b128(len(obj), write)
                write(STRING)
                write(obj)
        else:
            raise BananaError, "could not send object: %s" % repr(obj)
    def sendClose(self, openID):
        int2b128(openID, self.transport.write)
        self.transport.write(CLOSE)
    def sendAbort(self, count=0):
        int2b128(count, self.transport.write)
        self.transport.write(ABORT)
    def sendError(self, msg):
        if len(msg) > SIZE_LIMIT:
            raise BananaError, \
                  "error string is too long to send (%d)" % len(msg)
        int2b128(len(msg), self.transport.write)
        self.transport.write(ERROR)
        self.transport.write(msg)
        self.transport.loseConnection()
    def sendFailed(self, f):
        print "SendBanana.sendFailed:", f
        log.msg("Sendfailed.sendfailed")
        log.err(f)
        try:
            if self.transport:
                self.transport.loseConnection()
        except:
            print "exception during transport.loseConnection"
            log.err()
        try:
            self.rootSlicer.connectionLost(f)
        except:
            print "exception during rootSlicer.connectionLost"
            log.err()
    unslicerClass = slicer.RootUnslicer
    debugReceive = False
    logViolations = False
    logReceiveErrors = True
    def initReceive(self):
        self.rootUnslicer = self.unslicerClass()
        self.rootUnslicer.protocol = self
        self.receiveStack = [self.rootUnslicer]
        self.objectCounter = 0
        self.objects = {}
        self.inOpen = False # set during the Index Phase of an OPEN sequence
        self.opentype = [] # accumulates Index Tokens
        self.negotiated = False
        self.connectionAbandoned = False
        self.buffer = ''
        self.incomingVocabulary = {}
        self.skipBytes = 0 # used to discard a single long token
        self.discardCount = 0 # used to discard non-primitive objects
        self.exploded = None # last-ditch error catcher
    def printStack(self, verbose=0):
        print "STACK:"
        for s in self.receiveStack:
            if verbose:
                d = s.__dict__.copy()
                del d['protocol']
                print " %s: %s" % (s, d)
            else:
                print " %s" % s
    def setObject(self, count, obj):
        for i in range(len(self.receiveStack)-1, -1, -1):
            self.receiveStack[i].setObject(count, obj)
    def getObject(self, count):
        for i in range(len(self.receiveStack)-1, -1, -1):
            obj = self.receiveStack[i].getObject(count)
            if obj is not None:
                return obj
        raise ValueError, "dangling reference '%d'" % count
    def setIncomingVocabulary(self, vocabDict):
        self.incomingVocabulary = vocabDict
    def dataReceived(self, chunk):
        if self.connectionAbandoned:
            return
        try:
            self.handleData(chunk)
        except Exception, e:
            if isinstance(e, BananaError):
                e.where = self.describeReceive()
                msg = str(e) # send them the text of the error
            else:
                msg = ("exception while processing data, more "
                       "information in the logfiles")
                if not self.logReceiveErrors:
                    msg += ", except that self.logReceiveErrors=False"
                    msg += ", sucks to be you"
            self.sendError(msg)
            self.reportReceiveError(Failure())
            self.connectionAbandoned = True
    def reportReceiveError(self, f):
        log.msg("Banana.reportReceiveError: an error occured during receive")
        if self.logReceiveErrors:
            log.err(f)
        if self.debugReceive:
            log.msg(f.getBriefTraceback())
    def handleData(self, chunk):
        if self.skipBytes:
            if len(chunk) < self.skipBytes:
                self.skipBytes -= len(chunk)
                return
            chunk = chunk[self.skipBytes:]
            self.skipBytes = 0
        buffer = self.buffer + chunk
        while buffer:
            assert self.buffer != buffer, \
                   ("Banana.handleData: no progress made: %s %s" %
                    (repr(buffer),))
            self.buffer = buffer
            pos = 0
            for ch in buffer:
                if ch >= HIGH_BIT_SET:
                    break
                pos = pos + 1
            else:
                if pos > 64:
                    raise BananaError("token prefix is limited to 64 bytes")
                return # still waiting for header to finish
            typebyte = buffer[pos]
            if pos > 64:
                raise BananaError("token prefix is limited to 64 bytes")
            if pos:
                header = b1282int(buffer[:pos])
            else:
                header = 0
            rejected = False
            if self.discardCount:
                rejected = True
            wasInOpen = self.inOpen
            if typebyte == OPEN:
                if self.inOpen:
                    raise BananaError("OPEN token followed by OPEN")
                self.inOpen = True
            if (not rejected) and (typebyte not in (ABORT, CLOSE, ERROR)):
                try:
                    top = self.receiveStack[-1]
                    if wasInOpen:
                        top.openerCheckToken(typebyte, header, self.opentype)
                    else:
                        top.checkToken(typebyte, header)
                except Violation, v:
                    rejected = True
                    f = BananaFailure()
                    if wasInOpen:
                        methname = "openerCheckToken"
                    else:
                        methname = "checkToken"
                    self.handleViolation(f, methname, inOpen=self.inOpen)
                    self.inOpen = False
            if typebyte == ERROR and header > SIZE_LIMIT:
                raise BananaError("oversized ERROR token")
            rest = buffer[pos+1:]
            if typebyte == OPEN:
                buffer = rest
                self.inboundOpenCount = header
                if rejected:
                    if self.debugReceive:
                        print "DROP (OPEN)"
                    if self.inOpen:
                        self.discardCount += 1
                        if self.debugReceive:
                            print "++discardCount (OPEN), now %d" \
                                  % self.discardCount
                        self.inOpen = False
                    else:
                        pass
                else:
                    self.inOpen = True
                    self.opentype = []
                continue
            elif typebyte == CLOSE:
                buffer = rest
                count = header
                if self.discardCount:
                    self.discardCount -= 1
                    if self.debugReceive:
                        print "--discardCount (CLOSE), now %d" \
                              % self.discardCount
                else:
                    self.handleClose(count)
                continue
            elif typebyte == ABORT:
                buffer = rest
                count = header
                if rejected:
                    if self.debugReceive:
                        print "DROP (ABORT)"
                    continue
                try:
                    raise Violation("ABORT received")
                except Violation:
                    f = BananaFailure()
                    self.handleViolation(f, "receive-abort")
                continue
            elif typebyte == ERROR:
                strlen = header
                if len(rest) >= strlen:
                    buffer = rest[strlen:]
                    obj = rest[:strlen]
                    self.handleError(obj)
                    return
                else:
                    return # there is more to come
            elif typebyte == LIST:
                raise BananaError("oldbanana peer detected, " +
                                  "compatibility code not yet written")
            elif typebyte == STRING:
                strlen = header
                if len(rest) >= strlen:
                    buffer = rest[strlen:]
                    obj = rest[:strlen]
                else:
                    if rejected:
                        if self.debugReceive:
                            print "DROPPED some string bits"
                        self.skipBytes = strlen - len(rest)
                        self.buffer = ""
                    return
            elif typebyte == INT:
                buffer = rest
                obj = int(header)
            elif typebyte == NEG:
                buffer = rest
                obj = int(-long(header))
            elif typebyte == LONGINT or typebyte == LONGNEG:
                strlen = header
                if len(rest) >= strlen:
                    buffer = rest[strlen:]
                    obj = bytes_to_long(rest[:strlen])
                    if typebyte == LONGNEG:
                        obj = -obj
                else:
                    if rejected:
                        self.skipBytes = strlen - len(rest)
                        self.buffer = ""
                    return
            elif typebyte == VOCAB:
                buffer = rest
                obj = self.incomingVocabulary[header]
            elif typebyte == FLOAT:
                if len(rest) >= 8:
                    buffer = rest[8:]
                    obj = struct.unpack("!d", rest[:8])[0]
                else:
                    return
            else:
                raise BananaError("Invalid Type Byte 0x%x" % ord(typebyte))
            if not rejected:
                if self.inOpen:
                    self.handleOpen(self.inboundOpenCount, obj)
                else:
                    self.handleToken(obj)
            else:
                if self.debugReceive:
                    print "DROP", type(obj), obj
                pass # drop the object
        self.buffer = ''
    def handleOpen(self, openCount, indexToken):
        self.opentype.append(indexToken)
        opentype = tuple(self.opentype)
        if self.debugReceive:
            print "handleOpen(%d,%s)" % (openCount, indexToken)
        objectCount = self.objectCounter
        top = self.receiveStack[-1]
        try:
            child = top.doOpen(opentype)
            if not child:
                if self.debugReceive:
                    print " doOpen wants more index tokens"
                return # they want more index tokens, leave .inOpen=True
            if self.debugReceive:
                print " opened[%d] with %s" % (openCount, child)
        except Violation, v:
            self.inOpen = False
            f = BananaFailure()
            self.handleViolation(f, "doOpen", inOpen=True)
            return
        assert tokens.IUnslicer.providedBy(child), "child is %s" % child
        self.objectCounter += 1
        self.inOpen = False
        child.protocol = self
        child.openCount = openCount
        child.parent = top
        self.receiveStack.append(child)
        try:
            child.start(objectCount)
        except Violation, v:
            f = BananaFailure()
            self.handleViolation(f, "start")
    def handleToken(self, token, ready_deferred=None):
        top = self.receiveStack[-1]
        if self.debugReceive: print "handleToken(%s)" % (token,)
        if ready_deferred:
            assert isinstance(ready_deferred, defer.Deferred)
        try:
            top.receiveChild(token, ready_deferred)
        except Violation, v:
            f = BananaFailure()
            self.handleViolation(f, "receiveChild")
    def handleClose(self, closeCount):
        if self.debugReceive:
            print "handleClose(%d)" % closeCount
        if self.receiveStack[-1].openCount != closeCount:
            raise BananaError("lost sync, got CLOSE(%d) but expecting %s" \
                              % (closeCount, self.receiveStack[-1].openCount))
        child = self.receiveStack[-1] # don't pop yet: describe() needs it
        try:
            obj, ready_deferred = child.receiveClose()
        except Violation, v:
            f = BananaFailure()
            self.handleViolation(f, "receiveClose", inClose=True)
            return
        if self.debugReceive: print "receiveClose returned", obj
        try:
            child.finish()
        except Violation, v:
            f = BananaFailure()
            self.handleViolation(f, "finish", inClose=True)
            return
        self.receiveStack.pop()
        self.handleToken(obj, ready_deferred)
    def handleViolation(self, f, methname, inOpen=False, inClose=False):
        """An Unslicer has decided to give up, or we have given up on it
        (because we received an ABORT token). 
        """
        where = self.describeReceive()
        f.value.setLocation(where)
        if self.debugReceive:
            print " handleViolation-%s (inOpen=%s, inClose=%s): %s" \
                  % (methname, inOpen, inClose, f)
        assert isinstance(f, BananaFailure)
        if self.logViolations:
            log.msg("Violation in %s at %s" % (methname, where))
            log.err(f)
        if inOpen:
            self.discardCount += 1
            if self.debugReceive:
                print "  ++discardCount (inOpen), now %d" % self.discardCount
        while True:
            if self.debugReceive:
                print " reportViolation to %s" % self.receiveStack[-1]
            f = self.receiveStack[-1].reportViolation(f)
            if not f:
                if self.debugReceive:
                    print "  buck stopped, error absorbed"
                break
            if self.debugReceive:
                print "  popping %s" % self.receiveStack[-1]
            if not inClose:
                self.discardCount += 1
                if self.debugReceive:
                    print "  ++discardCount (pop, not inClose), now %d" \
                          % self.discardCount
            inClose = False
            old = self.receiveStack.pop()
            try:
                old.finish() # ??
            except Violation:
                pass # they've already failed once
            if not self.receiveStack:
                why = "Oh my god, you killed the RootUnslicer! " + \
                      "You bastard!!"
                raise BananaError(why)
    def handleError(self, msg):
        log.msg("got banana ERROR from remote side: %s" % msg)
        self.transport.loseConnection(BananaError("remote error: %s" % msg))
    def describeReceive(self):
        where = []
        for i in self.receiveStack:
            try:
                piece = i.describe()
            except:
                piece = "???"
            where.append(piece)
        return ".".join(where)
    def receivedObject(self, obj):
        """Decoded objects are delivered here, unless you use a RootUnslicer
        variant which does something else in its .childFinished method.
        """
        raise NotImplementedError
    def reportViolation(self, why):
        return why
