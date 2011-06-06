from twisted.python import log
from twisted.python.failure import Failure
from twisted.internet import protocol, reactor, defer
from twisted.pb import broker, referenceable
from twisted.pb.tokens import BananaError, \
     NegotiationError, RemoteNegotiationError
try:
    from twisted.pb import crypto
except ImportError:
    crypto = None
if crypto and not crypto.available:
    crypto = None
def isSubstring(small, big):
    assert type(small) is str and type(big) is str
    return small in big
PLAINTEXT, ENCRYPTED, DECIDING, BANANA, ABANDONED = range(5)
class _SimpleCallQueue:
    def __init__(self):
        self.events = []
        self.flushObservers = []
        self.timer = None
    def do(self, c, *a, **k):
        self.events.append((c, a, k))
        if not self.timer:
            self.timer = reactor.callLater(0, self.turn)
    def turn(self):
        self.timer = None
        c, a, k = self.events.pop(0)
        try:
            c(*a, **k)
        except:
            log.err()
        if self.events and not self.timer:
            self.timer = reactor.callLater(0, self.turn)
        if not self.events:
            observers, self.flushObservers = self.flushObservers, []
            for o in observers:
                o.callback(None)
    def flush(self):
        if not self.events:
            return defer.succeed(None)
        d = defer.Deferred()
        self.flushObservers.append(d)
        return d
_theSimpleQueue = _SimpleCallQueue()
def eventually(value=None):
    """This is the eventual-send operation, used as a plan-coordination
    primitive. It will create a Deferred which fires after the current call
    stack has been completed, and after all other deferreds previously
    scheduled with eventually().
    """
    d = defer.Deferred()
    _theSimpleQueue.do(d.callback, value)
    return d
def flushEventualQueue():
    """This returns a Deferred which fires when the eventual-send queue is
    finally empty. This is useful to wait upon as the last step of a Trial
    test method.
    """
    return _theSimpleQueue.flush()
class Negotiation(protocol.Protocol):
    """This is the first protocol to speak over the wire. It is responsible
    for negotiating the connection parameters, then switching the connection
    over to the actual Banana protocol. This removes all the details of
    negotiation from Banana, and makes it easier to use a more complex scheme
    (including a STARTTLS transition) in PB.
    Negotiation consists of three phases. In the PLAINTEXT phase, the client
    side (i.e. the one which initiated the connection) sends an
    HTTP-compatible GET request for the target Tub ID. This request includes
    an Connection: Upgrade header. The GET request serves a couple of
    purposes: if a PB client is accidentally pointed at an HTTP server, it
    will trigger a sensible 404 Error instead of getting confused. A regular
    HTTP server can be used to send back a 303 Redirect, allowing Apache (or
    whatever) to be used as a redirection server.
    After sending the GET request, the client waits for the server to send a
    101 Switching Protocols command, then starts the TLS session. It may also
    receive a 303 Redirect command, in which case it drops the connection and
    tries again with the new target.
    In the PLAINTEXT phase, the server side (i.e. the one which accepted the
    connection) waits for the client's GET request, extracts the TubID from
    the first line, consults the local Listener object to locate the
    appropriate Tub (and its certificate), sends back a 101 Switching
    Protocols response, then starts the TLS session with the Tub's
    certificate. If the Listener reports that the requested Tub is listening
    elsewhere, the server sends back a 303 Redirect instead, and then drops
    the connection.
    By the end of the PLAINTEXT phase, both ends know which Tub they are
    using (self.tub has been set).
    Both sides send a Hello Block upon entering the ENCRYPTED phase, which in
    practice means just after starting the TLS session. The Hello block
    contains the negotiation offer, as a series of Key: Value lines separated
    by \\r\\n delimiters and terminated by a blank line. Upon receiving the
    other end's Hello block, each side switches to the DECIDING phase, and
    then evaluates the received Hello message.
    Each side compares TubIDs, and the side with the lexicographically higher
    value becomes the Master. (If, for some reason, one side does not claim a
    TubID, its value is treated as None, which always compares *less* than
    any actual TubID, so the non-TubID side will probably not be the Master.
    Any possible ties are resolved by having the server side be the master).
    Both sides know the other's TubID, so both sides know whether they are
    the Master or not.
    The Master has two jobs to do. The first is that it compares the
    negotiation offer against its own capabilities, and comes to a decision
    about what the connection parameters shall be. It may decide that the two
    sides are not compatible, in which case it will abandon the connection.
    The second job is to decide whether to continue to use the connection at
    all: if the Master already has a connection to the other Tub, it will
    drop this new one. This decision must be made by the Master (as opposed
    to the Server) because it is possible for both Tubs to connect to each
    other simultaneously, and this design avoids a race condition that could
    otherwise drop *both* connections.
    If the Master decides to continue with the connection, it sends the
    Decision block to the non-master side. It then swaps out the Negotiation
    protocol for a new Banana protocol instance that has been created with
    the same parameters that were used to create the Decision block.
    The non-master side is waiting in the DECIDING phase for this block. Upon
    receiving it, the non-master side evaluates the connection parameters and
    either drops the connection or swaps in a new Banana protocol instance
    with the same parameters. At this point, negotiation is complete and the
    Negotiation instances are dropped.
    @ivar negotationOffer: a dict which describes what we will offer to the
    far side. Each key/value pair will be put into a rfc822-style header and
    sent from the client to the server when the connection is established. On
    the server side, handleNegotiation() uses negotationOffer to indicate
    what we are locally capable of.
    Subclasses may influence the negotiation process by modifying this
    dictionary before connectionMade() is called.
    @ivar negotiationResults: a dict which describes what the two ends have
    agreed upon. This is computed by the server, stored locally, and sent
    down to the client. The client receives it and stores it without
    modification (server chooses).
    In general, the negotiationResults are the same on both sides of the same
    connection. However there may be certain parameters which are sent as
    part of the negotiation block (the PB TubID, for example) which will not.
    """
    myTubID = None
    tub = None
    theirTubID = None
    phase = PLAINTEXT
    encrypted = False
    doNegotiation = True
    debugNegotiation = False
    forceNegotiation = None
    SERVER_TIMEOUT = 60 # you have 60 seconds to complete negotiation, or else
    negotiationTimer = None
    def __init__(self):
        self.negotiationOffer = {"banana-negotiation-version": "1"}
        if self.forceNegotiation is not None:
            self.negotiationOffer['negotiation-forced'] = "True"
        self.buffer = ""
        self.options = {}
        self.debugTimers = {}
    def initClient(self, connector, targetHost):
        self.isClient = True
        self.tub = connector.tub
        self.myTubID = self.tub.tubID
        self.connector = connector
        self.target = connector.target
        self.targetHost = targetHost
        self.wantEncryption = bool(self.target.encrypted
                                   or self.tub.myCertificate)
        self.options = self.tub.options.copy()
    def initServer(self, listener):
        self.isClient = False
        self.listener = listener
        self.options = self.listener.options.copy()
    def parseLines(self, header):
        lines = header.split("\r\n")
        block = {}
        for line in lines:
            colon = line.index(":")
            key = line[:colon].lower()
            value = line[colon+1:].lstrip()
            block[key] = value
        return block
    def sendBlock(self, block):
        keys = block.keys()
        keys.sort()
        for k in keys:
            self.transport.write("%s: %s\r\n" % (k.lower(), block[k]))
        self.transport.write("\r\n") # end block
    def debug_doTimer(self, name, timeout, call, *args):
        if (self.options.has_key("debug_slow_%s" % name) and
            not self.debugTimers.has_key(name)):
            log.msg("debug_doTimer(%s)" % name)
            t = reactor.callLater(timeout, self.debug_fireTimer, name)
            self.debugTimers[name] = (t, [(call, args)])
            cb = self.options["debug_slow_%s" % name]
            if cb is not None and cb is not True:
                cb()
            return True
        return False
    def debug_addTimerCallback(self, name, call, *args):
        if self.debugTimers.get(name):
            self.debugTimers[name][1].append((call, args))
            return True
        return False
    def debug_forceTimer(self, name):
        if self.debugTimers.get(name):
            self.debugTimers[name][0].cancel()
            self.debug_fireTimer(name)
    def debug_forceAllTimers(self):
        for name in self.debugTimers:
            if self.debugTimers.get(name):
                self.debugTimers[name][0].cancel()
                self.debug_fireTimer(name)
    def debug_cancelAllTimers(self):
        for name in self.debugTimers:
            if self.debugTimers.get(name):
                self.debugTimers[name][0].cancel()
                self.debugTimers[name] = None
    def debug_fireTimer(self, name):
        calls = self.debugTimers[name][1]
        self.debugTimers[name] = None
        for call,args in calls:
            call(*args)
    def connectionMade(self):
        if self.doNegotiation:
            if self.isClient:
                self.connectionMadeClient()
            else:
                self.connectionMadeServer()
        else:
            self.switchToBanana({})
    def connectionMadeClient(self):
        assert self.phase == PLAINTEXT
        self.sendPlaintextClient()
    def sendPlaintextClient(self):
        req = []
        if self.target.encrypted:
            if self.debugNegotiation:
                log.msg("sendPlaintextClient: GET for tubID %s"
                        % self.target.tubID)
            req.append("GET /id/%s HTTP/1.1" % self.target.tubID)
        else:
            if self.debugNegotiation:
                log.msg("sendPlaintextClient: GET for no tubID")
            req.append("GET /id/ HTTP/1.1")
        req.append("Host: %s" % self.targetHost)
        if self.debugNegotiation:
            log.msg("sendPlaintextClient: wantEncryption=%s" %
                    self.wantEncryption)
        if self.wantEncryption:
            req.append("Upgrade: TLS/1.0")
        else:
            req.append("Upgrade: PB/1.0")
        req.append("Connection: Upgrade")
        self.transport.write("\r\n".join(req))
        self.transport.write("\r\n\r\n")
    def connectionMadeServer(self):
        if self.debug_doTimer("connectionMade", 1, self.connectionMade):
            return
        timeout = self.options.get('server_timeout', self.SERVER_TIMEOUT)
        if timeout:
            self.negotiationTimer = reactor.callLater(timeout,
                                                      self.negotiationTimedOut)
    def sendError(self, why):
        pass # TODO
    def negotiationTimedOut(self):
        del self.negotiationTimer
        why = Failure(NegotiationError("negotiation timeout"))
        self.sendError(why)
        self.negotiationFailed(why)
        self.transport.loseConnection(why)
    def stopNegotiationTimer(self):
        if self.negotiationTimer:
            self.negotiationTimer.cancel()
            del self.negotiationTimer
    def dataReceived(self, chunk):
        if self.debugNegotiation:
            log.msg("dataReceived(isClient=%s,phase=%s,options=%s): '%s'"
                    % (self.isClient, self.phase, self.options, chunk))
        if self.phase == ABANDONED:
            return
        self.buffer += chunk
        if self.debug_addTimerCallback("connectionMade",
                                       self.dataReceived, ''):
            return
        try:
            if len(self.buffer) > 4096:
                raise BananaError("Header too long")
            eoh = self.buffer.find('\r\n\r\n')
            if eoh == -1:
                return
            header, self.buffer = self.buffer[:eoh], self.buffer[eoh+4:]
            if self.phase == PLAINTEXT:
                if self.isClient:
                    self.handlePLAINTEXTClient(header)
                else:
                    self.handlePLAINTEXTServer(header)
            elif self.phase == ENCRYPTED:
                self.handleENCRYPTED(header)
            elif self.phase == DECIDING:
                self.handleDECIDING(header)
            else:
                assert 0, "should not get here"
            self.dataReceived("")
        except Exception, e:
            why = Failure()
            if self.debugNegotiation:
                log.msg("negotation had exception: %s" % why)
            if isinstance(e, RemoteNegotiationError):
                pass # they've already hung up
            else:
                if isinstance(e, NegotiationError):
                    errmsg = str(e)
                else:
                    errmsg = "internal server error, see logs"
                if self.phase == PLAINTEXT:
                    resp = "HTTP/1.1 500 Internal Server Error\r\n\r\n"
                    self.transport.write(resp)
                elif self.phase in (ENCRYPTED, DECIDING):
                    block = {'error': errmsg}
                    self.sendBlock(block)
                elif self.phase == BANANA:
                    pass # TODO
            self.negotiationFailed(why)
            self.transport.loseConnection(why)
            return
    def connectionLost(self, reason):
        self.debug_forceTimer("connectionMade")
        self.debug_cancelAllTimers()
        for k,t in self.debugTimers.items():
            if t:
                t[0].cancel()
                self.debugTimers[k] = None
        if self.isClient:
            l = self.tub.options.get("debug_gatherPhases")
            if l is not None:
                l.append(self.phase)
        self.negotiationFailed(reason)
    def handlePLAINTEXTServer(self, header):
        lines = header.split("\r\n")
        if not lines[0].startswith("GET "):
            raise BananaError("not right")
        command, url, version = lines[0].split()
        if not url.startswith("/id/"):
            raise BananaError("not right")
        targetTubID = url[4:]
        if self.debugNegotiation:
            log.msg("handlePLAINTEXTServer: targetTubID='%s'" % targetTubID)
        if targetTubID == "":
            targetTubID = None
        if isSubstring("Upgrade: TLS/1.0\r\n", header):
            wantEncrypted = True
        else:
            wantEncrypted = False
        if self.debugNegotiation:
            log.msg("handlePLAINTEXTServer: wantEncrypted=%s" % wantEncrypted)
        if wantEncrypted and not crypto:
            log.msg("Negotiate.handlePLAINTEXTServer: client wants "
                    "encryption for TubID=%s but we have no crypto, "
                    "hanging up on them" % targetTubID)
            raise NegotiationError("crypto not available")
        if wantEncrypted and targetTubID is None:
            wantEncrypted = False
        if targetTubID is not None and not wantEncrypted:
            raise NegotiationError("secure Tubs require encryption")
        tub, redirect = self.listener.lookupTubID(targetTubID)
        if tub:
            self.tub = tub # our tub
            self.options.update(self.tub.options)
            self.myTubID = tub.tubID
            self.sendPlaintextServerAndStartENCRYPTED(wantEncrypted)
        elif redirect:
            self.sendRedirect(redirect)
        else:
            raise NegotiationError("unknown TubID %s" % targetTubID)
    def sendPlaintextServerAndStartENCRYPTED(self, encrypted):
        if self.debug_doTimer("sendPlaintextServer", 1,
                              self.sendPlaintextServerAndStartENCRYPTED,
                              encrypted):
            return
        if encrypted:
            resp = "\r\n".join(["HTTP/1.1 101 Switching Protocols",
                                "Upgrade: TLS/1.0, PB/1.0",
                                "Connection: Upgrade",
                                ])
        else:
            resp = "\r\n".join(["HTTP/1.1 101 Switching Protocols",
                                "Upgrade: PB/1.0",
                                "Connection: Upgrade",
                                ])
        self.transport.write(resp)
        self.transport.write("\r\n\r\n")
        self.startENCRYPTED(encrypted)
    def sendRedirect(self, redirect):
        raise NotImplementedError # TODO
    def handlePLAINTEXTClient(self, header):
        if self.debugNegotiation:
            log.msg("handlePLAINTEXTClient: header='%s'" % header)
        lines = header.split("\r\n")
        tokens = lines[0].split()
        if tokens[1] != "101":
            raise BananaError("not right, got '%s', "
                              "expected 101 Switching Protocols"
                              % tokens[1])
        isEncrypted = isSubstring("Upgrade: TLS/1.0", header)
        if not isEncrypted:
            self.myTubID = None
        self.startENCRYPTED(isEncrypted)
    def startENCRYPTED(self, encrypted):
        if self.debugNegotiation:
            log.msg("startENCRYPTED(isClient=%s, encrypted=%s)" %
                    (self.isClient, encrypted))
        if encrypted:
            self.startTLS(self.tub.myCertificate)
        self.encrypted = encrypted
        self.phase = ENCRYPTED
        self.sendHello()
    def sendHello(self):
        """This is called on both sides as soon as the encrypted connection
        is established. This causes a negotiation block to be sent to the
        other side as an offer."""
        if self.debug_doTimer("sendHello", 1, self.sendHello):
            return
        hello = self.negotiationOffer.copy()
        if self.myTubID:
            hello['my-tub-id'] = self.myTubID
        if self.debugNegotiation:
            log.msg("Negotiate.sendHello (isClient=%s): %s" %
                    (self.isClient, hello))
        self.sendBlock(hello)
    def handleENCRYPTED(self, header):
        if self.debug_addTimerCallback("sendHello",
                                       self.handleENCRYPTED, header):
            return
        self.theirCertificate = None
        if self.encrypted:
            them = crypto.peerFromTransport(self.transport)
            if them and them.original:
                self.theirCertificate = them
        hello = self.parseLines(header)
        if hello.has_key("error"):
            raise RemoteNegotiationError(hello["error"])
        self.evaluateHello(hello)
    def evaluateHello(self, offer):
        """Evaluate the HELLO message sent by the other side. We compare
        TubIDs, and the higher value becomes the 'master' and makes the
        negotiation decisions.
        This method returns a tuple of DECISION,PARAMS. There are a few
        different possibilities:
         We are the master, we make a negotiation decision: DECISION is the
         block of data to send back to the non-master side, PARAMS are the
         connection parameters we will use ourselves.
         We are the master, we can't accomodate their request: raise
         NegotiationError
         We are not the master: DECISION is None
        """
        if self.debugNegotiation:
            log.msg("evaluateHello(isClient=%s): offer=%s" % (self.isClient,
                                                              offer,))
        version = offer.get('banana-negotiation-version')
        if version != '1':
            raise NegotiationError("Unrecognized version number, "
                                   "'%s' not '1', in %s"
                                   % (version, offer))
        forced = False
        f = offer.get('negotiation-forced', None)
        if f and f.lower() == "true":
            forced = True
        assert not forced # TODO: implement
        myTubID = self.myTubID
        theirTubID = offer.get("my-tub-id")
        if self.theirCertificate is None:
            if theirTubID is not None:
                raise BananaError("you must use a certificate to claim a "
                                  "TubID")
        else:
            digest = crypto.digest32(self.theirCertificate.digest("sha1"))
            if digest != theirTubID:
                raise BananaError("TubID mismatch")
        if theirTubID:
            theirTubRef = referenceable.TubRef(theirTubID)
        else:
            theirTubRef = None # unencrypted
        self.theirTubRef = theirTubRef # for use by non-master side, later
        if self.isClient and self.target.encrypted:
            if theirTubRef != self.target:
                raise BananaError("connected to the wrong Tub")
        if myTubID is None and theirTubID is None:
            iAmTheMaster = not self.isClient
        elif myTubID is None:
            iAmTheMaster = False
        elif theirTubID is None:
            iAmTheMaster = True
        else:
            iAmTheMaster = myTubID > theirTubID
        if self.debugNegotiation:
            log.msg("iAmTheMaster: %s" % iAmTheMaster)
        decision, params = None, None
        if iAmTheMaster:
            decision = {}
            if theirTubRef and theirTubRef in self.tub.brokers:
                if self.debugNegotiation:
                    log.msg(" abandoning the connection: we already have one")
                raise NegotiationError("Duplicate connection")
            decision = {}
            decision['banana-decision-version'] = "1"
            ignoredKeys = ["my-tub-id"]
            us = dict([(k, self.negotiationOffer[k])
                       for k in self.negotiationOffer.keys()
                       if k not in ignoredKeys])
            them = dict([(k, offer[k])
                         for k in offer.keys()
                         if k not in ignoredKeys])
            if them != us:
                raise NegotiationError("our negotiation offers are different")
            params = {}
        else:
            pass
        if iAmTheMaster:
            if self.debugNegotiation:
                log.msg("Negotiation.sendDecision: %s" % decision)
            self.sendDecision(decision, params)
        else:
            self.phase = DECIDING
    def sendDecision(self, decision, params):
        if self.debug_doTimer("sendDecision", 1,
                              self.sendDecision, decision, params):
            return
        if self.debug_addTimerCallback("sendHello",
                                       self.sendDecision, decision, params):
            return
        self.sendBlock(decision)
        self.switchToBanana(params)
    def handleDECIDING(self, header):
        if self.debugNegotiation:
            log.msg("handleDECIDING(isClient=%s): %s" % (self.isClient,
                                                         header))
        if self.debug_doTimer("handleDECIDING", 1,
                              self.handleDECIDING, header):
            return
        decision = self.parseLines(header)
        params = self.acceptDecision(decision)
        self.switchToBanana(params)
    def acceptDecision(self, decision):
        """This is called on the client end when it receives the results of
        the negotiation from the server. The client must accept this decision
        (and return the connection parameters dict), or raise
        NegotiationError to hang up.negotiationResults."""
        if self.debugNegotiation:
            log.msg("Banana.acceptDecision: got %s" % decision)
        version = decision.get('banana-decision-version')
        if version != '1':
            raise NegotiationError("Unrecognized version number, "
                                   "'%s' not '1', in %s"
                                   % (version, decision))
        if decision.has_key("error"):
            error = decision["error"]
            raise RemoteNegotiationError("Banana negotiation failed: %s"
                                         % error)
        params = {}
        return params
    def startTLS(self, cert):
        if self.debugNegotiation:
            log.msg("startTLS, client=%s" % self.isClient)
        kwargs = {}
        if cert:
            kwargs['privateKey'] = cert.privateKey.original
            kwargs['certificate'] = cert.original
        opts = crypto.MyOptions(**kwargs)
        self.transport.startTLS(opts)
    def switchToBanana(self, params):
        if self.debugNegotiation:
            log.msg("Negotiate.switchToBanana(isClient=%s)" % self.isClient)
        self.stopNegotiationTimer()
        b = broker.Broker(params)
        b.factory = self.factory # not used for PB code
        b.setTub(self.tub)
        b.unsafeTracebacks = self.tub.unsafeTracebacks
        if self.tub and self.tub.debugBanana:
            b.debugSend = True
            b.debugReceive = True
        self.transport.protocol = b
        b.makeConnection(self.transport)
        b.dataReceived(self.buffer)
        if self.isClient:
            self.connector.negotiationComplete(self.factory)
        if self.isClient:
            theirTubRef = self.target
        else:
            theirTubRef = self.theirTubRef
        self.tub.brokerAttached(theirTubRef, b, self.isClient)
    def negotiationFailed(self, reason):
        if self.debugNegotiation:
            log.msg("Negotiation.negotiationFailed: %s" % reason)
        self.stopNegotiationTimer()
        if self.phase != ABANDONED and self.isClient:
            d = eventually()
            d.addCallback(lambda res:
                          self.connector.negotiationFailed(self.factory,
                                                           reason))
        self.phase = ABANDONED
        cb = self.options.get("debug_negotiationFailed_cb")
        if cb:
            eventually().addCallback(lambda res: cb(reason))
class TubConnectorClientFactory(protocol.ClientFactory):
    def __init__(self, tc, host):
        self.tc = tc # the TubConnector
        self.host = host
    def startedConnecting(self, connector):
        self.connector = connector
    def disconnect(self):
        self.connector.disconnect()
    def buildProtocol(self, addr):
        proto = Negotiation()
        proto.initClient(self.tc, self.host)
        proto.factory = self
        return proto
    def clientConnectionFailed(self, connector, reason):
        self.tc.clientConnectionFailed(self, reason)
class TubConnector:
    """I am used to make an outbound connection. I am given a target TubID
    and a list of locationHints, and I try all of them until I establish a
    Broker connected to the target. I will consider redirections returned
    along the way. The first hint that yields a connected Broker will stop
    the search. If targetTubID is None, we are going to make an unencrypted
    connection.
    This is a single-use object. The connection attempt begins as soon as my
    connect() method is called.
    @param locationHints: the list of 'host:port' hints where the remote tub
                          can be found.
    """
    failureReason = None
    CONNECTION_TIMEOUT = 60
    timer = None
    def __init__(self, parent, tubref):
        self.tub = parent
        self.target = tubref
        self.remainingLocations = self.target.getLocations()
        self.attemptedLocations = []
        self.pendingConnections = {}
    def connect(self):
        """Begin the connection process. This should only be called once.
        This will either result in the successful Negotiation object invoking
        the parent Tub's brokerAttached() method, our us calling the Tub's
        connectionFailed() method."""
        timeout = self.tub.options.get('connect_timeout',
                                       self.CONNECTION_TIMEOUT)
        self.timer = reactor.callLater(timeout, self.connectionTimedOut)
        self.active = True
        self.connectToAll()
    def stopConnectionTimer(self):
        if self.timer:
            self.timer.cancel()
            del self.timer
    def shutdown(self):
        self.active = False
        self.stopConnectionTimer()
        for c in self.pendingConnections.values():
            c.disconnect()
    def connectToAll(self):
        while self.remainingLocations:
            location = self.remainingLocations.pop()
            if location in self.attemptedLocations:
                continue
            self.attemptedLocations.append(location)
            host, port = location.split(":")
            port = int(port)
            f = TubConnectorClientFactory(self, host)
            c = reactor.connectTCP(host, port, f)
            self.pendingConnections[f] = c
            if self.tub.options.get("debug_stall_second_connection"):
                reactor.callLater(0.1, self.connectToAll)
                return
        self.checkForFailure()
    def connectionTimedOut(self):
        self.timer = None
        why = "no connection established within client timeout"
        self.failureReason = Failure(NegotiationError(why))
        self.shutdown()
        self.failed()
    def clientConnectionFailed(self, factory, reason):
        if not self.failureReason:
            self.failureReason = reason
        del self.pendingConnections[factory]
        self.checkForFailure()
    def redirectReceived(self, newLocation):
        self.remainingLocations.append(newLocation)
        self.connectToAll()
    def negotiationFailed(self, factory, reason):
        assert isinstance(reason, Failure), \
               "Hey, %s isn't a Failure" % (reason,)
        if (not self.failureReason or
            isinstance(reason, NegotiationError)):
            self.failureReason = reason
        del self.pendingConnections[factory]
        self.checkForFailure()
    def negotiationComplete(self, factory):
        self.active = False
        if self.timer:
            self.timer.cancel()
            self.timer = None
        del self.pendingConnections[factory] # this one succeeded
        for f in self.pendingConnections.keys(): # abandon the others
            f.disconnect()
    def checkForFailure(self):
        if not self.active:
            return
        if self.remainingLocations:
            return
        if self.pendingConnections:
            return
        if (self.failureReason.check(RemoteNegotiationError) and
            isSubstring(self.failureReason.value.args[0],
                        "Duplicate connection")):
            log.msg("TubConnector.checkForFailure: connection attempt "
                    "failed because the other end decided ours was a "
                    "duplicate connection, so we won't signal the "
                    "failure here")
            return
        self.failed()
    def failed(self):
        self.stopConnectionTimer()
        self.active = False
        self.tub.connectionFailed(self.target, self.failureReason)
