from twisted.trial import unittest
from twisted.internet import protocol, defer, reactor
from twisted.application import internet
from twisted.pb import negotiate, pb, tokens
try:
    from twisted.pb import crypto
except ImportError:
    crypto = None
if crypto and not crypto.available:
    crypto = None
class Target(pb.Referenceable):
    def __init__(self):
        self.calls = 0
    def remote_call(self):
        self.calls += 1
def getPage(url):
    """This is a variant of the standard twisted.web.client.getPage, which is
    smart enough to shut off its connection when its done (even if it fails).
    """
    from twisted.web import client
    scheme, host, port, path = client._parse(url)
    factory = client.HTTPClientFactory(url)
    c = reactor.connectTCP(host, port, factory)
    def shutdown(res, c):
        c.disconnect()
        return res
    factory.deferred.addBoth(shutdown, c)
    return factory.deferred
class OneTimeDeferred(defer.Deferred):
    def callback(self, res):
        if self.called:
            return
        return defer.Deferred.callback(self, res)
class BaseMixin:
    def setUp(self):
        self.connections = []
        self.servers = []
        self.services = []
    def tearDown(self):
        for c in self.connections:
            if c.transport:
                c.transport.loseConnection()
        dl = []
        for s in self.servers:
            dl.append(defer.maybeDeferred(s.stopListening))
        for s in self.services:
            dl.append(defer.maybeDeferred(s.stopService))
        d = defer.DeferredList(dl)
        d.addCallback(self._checkListeners)
        return d
    def _checkListeners(self, res):
        self.failIf(pb.Listeners)
    def stall(self, res, timeout):
        d = defer.Deferred()
        reactor.callLater(timeout, d.callback, res)
        return d
    def makeServer(self, encrypted, options={}, listenerOptions={}):
        self.tub = tub = pb.PBService(encrypted=encrypted, options=options)
        tub.startService()
        self.services.append(tub)
        l = tub.listenOn("tcp:0", listenerOptions)
        tub.setLocation("localhost:%d" % l.getPortnum())
        self.target = Target()
        return tub.registerReference(self.target), l.getPortnum()
    def makeNullServer(self):
        f = protocol.Factory()
        f.protocol = protocol.Protocol # discards everything
        s = internet.TCPServer(0, f)
        s.startService()
        self.services.append(s)
        portnum = s._port.getHost().port
        return portnum
    def makeHTTPServer(self):
        try:
            from twisted.web import server, resource, static
        except ImportError:
            raise unittest.SkipTest('this test needs twisted.web')
        root = resource.Resource()
        root.putChild("", static.Data("hello\n", "text/plain"))
        s = internet.TCPServer(0, server.Site(root))
        s.startService()
        self.services.append(s)
        portnum = s._port.getHost().port
        return portnum
    def connectClient(self, portnum):
        tub = pb.PBService(encrypted=False)
        tub.startService()
        self.services.append(tub)
        d = tub.getReference("pb://localhost:%d/hello" % portnum)
        return d
    def connectHTTPClient(self, portnum):
        return getPage("http://localhost:%d/foo" % portnum)
class Basic(BaseMixin, unittest.TestCase):
    def testOptions(self):
        url, portnum = self.makeServer(False, {'opt': 12})
        self.failUnlessEqual(self.tub.options['opt'], 12)
    def testEncrypted(self):
        if not crypto:
            raise unittest.SkipTest("crypto not available")
        url, portnum = self.makeServer(True)
        client = pb.PBService(encrypted=True)
        client.startService()
        self.services.append(client)
        d = client.getReference(url)
        return d
    testEncrypted.timeout = 10
    def testUnencrypted(self):
        url, portnum = self.makeServer(False)
        client = pb.PBService(encrypted=False)
        client.startService()
        self.services.append(client)
        d = client.getReference(url)
        return d
    testUnencrypted.timeout = 10
    def testHalfEncrypted1(self):
        if not crypto:
            raise unittest.SkipTest("crypto not available")
        url, portnum = self.makeServer(True)
        client = pb.PBService(encrypted=False)
        client.startService()
        self.services.append(client)
        d = client.getReference(url)
        return d
    testHalfEncrypted1.timeout = 10
    def testHalfEncrypted2(self):
        if not crypto:
            raise unittest.SkipTest("crypto not available")
        url, portnum = self.makeServer(False)
        client = pb.PBService(encrypted=True)
        client.startService()
        self.services.append(client)
        d = client.getReference(url)
        return d
    testHalfEncrypted2.timeout = 10
class Versus(BaseMixin, unittest.TestCase):
    def testVersusHTTPServerEncrypted(self):
        if not crypto:
            raise unittest.SkipTest("crypto not available")
        portnum = self.makeHTTPServer()
        client = pb.PBService(encrypted=True)
        client.startService()
        self.services.append(client)
        url = "pb://1234@localhost:%d/target" % portnum
        d = client.getReference(url)
        d.addCallbacks(lambda res: self.fail("this is supposed to fail"),
                       lambda f: f.trap(pb.BananaError))
        d.addCallback(self.stall, 1)
        return d
    testVersusHTTPServerEncrypted.timeout = 10
    def testVersusHTTPServerUnencrypted(self):
        portnum = self.makeHTTPServer()
        client = pb.PBService(encrypted=False)
        client.startService()
        self.services.append(client)
        url = "pbu://localhost:%d/target" % portnum
        d = client.getReference(url)
        d.addCallbacks(lambda res: self.fail("this is supposed to fail"),
                       lambda f: f.trap(pb.BananaError))
        d.addCallback(self.stall, 1) # same reason as above
        return d
    testVersusHTTPServerUnencrypted.timeout = 10
    def testVersusHTTPClientUnencrypted(self):
        try:
            from twisted.web import error
        except ImportError:
            raise unittest.SkipTest('this test needs twisted.web')
        url, portnum = self.makeServer(False)
        d = self.connectHTTPClient(portnum)
        d.addCallbacks(lambda res: self.fail("this is supposed to fail"),
                       lambda f: f.trap(error.Error))
        return d
    testVersusHTTPClientUnencrypted.timeout = 10
    def testVersusHTTPClientEncrypted(self):
        if not crypto:
            raise unittest.SkipTest("crypto not available")
        try:
            from twisted.web import error
        except ImportError:
            raise unittest.SkipTest('this test needs twisted.web')
        url, portnum = self.makeServer(True)
        d = self.connectHTTPClient(portnum)
        d.addCallbacks(lambda res: self.fail("this is supposed to fail"),
                       lambda f: f.trap(error.Error))
        return d
    testVersusHTTPClientEncrypted.timeout = 10
    def testNoConnection(self):
        url, portnum = self.makeServer(False)
        d = self.tub.stopService()
        d.addCallback(self._testNoConnection_1, url)
        return d
    testNoConnection.timeout = 10
    def _testNoConnection_1(self, res, url):
        self.services.remove(self.tub)
        client = pb.PBService(encrypted=False)
        client.startService()
        self.services.append(client)
        d = client.getReference(url)
        d.addCallbacks(lambda res: self.fail("this is supposed to fail"),
                       self._testNoConnection_fail)
        return d
    def _testNoConnection_fail(self, why):
        from twisted.internet import error
        self.failUnless(why.check(error.ConnectionRefusedError))
    def testClientTimeout(self):
        portnum = self.makeNullServer()
        client = pb.PBService(encrypted=False, options={'connect_timeout': 1})
        client.startService()
        self.services.append(client)
        url = "pbu://localhost:%d/target" % portnum
        d = client.getReference(url)
        d.addCallbacks(lambda res: self.fail("hey! this is supposed to fail"),
                       lambda f: f.trap(tokens.NegotiationError))
        return d
    testClientTimeout.timeout = 10
    def testServerTimeout(self):
        d = OneTimeDeferred()
        options = {'server_timeout': 1,
                   'debug_negotiationFailed_cb': d.callback
                   }
        url, portnum = self.makeServer(False, listenerOptions=options)
        f = protocol.ClientFactory()
        f.protocol = protocol.Protocol # discards everything
        s = internet.TCPClient("localhost", portnum, f)
        s.startService()
        self.services.append(s)
        d.addCallbacks(lambda res: self.fail("hey! this is supposed to fail"),
                       lambda f: self._testServerTimeout_1)
        return d
    testServerTimeout.timeout = 10
    def _testServerTimeout_1(self, f):
        self.failUnless(f.check(tokens.NegotiationError))
        self.failUnlessEqual(f.value.args[0], "negotiation timeout")
class Parallel(BaseMixin, unittest.TestCase):
    def makeServers(self, tubopts={}, lo1={}, lo2={}):
        self.tub = tub = pb.PBService(encrypted=True, options=tubopts)
        tub.startService()
        self.services.append(tub)
        l1 = tub.listenOn("tcp:0", lo1)
        l2 = tub.listenOn("tcp:0", lo2)
        self.p1, self.p2 = l1.getPortnum(), l2.getPortnum()
        tub.setLocation("localhost:%d" % l1.getPortnum(),
                        "localhost:%d" % l2.getPortnum())
        self.target = Target()
        return tub.registerReference(self.target)
    def connect(self, url, encrypted=True):
        self.clientPhases = []
        opts = {"debug_stall_second_connection": True,
                "debug_gatherPhases": self.clientPhases}
        self.client = client = pb.PBService(encrypted=encrypted,
                                            options=opts)
        client.startService()
        self.services.append(client)
        d = client.getReference(url)
        return d
    def NOTtearDown(self):
        d = BaseMixin.tearDown(self)
        d.addCallback(self.stall, 1)
        return d
    def checkConnectedToFirstListener(self, rr, targetPhases):
        self.failUnlessEqual(rr.tracker.broker.transport.getPeer().port,
                             self.p1)
        d = self.stall(rr, 0.5)
        d.addCallback(lambda res:
                      self.failUnlessEqual(self.clientPhases, targetPhases,
                                           "negotiation was abandoned in "
                                           "the wrong phase"))
        return d
    def test1(self):
        url = self.makeServers(lo1={'debug_slow_connectionMade': True})
        d = self.tub.stopListeningOn(self.tub.getListeners()[1])
        d.addCallback(self._test1_1, url)
        return d
    def _test1_1(self, res, url):
        d = self.connect(url)
        d.addCallback(self.checkConnectedToFirstListener, [])
        return d
    test1.timeout = 10
    def test2(self):
        url = self.makeServers(lo2={'debug_slow_connectionMade': True})
        d = self.connect(url)
        d.addCallback(self.checkConnectedToFirstListener,
                      [negotiate.PLAINTEXT])
        return d
    test2.timeout = 10
    def test3(self):
        url = self.makeServers(lo2={'debug_slow_sendPlaintextServer': True})
        d = self.connect(url)
        d.addCallback(self.checkConnectedToFirstListener,
                      [negotiate.PLAINTEXT])
        return d
    test3.timeout = 10
    def test4(self):
        url = self.makeServers(lo2={'debug_slow_sendHello': True})
        d = self.connect(url)
        d.addCallback(self.checkConnectedToFirstListener,
                      [negotiate.ENCRYPTED])
        return d
    test4.timeout = 10
    def test5(self):
        url = self.makeServers(lo2={'debug_slow_sendDecision': True})
        d = self.connect(url, encrypted=False)
        d.addCallback(self.checkConnectedToFirstListener,
                      [negotiate.DECIDING])
        return d
    test5.timeout = 10
class CrossfireMixin(BaseMixin):
    tub1IsMaster = False
    def makeServers(self, t1opts={}, t2opts={}, lo1={}, lo2={},
                    tubAencrypted=True, tubBencrypted=True):
        a = pb.PBService(encrypted=tubAencrypted, options=t1opts)
        b = pb.PBService(encrypted=tubBencrypted, options=t1opts)
        if a.tubID > b.tubID:
            tub1,tub2 = a,b
        else:
            tub1,tub2 = b,a
        if not self.tub1IsMaster:
            tub1,tub2 = tub2,tub1
        self.tub1 = tub1
        self.tub2 = tub2
        self.tub1phases = []
        t1opts['debug_gatherPhases'] = self.tub1phases
        tub1.options = t1opts
        self.tub2phases = []
        t2opts['debug_gatherPhases'] = self.tub2phases
        tub2.options = t2opts
        tub1.startService()
        self.services.append(tub1)
        l1 = tub1.listenOn("tcp:0", lo1)
        tub1.setLocation("localhost:%d" % l1.getPortnum())
        self.target1 = Target()
        self.url1 = tub1.registerReference(self.target1)
        tub2.startService()
        self.services.append(tub2)
        l2 = tub2.listenOn("tcp:0", lo2)
        tub2.setLocation("localhost:%d" % l2.getPortnum())
        self.target2 = Target()
        self.url2 = tub2.registerReference(self.target2)
    def connect(self):
        d1 = self.tub2.getReference(self.url1)
        d = self.stall(None, 0.1)
        d.addCallback(self._connect, d1)
        return d, d1
    def _connect(self, res, d1):
        d2 = self.tub1.getReference(self.url2)
        return d2
    def checkConnectedViaReverse(self, rref, targetPhases):
        self.failUnlessEqual(rref.tracker.broker.transport.getPeer().port,
                             self.tub2.getListeners()[0].getPortnum())
        self.failUnlessEqual(self.tub2phases, targetPhases)
class CrossfireReverse(CrossfireMixin, unittest.TestCase):
    tub1IsMaster = False
    def test1(self):
        self.makeServers(lo1={'debug_slow_connectionMade': True})
        d = self.tub2.stopListeningOn(self.tub2.getListeners()[0])
        d.addCallback(self._test1_1)
        return d
    def _test1_1(self, res):
        d,d1 = self.connect()
        d.addCallbacks(lambda res: self.fail("hey! this is supposed to fail"),
                       self._test1_2, errbackArgs=(d1,))
        return d
    def _test1_2(self, why, d1):
        from twisted.internet import error
        self.failUnless(why.check(error.ConnectionRefusedError))
        return d1
    test1.timeout = 10
    def test2(self):
        self.makeServers(lo1={'debug_slow_connectionMade': True})
        d,d1 = self.connect()
        d.addCallback(self.checkConnectedViaReverse, [negotiate.PLAINTEXT])
        d.addCallback(lambda res: d1) # other getReference should work too
        return d
    test2.timeout = 10
    def test3(self):
        self.makeServers(lo1={'debug_slow_sendPlaintextServer': True})
        d,d1 = self.connect()
        d.addCallback(self.checkConnectedViaReverse, [negotiate.PLAINTEXT])
        d.addCallback(lambda res: d1) # other getReference should work too
        return d
    test3.timeout = 10
    def test4(self):
        self.makeServers(lo1={'debug_slow_sendHello': True})
        d,d1 = self.connect()
        d.addCallback(self.checkConnectedViaReverse, [negotiate.ENCRYPTED])
        d.addCallback(lambda res: d1) # other getReference should work too
        return d
    test4.timeout = 10
class CrossfireReverse(CrossfireReverse):
    tub1IsMaster = True
    def test5(self):
        self.makeServers(lo1={'debug_slow_sendDecision': True})
        d,d1 = self.connect()
        d.addCallback(self.checkConnectedViaReverse, [negotiate.DECIDING])
        d.addCallback(lambda res: d1) # other getReference should work too
        return d
    test5.timeout = 10
class Existing(CrossfireMixin, unittest.TestCase):
    def checkNumBrokers(self, res, expected, dummy):
        if type(expected) not in (tuple,list):
            expected = [expected]
        self.failUnless(len(self.tub1.brokers) +
                        len(self.tub1.unencryptedBrokers) in expected)
        self.failUnless(len(self.tub2.brokers) +
                        len(self.tub2.unencryptedBrokers) in expected)
    def testEncrypted(self):
        self.makeServers()
        d = self.tub1.getReference(self.url2)
        d.addCallback(self._testEncrypted_1)
        return d
    def _testEncrypted_1(self, r12):
        d = self.tub2.getReference(self.url1)
        d.addCallback(self.checkNumBrokers, 1, (r12,))
        return d
    def testUnencrypted(self):
        self.makeServers(tubAencrypted=False, tubBencrypted=False)
        d = self.tub1.getReference(self.url2)
        d.addCallback(self._testUnencrypted_1)
        return d
    def _testUnencrypted_1(self, r12):
        d = self.tub2.getReference(self.url1)
        d.addCallback(self.checkNumBrokers, 2, (r12,))
        return d
    def testHalfEncrypted1(self):
        self.makeServers(tubAencrypted=True, tubBencrypted=False)
        d = self.tub1.getReference(self.url2)
        d.addCallback(self._testHalfEncrypted1_1)
        return d
    def _testHalfEncrypted1_1(self, r12):
        d = self.tub2.getReference(self.url1)
        d.addCallback(self.checkNumBrokers, (1,2), (r12,))
        return d
    def testHalfEncrypted2(self):
        self.makeServers(tubAencrypted=True, tubBencrypted=False)
        d = self.tub2.getReference(self.url1)
        d.addCallback(self._testHalfEncrypted2_1)
        return d
    def _testHalfEncrypted2_1(self, r21):
        d = self.tub1.getReference(self.url2)
        d.addCallback(self.checkNumBrokers, 2, (r21,))
        return d
