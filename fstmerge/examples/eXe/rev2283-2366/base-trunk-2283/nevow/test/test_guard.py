import gc
from nevow import rend
from nevow import inevow
from nevow import guard
from nevow import context
from nevow import appserver
from twisted.cred.checkers import *
from twisted.cred.portal import *
from twisted.cred.credentials import *
from twisted.internet import address
from nevow.testutil import TestCase
class FakeHTTPChannel:
    def __init__(self):
        self.transport = self
        self.factory = self
        self.received_cookies = {}
    def log(self, req):
        pass
    def requestDone(self, req):
        self.req = req
    def getPeer(self):
        return address.IPv4Address("TCP", "fake", 12345)
    def getHost(self):
        return address.IPv4Address("TCP", "fake", 80)
    def write(self, data):
        pass
    def writeSequence(self, datas):
        for data in datas:
            self.write(data)
    def makeFakeRequest(self, path, username='',password='',
                        requestClass=None):
        if requestClass is None:
            requestClass = FakeHTTPRequest
        req = requestClass(self, queued=0)
        req.user = username
        req.password = password
        req.received_cookies.update(self.received_cookies)
        req.requestReceived("GET", path, "1.0")
        return req
class FakeHTTPRequest(appserver.NevowRequest):
    def __init__(self, *args, **kw):
        appserver.NevowRequest.__init__(self, *args, **kw)
        self._pchn = self.channel
        self._cookieCache = {}
        from cStringIO import StringIO
        self.content = StringIO()
        self.received_headers['host'] = 'fake.com'
        self.written = StringIO()
    def followRedirect(self):
        L = self.headers['location']
        if L.startswith('http://'):
            L = L[len("http://"):]
            urlist = L.split('/')
            urlist[0] = ''
            nexturi = '/'.join(urlist)
        else:
            nexturi = L
        return self._pchn.makeFakeRequest(nexturi,
                                          requestClass=self.__class__)
    def followAllRedirects(self):
        R = self
        MAX_REDIRECTS = 5
        I = 1
        while R.headers.has_key('location'):
            assert I < MAX_REDIRECTS, "Too many redirects (to %s)" % R.headers['location']
            R = R.followRedirect()
            I += 1
        return R
    def write(self, data):
        self.written.write(data)
        appserver.NevowRequest.write(self, data)
    def addCookie(self, k, v, *args,**kw):
        appserver.NevowRequest.addCookie(self,k,v,*args,**kw)
        assert not self._cookieCache.has_key(k), "Should not be setting duplicate cookies!"
        self._cookieCache[k] = (v, args, kw)
        self.channel.received_cookies[k] = v
    def processingFailed(self, fail):
        raise fail
class FakeHTTPRequest_noCookies(FakeHTTPRequest):
    def addCookie(self, k, v, *args,**kw):
        pass
class FakeHTTPRequest_forceSSL(FakeHTTPRequest):
    _forceSSL = True
class FakeSite(appserver.NevowSite):
    pass
class GuardTestSuper(TestCase):
    sessions = {}
    def tearDown(self):
        for sz in self.sessions.values():
            sz.expire()
    def createPortal(self, realmFactory=None):
        if realmFactory is None:
            realmFactory = SillyRealm
        r = realmFactory()
        p = Portal(r)
        p.registerChecker(AllowAnonymousAccess(), IAnonymous)
        return p
    def createSessionWrapper(self, portal):
        swrap = guard.SessionWrapper(portal)
        self.sessions = swrap.sessions
        return swrap
    def createChannel(self, resource):
        s = FakeSite(resource)
        c = FakeHTTPChannel()
        c.site = s
        return c
def getGuard(channel):
    resource = channel.site.resource
    while isinstance(resource, ParentPage):
        assert len(resource.children) == 1
        resource = resource.children.values()[0]
    return resource
class GetLoggedInAvatar(rend.Page):
    def child_(self, ctx):
        return self
    def renderHTTP(self, ctx):
        session = inevow.ISession(ctx)
        assert isinstance(session, guard.GuardSession)
        r = session.getLoggedInRoot()
        assert r is self
        return r.__class__.__name__
class GetLoggedInAnonymous(rend.Page):
    def child_(self, ctx): return self
    def renderHTTP(self, ctx):
        raise RuntimeError, "We weren't supposed to get here."
class GetLoggedInRealm:
    __implements__ = IRealm
    def requestAvatar(self, avatarId, mind, *interfaces):
        if avatarId == ANONYMOUS:
            return inevow.IResource, GetLoggedInAnonymous(), lambda: None
        else:
            return inevow.IResource, GetLoggedInAvatar(), lambda: None
class GuardTestFuncs:
    def createGuard(self, portal):
        if not self.guardPath:
            root = self.createSessionWrapper(portal)
        else:
            root = ParentPage('root')
            cur = root
            for segment in self.guardPath[:-1]:
                new = ParentPage(segment)
                cur.putChild(segment, new)
                cur = new
            cur.putChild(self.guardPath[-1],
                         self.createSessionWrapper(portal))
        chan = self.createChannel(root)
        return chan
    def getGuardPath(self):
        """
        Return a path to the guard. An empty string if guard is a the root,
        otherwise guaranteed to start with a slash and end with a non-slash.
        """
        if not self.guardPath:
            return ''
        else:
            return '/' + '/'.join(self.guardPath)
    def testHttpAuthInit(self):
        p = self.createPortal()
        chan = self.createGuard(p)
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        for x in range(3):
            req = chan.makeFakeRequest('%s/' % self.getGuardPath(), "test", "test")
            self.assertEquals(req.written.getvalue(), "Yes")
        self.assertEquals(len(self.sessions),1)
    def testSessionInit(self):
        p = self.createPortal()
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/xxx/yyy/' % self.getGuardPath())
        self.assertEquals( len(req._cookieCache.values()), 1, "Bad number of cookies in response.")
        self.failUnless(req.headers.has_key('location'))
        cookie = req._cookieCache.values()[0][0]
        self.assertEquals(req.headers['location'],
            'http://fake.com%s/%s/xxx/yyy/' % (self.getGuardPath(), guard.SESSION_KEY+cookie, ))
        req = req.followRedirect()
        self.assertEquals(req.headers['location'].split('?')[0],
            'http://fake.com%s/xxx/yyy/' % self.getGuardPath())
        req = req.followRedirect()
        self.failIf(req.headers.has_key('location'))
        self.assertEquals(req.written.getvalue(), "No")
    def testSessionInit_noCookies(self):
        p = self.createPortal()
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/xxx/yyy/' % self.getGuardPath(), requestClass=FakeHTTPRequest_noCookies)
        self.failUnless(req.headers.has_key('location'))
        location = req.headers['location']
        prefix = 'http://fake.com%s/%s' % (self.getGuardPath(), guard.SESSION_KEY)
        suffix = '/xxx/yyy/'
        self.failUnless(location.startswith(prefix))
        self.failUnless(location.endswith(suffix))
        for c in location[len(prefix):-len(suffix)]:
            self.failUnless(c in '0123456789abcdef')
        req = req.followRedirect()
        self.failIf(req.headers.has_key('location'))
        self.assertEquals(req.written.getvalue(), "No")
    def testUsernamePassword(self):
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "No")
        req = chan.makeFakeRequest('%s/__login__/?username=test&password=test' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Yes")
        chan.makeFakeRequest("%s/__logout__" % self.getGuardPath()).followRedirect()
        k = chan.makeFakeRequest("%s/" % self.getGuardPath())
        self.assertEquals(k.written.getvalue(), "No")
    def testLoginWithNoSession(self):
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/__login__/?username=test&password=test' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Yes")
    def testFormWithNoSession(self):
        p = self.createPortal()
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/?aFormArgument=1' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), 'We got the form.')
    def testNoSlash(self):
        """URL-based sessions do not fail even if there is no slash after the session key."""
        p = self.createPortal()
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/' % self.getGuardPath(), requestClass=FakeHTTPRequest_noCookies).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "No")
        self.failUnless(req.path.startswith('%s/%s' % (self.getGuardPath(), guard.SESSION_KEY)))
        self.failUnless(req.path.endswith('/'))
        req = chan.makeFakeRequest(req.path[:-1], requestClass=FakeHTTPRequest_noCookies).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "No")
    def testTrailingSlashMatters_noCookies(self):
        class TrailingSlashPage(rend.Page):
            def locateChild(self, request, segments):
                return self.__class__('%s/%s' % (self.original, segments[0])), segments[1:]
        class TrailingSlashAvatar(TrailingSlashPage):
            def renderHTTP(self, request):
                return 'Authenticated %s' % self.original
        class TrailingSlashAnonymous(TrailingSlashPage):
            def renderHTTP(self, ctx):
                return 'Anonymous %s' % self.original
        class TrailingSlashRealm:
            __implements__ = IRealm
            def __init__(self, path):
                self.path = path
            def requestAvatar(self, avatarId, mind, *interfaces):
                if avatarId == ANONYMOUS:
                    return inevow.IResource, TrailingSlashAnonymous(self.path), lambda: None
                else:
                    return inevow.IResource, TrailingSlashAvatar(self.path), lambda: None
        p = self.createPortal(realmFactory=lambda : TrailingSlashRealm(self.getGuardPath()))
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/' % self.getGuardPath(), requestClass=FakeHTTPRequest_noCookies).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Anonymous %s/" % self.getGuardPath())
        self.failUnless(req.path.startswith('%s/%s' % (self.getGuardPath(), guard.SESSION_KEY)))
        self.failUnless(req.path.endswith('/'))
        req = chan.makeFakeRequest(req.path[:-1], requestClass=FakeHTTPRequest_noCookies).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Anonymous %s" % self.getGuardPath())
    def testTrailingSlashMatters_withCookies(self):
        if not self.guardPath:
            return
        class TrailingSlashPage(rend.Page):
            def locateChild(self, request, segments):
                return self.__class__('%s/%s' % (self.original, segments[0])), segments[1:]
        class TrailingSlashAvatar(TrailingSlashPage):
            def renderHTTP(self, request):
                return 'Authenticated %s' % self.original
        class TrailingSlashAnonymous(TrailingSlashPage):
            def renderHTTP(self, ctx):
                return 'Anonymous %s' % self.original
        class TrailingSlashRealm:
            __implements__ = IRealm
            def __init__(self, path):
                self.path = path
            def requestAvatar(self, avatarId, mind, *interfaces):
                if avatarId == ANONYMOUS:
                    return inevow.IResource, TrailingSlashAnonymous(self.path), lambda: None
                else:
                    return inevow.IResource, TrailingSlashAvatar(self.path), lambda: None
        p = self.createPortal(realmFactory=lambda : TrailingSlashRealm(self.getGuardPath()))
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Anonymous %s/" % self.getGuardPath())
        req = chan.makeFakeRequest('%s' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Anonymous %s" % self.getGuardPath())
    def testPlainTextCookie(self):
        """Cookies from non-SSL sites have no secure attribute."""
        p = self.createPortal()
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/xxx/yyy/' % self.getGuardPath())
        self.assertEquals( len(req._cookieCache.values()), 1, "Bad number of cookies in response.")
        cookie, a, kw = req._cookieCache.values()[0]
        secure = kw.get('secure', None)
        self.failIf(secure)
    def testPlainTextCookie_evenWithSecureCookies(self):
        """Cookies from non-SSL sites have no secure attribute, even if secureCookie is true."""
        p = self.createPortal()
        chan = self.createGuard(p)
        gu = getGuard(chan)
        gu.secureCookies = False
        req = chan.makeFakeRequest('%s/xxx/yyy/' % self.getGuardPath())
        self.assertEquals( len(req._cookieCache.values()), 1, "Bad number of cookies in response.")
        cookie, a, kw = req._cookieCache.values()[0]
        secure = kw.get('secure', None)
        self.failIf(secure)
    def testSecureCookie_secureCookies(self):
        """Cookies from SSL sites have secure=True."""
        p = self.createPortal()
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/xxx/yyy/' % self.getGuardPath(),
                                   requestClass=FakeHTTPRequest_forceSSL)
        self.assertEquals( len(req._cookieCache.values()), 1, "Bad number of cookies in response.")
        cookie, a, kw = req._cookieCache.values()[0]
        secure = kw.get('secure', None)
        self.failUnless(secure)
    def testSecureCookie_noSecureCookies(self):
        """Cookies from SSL sites do not have secure=True if secureCookies is false."""
        p = self.createPortal()
        chan = self.createGuard(p)
        gu = getGuard(chan)
        gu.secureCookies = False
        req = chan.makeFakeRequest('%s/xxx/yyy/' % self.getGuardPath(),
                                   requestClass=FakeHTTPRequest_forceSSL)
        self.assertEquals( len(req._cookieCache.values()), 1, "Bad number of cookies in response.")
        cookie, a, kw = req._cookieCache.values()[0]
        secure = kw.get('secure', None)
        self.failIf(secure)
    def testCookiePath(self):
        """Cookies get the correct path setting sites have no secure attribute."""
        p = self.createPortal()
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/xxx/yyy/' % self.getGuardPath())
        self.assertEquals( len(req._cookieCache.values()), 1, "Bad number of cookies in response.")
        cookie, a, kw = req._cookieCache.values()[0]
        path = kw.get('path', None)
        wanted = self.getGuardPath()
        if wanted == '':
            wanted = '/'
        self.failUnlessEqual(path, wanted)
    def testLoginExtraPath(self):
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/__login__/sub/path?username=test&password=test' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Yes")
        self.assertEquals(req.path, '%s/sub/path' % self.getGuardPath())
    def testLoginExtraPath_withSlash(self):
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/__login__/sub/path/?username=test&password=test' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Yes")
        self.assertEquals(req.path, '%s/sub/path/' % self.getGuardPath())
    def testLogoutExtraPath(self):
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/__login__?username=test&password=test' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Yes")
        req2 = chan.makeFakeRequest("%s/__logout__/sub/path" % self.getGuardPath()).followRedirect()
        self.assertEquals(req2.written.getvalue(), "No")
        self.assertEquals(req2.path, '%s/sub/path' % self.getGuardPath())
    def testLogoutExtraPath_withSlash(self):
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/__login__?username=test&password=test' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "Yes")
        req2 = chan.makeFakeRequest("%s/__logout__/sub/path/" % self.getGuardPath()).followRedirect()
        self.assertEquals(req2.written.getvalue(), "No")
        self.assertEquals(req2.path, '%s/sub/path/' % self.getGuardPath())
    def testGetLoggedInRoot_getLogin(self):
        p = self.createPortal(realmFactory=GetLoggedInRealm)
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/__login__?username=test&password=test' % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "GetLoggedInAvatar")
    def testGetLoggedInRoot_httpAuthLogin(self):
        p = self.createPortal(realmFactory=GetLoggedInRealm)
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'), IUsernamePassword)
        chan = self.createGuard(p)
        for x in range(4):
            req = chan.makeFakeRequest('%s/' % self.getGuardPath(), "test", "test")
            self.assertEquals(req.written.getvalue(), "GetLoggedInAvatar")
        self.assertEquals(len(self.sessions),1)
    def testErrorPage_httpAuth(self):
        """Failed HTTP Auth results in a 403 error."""
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'),
                          IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s' % self.getGuardPath(),
                                   "test", "invalid-password")
        self.assertEquals(req.headers.get('location', None), None)
        self.assertEquals(req.code, 403)
        self.assertEquals(req.written.getvalue(),
                          '<html><head><title>Forbidden</title></head>'
                          +'<body><h1>Forbidden</h1>Request was forbidden.'
                          +'</body></html>')
        self.assertEquals(req.path, self.getGuardPath())
    def testErrorPage_httpAuth_deep(self):
        """Failed HTTP Auth results in a 403 error."""
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'),
                          IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest('%s/quux/thud' % self.getGuardPath(),
                                   "test", "invalid-password")
        self.assertEquals(req.headers.get('location', None), None)
        self.assertEquals(req.code, 403)
        self.assertEquals(req.written.getvalue(),
                          '<html><head><title>Forbidden</title></head>'
                          +'<body><h1>Forbidden</h1>Request was forbidden.'
                          +'</body></html>')
        self.assertEquals(req.path, '%s/quux/thud' % self.getGuardPath())
    def testErrorPage_getLogin(self):
        """Failed normal login results in anonymous view of the same page."""
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'),
                          IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest(
            '%s/__login__?username=test&password=invalid-password'
            % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), 'No')
        wanted = self.getGuardPath()
        if wanted == '':
            wanted = '/'
        self.assertEquals(req.path, wanted)
    def testErrorPage_getLogin_deep(self):
        """Failed normal login results in anonymous view of the same page."""
        p = self.createPortal()
        p.registerChecker(InMemoryUsernamePasswordDatabaseDontUse(test='test'),
                          IUsernamePassword)
        chan = self.createGuard(p)
        req = chan.makeFakeRequest(
            '%s/__login__/quux/thud?username=test&password=invalid-password'
            % self.getGuardPath()).followAllRedirects()
        self.assertEquals(req.written.getvalue(), 'No')
        self.assertEquals(req.path, '%s/quux/thud' % self.getGuardPath())
class ParentPage(rend.Page):
    addSlash = True
    def renderHTTP(self, context):
        request = context.locate(inevow.IRequest)
        return 'This is %s at %s' % (self.original, request.URLPath())
class GuardTest(GuardTestSuper, GuardTestFuncs):
    guardPath = []
class GuardTest_NotAtRoot_oneLevel(GuardTestSuper, GuardTestFuncs):
    guardPath = ['foo']
class GuardTest_NotAtRoot_manyLevels(GuardTestSuper, GuardTestFuncs):
    guardPath = ['foo', 'bar', 'baz']
class SillyPage(rend.Page):
    def locateChild(self, request, segments):
        return self.__class__(), segments[1:]
class SillyAvatar(SillyPage):
    def renderHTTP(self, request):
        return 'Yes'
class SillyAnonymous(SillyPage):
    def renderHTTP(self, ctx):
        if ctx.arg('aFormArgument'):
            return "We got the form."
        return 'No'
class SillyRealm:
    __implements__ = IRealm
    def requestAvatar(self, avatarId, mind, *interfaces):
        if avatarId == ANONYMOUS:
            return inevow.IResource, SillyAnonymous(), lambda: None
        else:
            return inevow.IResource, SillyAvatar(), lambda: None
class LeakyPage(SillyPage):
    def renderHTTP(self, request):
        return "woo"
class LeakyRealm:
    __implements__ = IRealm
    def requestAvatar(self, avatarId, mind, *interfaces):
        if avatarId == ANONYMOUS:
            return inevow.IResource, SillyAnonymous(), lambda: None
        else:
            return inevow.IResource, LeakyPage(), lambda: None
class LeakTest(GuardTestSuper):
    def makeRequest(self, sw, p, username):
        chan = self.createChannel(sw)
        req = chan.makeFakeRequest(
            '/__login__?username=%(username)s&password=%(username)s' % {'username': username}
        ).followAllRedirects()
        self.assertEquals(req.written.getvalue(), "woo")
        for i in range(10):
            req = chan.makeFakeRequest(
                '/foo/bar/baz/bamf/'
            ).followAllRedirects()
    def _leaky(self):
        p = self.createPortal(LeakyRealm)
        sw = self.createSessionWrapper(p)
        p.registerChecker(
            InMemoryUsernamePasswordDatabaseDontUse(
                test0='test0',
                test1='test1',
                test2='test2',
                test3='test3',
                test4='test4',
                test5='test5',
                test6='test6',
                test7='test7',
                test8='test8',
                test9='test9'),
            IUsernamePassword)
        for x in range(10):
            self.makeRequest(sw, p, 'test%s' % x)
    def test_leak(self):
        self._leaky()
        ref = [x for x in gc.get_referrers(context.PageContext) if isinstance(x, context.PageContext)]
