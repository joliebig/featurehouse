"""Test methods in twisted.internet.threads and reactor thread APIs."""
import sys, os
from twisted.trial import unittest
from twisted.internet import reactor, defer, interfaces, threads, protocol, error
from twisted.python import failure, threadable, log
class ReactorThreadsTestCase(unittest.TestCase):
    """
    Tests for the reactor threading API.
    """
    def testSuggestThreadPoolSize(self):
        reactor.suggestThreadPoolSize(34)
        reactor.suggestThreadPoolSize(4)
    def testCallInThread(self):
        waiter = threading.Event()
        result = []
        def threadedFunc():
            result.append(threadable.isInIOThread())
            waiter.set()
        reactor.callInThread(threadedFunc)
        waiter.wait(120)
        if not waiter.isSet():
            self.fail("Timed out waiting for event.")
        else:
            self.assertEquals(result, [False])
    def testCallFromThread(self):
        firedByReactorThread = defer.Deferred()
        firedByOtherThread = defer.Deferred()
        def threadedFunc():
            reactor.callFromThread(firedByOtherThread.callback, None)
        reactor.callInThread(threadedFunc)
        reactor.callFromThread(firedByReactorThread.callback, None)
        return defer.DeferredList(
            [firedByReactorThread, firedByOtherThread],
            fireOnOneErrback=True)
    def testWakerOverflow(self):
        self.failure = None
        waiter = threading.Event()
        def threadedFunction():
            for i in xrange(100000):
                try:
                    reactor.callFromThread(lambda: None)
                except:
                    self.failure = failure.Failure()
                    break
            waiter.set()
        reactor.callInThread(threadedFunction)
        waiter.wait(120)
        if not waiter.isSet():
            self.fail("Timed out waiting for event")
        if self.failure is not None:
            return defer.fail(self.failure)
class Counter:
    index = 0
    problem = 0
    def add(self):
        """A non thread-safe method."""
        next = self.index + 1
        if next != self.index + 1:
            self.problem = 1
            raise ValueError
        self.index = next
class DeferredResultTestCase(unittest.TestCase):
    """
    Test twisted.internet.threads.
    """
    def setUp(self):
        reactor.suggestThreadPoolSize(8)
    def tearDown(self):
        reactor.suggestThreadPoolSize(0)
    def testCallMultiple(self):
        L = []
        N = 10
        d = defer.Deferred()
        def finished():
            self.assertEquals(L, range(N))
            d.callback(None)
        threads.callMultipleInThread([
            (L.append, (i,), {}) for i in xrange(N)
            ] + [(reactor.callFromThread, (finished,), {})])
        return d
    def testDeferredResult(self):
        d = threads.deferToThread(lambda x, y=5: x + y, 3, y=4)
        d.addCallback(self.assertEquals, 7)
        return d
    def testDeferredFailure(self):
        class NewError(Exception):
            pass
        def raiseError():
            raise NewError
        d = threads.deferToThread(raiseError)
        return self.assertFailure(d, NewError)
    def testDeferredFailure2(self):
        d = threads.deferToThread(lambda: None)
        d.addCallback(lambda ign: threads.deferToThread(lambda: 1/0))
        return self.assertFailure(d, ZeroDivisionError)
_callBeforeStartupProgram = """
import time
import %(reactor)s
%(reactor)s.install()
from twisted.internet import reactor
def threadedCall():
    print 'threaded call'
reactor.callInThread(threadedCall)
for i in xrange(100):
    time.sleep(0.0)
"""
class ThreadStartupProcessProtocol(protocol.ProcessProtocol):
    def __init__(self, finished):
        self.finished = finished
        self.out = []
        self.err = []
    def outReceived(self, out):
        self.out.append(out)
    def errReceived(self, err):
        self.err.append(err)
    def processEnded(self, reason):
        self.finished.callback((self.out, self.err, reason))
class StartupBehaviorTestCase(unittest.TestCase):
    """
    Test cases for the behavior of the reactor threadpool near startup
    boundary conditions.
    In particular, this asserts that no threaded calls are attempted
    until the reactor starts up, that calls attempted before it starts
    are in fact executed once it has started, and that in both cases,
    the reactor properly cleans itself up (which is tested for
    somewhat implicitly, by requiring a child process be able to exit,
    something it cannot do unless the threadpool has been properly
    torn down).
    """
    def testCallBeforeStartupUnexecuted(self):
        progname = self.mktemp()
        progfile = file(progname, 'w')
        progfile.write(_callBeforeStartupProgram % {'reactor': reactor.__module__})
        progfile.close()
        def programFinished((out, err, reason)):
            if reason.check(error.ProcessTerminated):
                self.fail("Process did not exit cleanly (out: %s err: %s)" % (out, err))
            if err:
                log.msg("Unexpected output on standard error: %s" % (err,))
            self.failIf(out, "Expected no output, instead received:\n%s" % (out,))
        def programTimeout(err):
            err.trap(error.TimeoutError)
            proto.signalProcess('KILL')
            return err
        env = os.environ.copy()
        env['PYTHONPATH'] = os.pathsep.join(sys.path)
        d = defer.Deferred().addCallbacks(programFinished, programTimeout)
        proto = ThreadStartupProcessProtocol(d)
        reactor.spawnProcess(proto, sys.executable, ('python', progname), env)
        return d
if interfaces.IReactorThreads(reactor, None) is None:
    for cls in (ReactorThreadsTestCase,
                DeferredResultTestCase,
                StartupBehaviorTestCase):
        cls.skip = "No thread support, nothing to test here."
else:
    import threading
if interfaces.IReactorProcess(reactor, None) is None:
    for cls in (StartupBehaviorTestCase,):
        cls.skip = "No process support, cannot run subprocess thread tests."
