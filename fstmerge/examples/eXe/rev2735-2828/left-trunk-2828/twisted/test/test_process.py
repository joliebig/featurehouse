"""
Test running processes.
"""
from __future__ import nested_scopes, generators
from twisted.trial import unittest
from twisted.trial.util import spinUntil, spinWhile
from twisted.python import log
import gzip
import os
import popen2
import time
import sys
import signal
import shutil
try:
    import cStringIO as StringIO
except ImportError:
    import StringIO
from twisted.internet import reactor, protocol, error, interfaces, defer
from twisted.python import util, runtime, components
from twisted.python import procutils
class TrivialProcessProtocol(protocol.ProcessProtocol):
    finished = 0
    def processEnded(self, reason):
        self.finished = 1
        self.reason = reason
class TestProcessProtocol(protocol.ProcessProtocol):
    finished = 0
    def connectionMade(self):
        self.stages = [1]
        self.data = ''
        self.err = ''
        self.transport.write("abcd")
    def outReceived(self, data):
        self.data = self.data + data
    def outConnectionLost(self):
        self.stages.append(2)
        if self.data != "abcd":
            raise RuntimeError
        self.transport.write("1234")
    def errReceived(self, data):
        self.err = self.err + data
    def errConnectionLost(self):
        self.stages.append(3)
        if self.err != "1234":
            print 'err != 1234: ' + repr(self.err)
            raise RuntimeError()
        self.transport.write("abcd")
        self.stages.append(4)
    def inConnectionLost(self):
        self.stages.append(5)
    def processEnded(self, reason):
        self.finished = 1
        self.reason = reason
class EchoProtocol(protocol.ProcessProtocol):
    s = "1234567" * 11
    n = 10
    finished = 0
    failure = None
    def __init__(self, onEnded):
        self.onEnded = onEnded
        self.count = 0
    def connectionMade(self):
        assert self.n > 2
        for i in range(self.n - 2):
            self.transport.write(self.s)
        self.transport.writeSequence([self.s, self.s])
        self.buffer = self.s * self.n
    def outReceived(self, data):
        if buffer(self.buffer, self.count, len(data)) != buffer(data):
            self.failure = ("wrong bytes received", data, self.count)
            self.transport.closeStdin()
        else:
            self.count += len(data)
            if self.count == len(self.buffer):
                self.transport.closeStdin()
    def processEnded(self, reason):
        self.finished = 1
        if not reason.check(error.ProcessDone):
            self.failure = "process didn't terminate normally: " + str(reason)
        self.onEnded.callback(self)
class SignalProtocol(protocol.ProcessProtocol):
    def __init__(self, sig, testcase):
        self.signal = sig
        self.going = 1
        self.testcase = testcase
    def outReceived(self, data):
        self.transport.signalProcess(self.signal)
    def processEnded(self, reason):
        self.going = 0
        if not reason.check(error.ProcessTerminated):
            self.failure = "wrong termination: %s" % reason
            return
        v = reason.value
        if v.exitCode is not None:
            self.failure = "SIG%s: exitCode is %s, not None" % \
                           (self.signal, v.exitCode)
            return
        if v.signal != getattr(signal,'SIG'+self.signal):
            self.failure = "SIG%s: .signal was %s, wanted %s" % \
                           (self.signal, v.signal,
                            getattr(signal,'SIG'+self.signal))
            return
        if os.WTERMSIG(v.status) != getattr(signal,'SIG'+self.signal):
            self.failure = 'SIG%s: %s' % (self.signal,
                                          os.WTERMSIG(v.status))
            return
        self.failure = None
class SignalMixin:
    sigchldHandler = None
    def setUpClass(self):
        if hasattr(reactor, "_handleSigchld") and hasattr(signal, "SIGCHLD"):
            log.msg("Installing SIGCHLD signal handler.")
            self.sigchldHandler = signal.signal(signal.SIGCHLD,
                                                reactor._handleSigchld)
        else:
            log.msg("Skipped installing SIGCHLD signal handler.")
    def tearDownClass(self):
        if self.sigchldHandler:
            log.msg("Uninstalled SIGCHLD signal handler.")
            signal.signal(signal.SIGCHLD, self.sigchldHandler)
class TestManyProcessProtocol(TestProcessProtocol):
    def __init__(self):
        self.deferred = defer.Deferred()
    def processEnded(self, reason):
        TestProcessProtocol.processEnded(self, reason)
        if reason.check(error.ProcessDone):
            self.deferred.callback(None)
        else:
            self.deferred.errback(reason)
class ProcessTestCase(SignalMixin, unittest.TestCase):
    """Test running a process."""
    usePTY = False
    def testStdio(self):
        """twisted.internet.stdio test."""
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_twisted.py")
        p = Accumulator()
        p.endedDeferred = defer.Deferred()
        d = p.endedDeferred
        env = {"PYTHONPATH": os.pathsep.join(sys.path)}
        reactor.spawnProcess(p, exe, [exe, "-u", scriptPath], env=env,
                             path=None, usePTY=self.usePTY)
        p.transport.write("hello, world")
        p.transport.write("abc")
        p.transport.write("123")
        p.transport.closeStdin()
        def processEnded(ign):
            self.assertEquals(p.outF.getvalue(), "hello, worldabc123",
                              "Output follows:\n"
                              "%s\n"
                              "Error message from process_twisted follows:\n"
                              "%s\n" % (p.outF.getvalue(), p.errF.getvalue()))
        return d.addCallback(processEnded)
    def testProcess(self):
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_tester.py")
        p = TestProcessProtocol()
        reactor.spawnProcess(p, exe, [exe, "-u", scriptPath], env=None)
        spinUntil(lambda :p.finished, 10)
        self.failUnless(p.finished)
        self.assertEquals(p.stages, [1, 2, 3, 4, 5])
        f = p.reason
        f.trap(error.ProcessTerminated)
        self.assertEquals(f.value.exitCode, 23)
        try:
            import process_tester, glob
            for f in glob.glob(process_tester.test_file_match):
                os.remove(f)
        except:
            pass
    def testManyProcesses(self):
        def _check(results, protocols):
            for p in protocols:
                self.failUnless(p.finished)
                self.assertEquals(p.stages, [1, 2, 3, 4, 5], "[%d] stages = %s" % (id(p.transport), str(p.stages)))
                f = p.reason
                f.trap(error.ProcessTerminated)
                self.assertEquals(f.value.exitCode, 23)
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_tester.py")
        args = [exe, "-u", scriptPath]
        protocols = []
        deferreds = []
        for i in xrange(50):
            p = TestManyProcessProtocol()
            protocols.append(p)
            reactor.spawnProcess(p, exe, args, env=None)
            deferreds.append(p.deferred)
        deferredList = defer.DeferredList(deferreds, consumeErrors=True)
        deferredList.addCallback(_check, protocols)
        return deferredList
    def testEcho(self):
        finished = defer.Deferred()
        p = EchoProtocol(finished)
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_echoer.py")
        reactor.spawnProcess(p, exe, [exe, "-u", scriptPath], env=None)
        def asserts(ignored):
            self.failIf(p.failure, p.failure)
            self.failUnless(hasattr(p, 'buffer'))
            self.assertEquals(len(''.join(p.buffer)), len(p.s * p.n))
        def takedownProcess(err):
            p.transport.closeStdin()
            return err
        return finished.addCallback(asserts).addErrback(takedownProcess)
    testEcho.timeout = 60 # XXX This should not be.  There is already a
    def testCommandLine(self):
        args = [r'a\"b ', r'a\b ', r' a\\"b', r' a\\b', r'"foo bar" "', '\tab', '"\\', 'a"b', "a'b"]
        pyExe = sys.executable
        scriptPath = util.sibpath(__file__, "process_cmdline.py")
        p = Accumulator()
        p.endedDeferred = defer.Deferred()
        d = p.endedDeferred
        reactor.spawnProcess(p, pyExe, [pyExe, "-u", scriptPath]+args, env=None,
                             path=None)
        def processEnded(ign):
            self.assertEquals(p.errF.getvalue(), "")
            recvdArgs = p.outF.getvalue().splitlines()
            self.assertEquals(recvdArgs, args)
        return d.addCallback(processEnded)
class TwoProcessProtocol(protocol.ProcessProtocol):
    finished = 0
    num = -1
    def outReceived(self, data):
        pass
    def processEnded(self, reason):
        self.finished = 1
class TestTwoProcessesBase:
    def setUp(self):
        self.processes = [None, None]
        self.pp = [None, None]
        self.done = 0
        self.verbose = 0
    def createProcesses(self, usePTY=0):
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_reader.py")
        for num in (0,1):
            self.pp[num] = TwoProcessProtocol()
            self.pp[num].num = num
            p = reactor.spawnProcess(self.pp[num],
                                     exe, [exe, "-u", scriptPath], env=None,
                                     usePTY=usePTY)
            self.processes[num] = p
    def close(self, num):
        if self.verbose: print "closing stdin [%d]" % num
        p = self.processes[num]
        pp = self.pp[num]
        self.failIf(pp.finished, "Process finished too early")
        p.loseConnection()
        if self.verbose: print self.pp[0].finished, self.pp[1].finished
    def check(self):
        if self.pp[0].finished and self.pp[1].finished:
            self.done = 1
        return self.done
    def testClose(self):
        if self.verbose: print "starting processes"
        self.createProcesses()
        reactor.callLater(1, self.close, 0)
        reactor.callLater(2, self.close, 1)
        spinUntil(self.check, 5)
class TestTwoProcessesNonPosix(TestTwoProcessesBase, SignalMixin, unittest.TestCase):
    pass
class TestTwoProcessesPosix(TestTwoProcessesBase, SignalMixin, unittest.TestCase):
    def tearDown(self):
        self.check()
        for i in (0,1):
            pp, process = self.pp[i], self.processes[i]
            if not pp.finished:
                try:
                    os.kill(process.pid, signal.SIGTERM)
                except OSError:
                    print "OSError"
        spinUntil(self.check, 5, msg="unable to shutdown child processes")
    def kill(self, num):
        if self.verbose: print "kill [%d] with SIGTERM" % num
        p = self.processes[num]
        pp = self.pp[num]
        self.failIf(pp.finished, "Process finished too early")
        os.kill(p.pid, signal.SIGTERM)
        if self.verbose: print self.pp[0].finished, self.pp[1].finished
    def testKill(self):
        if self.verbose: print "starting processes"
        self.createProcesses(usePTY=0)
        reactor.callLater(1, self.kill, 0)
        reactor.callLater(2, self.kill, 1)
        spinUntil(self.check, 5)
    def testClosePty(self):
        if self.verbose: print "starting processes"
        self.createProcesses(usePTY=1)
        reactor.callLater(1, self.close, 0)
        reactor.callLater(2, self.close, 1)
        spinUntil(self.check, 5)
    def testKillPty(self):
        if self.verbose: print "starting processes"
        self.createProcesses(usePTY=1)
        reactor.callLater(1, self.kill, 0)
        reactor.callLater(2, self.kill, 1)
        spinUntil(self.check, 5)
class FDChecker(protocol.ProcessProtocol):
    state = 0
    data = ""
    done = False
    failed = None
    def fail(self, why):
        self.failed = why
        self.done = True
    def connectionMade(self):
        self.transport.writeToChild(0, "abcd")
        self.state = 1
    def childDataReceived(self, childFD, data):
        if self.state == 1:
            if childFD != 1:
                self.fail("read '%s' on fd %d (not 1) during state 1" \
                          % (childFD, data))
                return
            self.data += data
            if len(self.data) == 6:
                if self.data != "righto":
                    self.fail("got '%s' on fd1, expected 'righto'" \
                              % self.data)
                    return
                self.data = ""
                self.state = 2
                self.transport.writeToChild(3, "efgh")
                return
        if self.state == 2:
            self.fail("read '%s' on fd %s during state 2" % (childFD, data))
            return
        if self.state == 3:
            if childFD != 1:
                self.fail("read '%s' on fd %s (not 1) during state 3" \
                          % (childFD, data))
                return
            self.data += data
            if len(self.data) == 6:
                if self.data != "closed":
                    self.fail("got '%s' on fd1, expected 'closed'" \
                              % self.data)
                    return
                self.state = 4
            return
        if self.state == 4:
            self.fail("read '%s' on fd %s during state 4" % (childFD, data))
            return
    def childConnectionLost(self, childFD):
        if self.state == 1:
            self.fail("got connectionLost(%d) during state 1" % childFD)
            return
        if self.state == 2:
            if childFD != 4:
                self.fail("got connectionLost(%d) (not 4) during state 2" \
                          % childFD)
                return
            self.state = 3
            self.transport.closeChildFD(5)
            return
    def processEnded(self, status):
        rc = status.value.exitCode
        if self.state != 4:
            self.fail("processEnded early, rc %d" % rc)
            return
        if status.value.signal != None:
            self.fail("processEnded with signal %s" % status.value.signal)
            return
        if rc != 0:
            self.fail("processEnded with rc %d" % rc)
            return
        self.done = True
class FDTest(SignalMixin, unittest.TestCase):
    def NOTsetUp(self):
        from twisted.internet import process
        process.Process.debug_child = True
    def NOTtearDown(self):
        from twisted.internet import process
        process.Process.debug_child = False
    def testFD(self):
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_fds.py")
        p = FDChecker()
        reactor.spawnProcess(p, exe, [exe, "-u", scriptPath], env=None,
                             path=None,
                             childFDs={0:"w", 1:"r", 2:2,
                                       3:"w", 4:"r", 5:"w"})
        spinUntil(lambda :p.done, 5)
        self.failIf(p.failed, p.failed)
    def testLinger(self):
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_linger.py")
        p = Accumulator()
        p.endedDeferred = defer.Deferred()
        d = p.endedDeferred
        reactor.spawnProcess(p, exe, [exe, "-u", scriptPath], env=None,
                             path=None,
                             childFDs={1:"r", 2:2},
                             )
        def processEnded(ign):
            self.failUnlessEqual(p.outF.getvalue(),
                                 "here is some text\ngoodbye\n")
        return d.addCallback(processEnded)
class Accumulator(protocol.ProcessProtocol):
    """Accumulate data from a process."""
    closed = 0
    endedDeferred = None
    def connectionMade(self):
        self.outF = StringIO.StringIO()
        self.errF = StringIO.StringIO()
    def outReceived(self, d):
        self.outF.write(d)
    def errReceived(self, d):
        self.errF.write(d)
    def outConnectionLost(self):
        pass
    def errConnectionLost(self):
        pass
    def processEnded(self, reason):
        self.closed = 1
        if self.endedDeferred is not None:
            d, self.endedDeferred = self.endedDeferred, None
            d.callback(None)
class PosixProcessBase:
    """Test running processes."""
    usePTY = 0
    def testNormalTermination(self):
        if os.path.exists('/bin/true'): cmd = '/bin/true'
        elif os.path.exists('/usr/bin/true'): cmd = '/usr/bin/true'
        else: raise RuntimeError("true not found in /bin or /usr/bin")
        p = TrivialProcessProtocol()
        reactor.spawnProcess(p, cmd, ['true'], env=None,
                             usePTY=self.usePTY)
        spinUntil(lambda :p.finished)
        p.reason.trap(error.ProcessDone)
        self.assertEquals(p.reason.value.exitCode, 0)
        self.assertEquals(p.reason.value.signal, None)
    def testAbnormalTermination(self):
        if os.path.exists('/bin/false'): cmd = '/bin/false'
        elif os.path.exists('/usr/bin/false'): cmd = '/usr/bin/false'
        else: raise RuntimeError("false not found in /bin or /usr/bin")
        p = TrivialProcessProtocol()
        reactor.spawnProcess(p, cmd, ['false'], env=None,
                             usePTY=self.usePTY)
        spinUntil(lambda :p.finished)
        p.reason.trap(error.ProcessTerminated)
        self.assertEquals(p.reason.value.exitCode, 1)
        self.assertEquals(p.reason.value.signal, None)
    def testSignal(self):
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_signal.py")
        signals = ('HUP', 'INT', 'KILL')
        for sig in signals:
            p = SignalProtocol(sig, self)
            reactor.spawnProcess(p, exe, [exe, "-u", scriptPath, sig],
                                 env=None,
                                 usePTY=self.usePTY)
            spinWhile(lambda :p.going)
            self.failIf(p.failure, p.failure)
class PosixProcessTestCase(SignalMixin, unittest.TestCase, PosixProcessBase):
    def testStderr(self):
        if not os.path.exists('/bin/ls'):
            raise RuntimeError("/bin/ls not found")
        p = Accumulator()
        p.endedDeferred = defer.Deferred()
        d = p.endedDeferred
        reactor.spawnProcess(p, '/bin/ls',
                             ["/bin/ls",
                              "ZZXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"],
                             env=None, path="/tmp",
                             usePTY=self.usePTY)
        def processEnded(ign):
            self.assertEquals(lsOut, p.errF.getvalue())
        return d.addCallback(processEnded)
    def testProcess(self):
        if os.path.exists('/bin/gzip'): cmd = '/bin/gzip'
        elif os.path.exists('/usr/bin/gzip'): cmd = '/usr/bin/gzip'
        else: raise RuntimeError("gzip not found in /bin or /usr/bin")
        s = "there's no place like home!\n" * 3
        p = Accumulator()
        p.endedDeferred = defer.Deferred()
        d = p.endedDeferred
        reactor.spawnProcess(p, cmd, [cmd, "-c"], env=None, path="/tmp",
                             usePTY=self.usePTY)
        p.transport.write(s)
        p.transport.closeStdin()
        def processEnded(ign):
            f = p.outF
            f.seek(0, 0)
            gf = gzip.GzipFile(fileobj=f)
            self.assertEquals(gf.read(), s)
        return d.addCallback(processEnded)
class PosixProcessTestCasePTY(SignalMixin, unittest.TestCase, PosixProcessBase):
    """Just like PosixProcessTestCase, but use ptys instead of pipes."""
    usePTY = 1
    def testOpeningTTY(self):
        exe = sys.executable
        scriptPath = util.sibpath(__file__, "process_tty.py")
        p = Accumulator()
        p.endedDeferred = defer.Deferred()
        d = p.endedDeferred
        reactor.spawnProcess(p, exe, [exe, "-u", scriptPath], env=None,
                            path=None, usePTY=self.usePTY)
        p.transport.write("hello world!\n")
        def processEnded(ign):
            self.assertEquals(
                p.outF.getvalue(),
                "hello world!\r\nhello world!\r\n",
                "Error message from process_tty follows:\n\n%s\n\n" % p.outF.getvalue())
        return d.addCallback(processEnded)
    def testBadArgs(self):
        pyExe = sys.executable
        pyArgs = [pyExe, "-u", "-c", "print 'hello'"]
        p = Accumulator()
        self.assertRaises(ValueError, reactor.spawnProcess, p, pyExe, pyArgs, usePTY=1, childFDs={1:'r'})
class Win32ProcessTestCase(SignalMixin, unittest.TestCase):
    """Test process programs that are packaged with twisted."""
    def testStdinReader(self):
        pyExe = sys.executable
        scriptPath = util.sibpath(__file__, "process_stdinreader.py")
        p = Accumulator()
        p.endedDeferred = defer.Deferred()
        d = p.endedDeferred
        reactor.spawnProcess(p, pyExe, [pyExe, "-u", scriptPath], env=None,
                             path=None)
        p.transport.write("hello, world")
        p.transport.closeStdin()
        def processEnded(ign):
            self.assertEquals(p.errF.getvalue(), "err\nerr\n")
            self.assertEquals(p.outF.getvalue(), "out\nhello, world\nout\n")
        return d.addCallback(processEnded)
    def testBadArgs(self):
        pyExe = sys.executable
        pyArgs = [pyExe, "-u", "-c", "print 'hello'"]
        p = Accumulator()
        self.assertRaises(ValueError, reactor.spawnProcess, p, pyExe, pyArgs, uid=1)
        self.assertRaises(ValueError, reactor.spawnProcess, p, pyExe, pyArgs, gid=1)
        self.assertRaises(ValueError, reactor.spawnProcess, p, pyExe, pyArgs, usePTY=1)
        self.assertRaises(ValueError, reactor.spawnProcess, p, pyExe, pyArgs, childFDs={1:'r'})
class UtilTestCase(unittest.TestCase):
    def setUpClass(klass):
        j = os.path.join
        foobar = j("foo", "bar")
        foobaz = j("foo", "baz")
        bazfoo = j("baz", "foo")
        barfoo = j("baz", "bar")
        for d in "foo", foobar, foobaz, "baz", bazfoo, barfoo:
            if os.path.exists(d):
                shutil.rmtree(d, True)
            os.mkdir(d)
        f = file(j(foobaz, "executable"), "w")
        f.close()
        os.chmod(j(foobaz, "executable"), 0700)
        f = file(j("foo", "executable"), "w")
        f.close()
        os.chmod(j("foo", "executable"), 0700)
        f = file(j(bazfoo, "executable"), "w")
        f.close()
        os.chmod(j(bazfoo, "executable"), 0700)
        f = file(j(bazfoo, "executable.bin"), "w")
        f.close()
        os.chmod(j(bazfoo, "executable.bin"), 0700)
        f = file(j(barfoo, "executable"), "w")
        f.close()
        klass.oldPath = os.environ['PATH']
        os.environ['PATH'] = os.pathsep.join((foobar, foobaz, bazfoo, barfoo))
    def tearDownClass(klass):
        j = os.path.join
        os.environ['PATH'] = klass.oldPath
        foobar = j("foo", "bar")
        foobaz = j("foo", "baz")
        bazfoo = j("baz", "foo")
        barfoo = j("baz", "bar")
        os.remove(j(foobaz, "executable"))
        os.remove(j("foo", "executable"))
        os.remove(j(bazfoo, "executable"))
        os.remove(j(bazfoo, "executable.bin"))
        os.remove(j(barfoo, "executable"))
        for d in foobar, foobaz, bazfoo, barfoo, "foo", "baz":
            os.rmdir(d)
    def testWhich(self):
        j = os.path.join
        paths = procutils.which("executable")
        self.assertEquals(paths, [
            j("foo", "baz", "executable"), j("baz", "foo", "executable")
        ])
    def testWhichPathExt(self):
        j = os.path.join
        old = os.environ.get('PATHEXT', None)
        os.environ['PATHEXT'] = os.pathsep.join(('.bin', '.exe', '.sh'))
        try:
            paths = procutils.which("executable")
        finally:
            if old is None:
                del os.environ['PATHEXT']
            else:
                os.environ['PATHEXT'] = old
        self.assertEquals(paths, [
            j("foo", "baz", "executable"), j("baz", "foo", "executable"),
            j("baz", "foo", "executable.bin")
        ])
class ClosingPipesProcessProtocol(protocol.ProcessProtocol):
    output = ''
    errput = ''
    def __init__(self, outOrErr):
        self.deferred = defer.Deferred()
        self.outOrErr = outOrErr
    def processEnded(self, reason):
        self.deferred.callback(reason)
    def outReceived(self, data):
        self.output += data
    def errReceived(self, data):
        self.errput += data
class ClosingPipes(unittest.TestCase):
    def doit(self, fd):
        p = ClosingPipesProcessProtocol(True)
        p.deferred.addCallbacks(
            callback=lambda _: self.fail("I wanted an errback."),
            errback=self._endProcess, errbackArgs=(p,))
        reactor.spawnProcess(p, sys.executable,
                             [sys.executable, '-u', '-c',
                              r'raw_input(); import sys, os; os.write(%d, "foo\n"); sys.exit(42)' % fd],
                             env=None)
        p.transport.write('go\n')
        if fd == 1:
            p.transport.closeStdout()
        elif fd == 2:
            p.transport.closeStderr()
        else:
            raise RuntimeError
        p.transport.closeStdin()
        return p.deferred
    def _endProcess(self, reason, p):
        self.failIf(reason.check(error.ProcessDone),
                    'Child should fail due to EPIPE.')
        reason.trap(error.ProcessTerminated)
        self.failIfEqual(reason.value.exitCode, 42,
                         'process reason was %r' % reason)
        self.failUnlessEqual(p.output, '')
        return p.errput
    def test_stdout(self):
        """ProcessProtocol.transport.closeStdout actually closes the pipe."""
        d = self.doit(1)
        def _check(errput):
            self.failIfEqual(errput.find('OSError'), -1)
            if runtime.platform.getType() != 'win32':
                self.failIfEqual(errput.find('Broken pipe'), -1)
        d.addCallback(_check)
        return d
    def test_stderr(self):
        """ProcessProtocol.transport.closeStderr actually closes the pipe."""
        d = self.doit(2)
        def _check(errput):
            self.failUnlessEqual(errput, '')
        d.addCallback(_check)
        return d
skipMessage = "wrong platform or reactor doesn't support IReactorProcess"
if (runtime.platform.getType() != 'posix') or (not interfaces.IReactorProcess(reactor, None)):
    PosixProcessTestCase.skip = skipMessage
    PosixProcessTestCasePTY.skip = skipMessage
    TestTwoProcessesPosix.skip = skipMessage
    FDTest.skip = skipMessage
else:
    lsOut = popen2.popen3("/bin/ls ZZXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")[2].read()
if (runtime.platform.getType() != 'win32') or (not interfaces.IReactorProcess(reactor, None)):
    Win32ProcessTestCase.skip = skipMessage
    TestTwoProcessesNonPosix.skip = skipMessage
if runtime.platform.getType() == 'win32':
    UtilTestCase.todo = "do not assume that platform retains 'executable' mode"
if not interfaces.IReactorProcess(reactor, None):
    ProcessTestCase.skip = skipMessage
    ClosingPipes.skip = skipMessage
