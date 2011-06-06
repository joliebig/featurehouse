"""
Test running processes.
"""
import warnings, os, sys, signal
from twisted.trial import unittest
from twisted.test.test_process import SignalMixin
from twisted.internet import reactor, utils, interfaces
class UtilsTestCase(SignalMixin, unittest.TestCase):
    """Test running a process."""
    output = None
    value = None
    def makeSourceFile(self, sourceLines):
        script = self.mktemp()
        scriptFile = file(script, 'wt')
        scriptFile.write(os.linesep.join(sourceLines) + os.linesep)
        scriptFile.close()
        return script
    def testOutput(self):
        scriptFile = self.makeSourceFile([
            'print "hello world"'
            ])
        d = utils.getProcessOutput(sys.executable, ['-u', scriptFile])
        return d.addCallback(self.assertEquals, "hello world\n")
    def testOutputWithErrorIgnored(self):
        exe = sys.executable
        scriptFile = self.makeSourceFile([
            'import sys',
            'sys.stderr.write("hello world\\n")'
            ])
        d = utils.getProcessOutput(exe, ['-u', scriptFile], errortoo=0)
        return self.assertFailure(d, IOError)
    def testOutputWithErrorCollected(self):
        exe = sys.executable
        scriptFile = self.makeSourceFile([
            'import sys',
            'sys.stderr.write("hello world\\n")'
            ])
        d = utils.getProcessOutput(exe, ['-u', scriptFile], errortoo=1)
        return d.addCallback(self.assertEquals, "hello world" + os.linesep)
    def testValue(self):
        exe = sys.executable
        scriptFile = self.makeSourceFile([
            "import sys",
            "sys.exit(1)"
            ])
        d = utils.getProcessValue(exe, ['-u', scriptFile])
        return d.addCallback(self.assertEquals, 1)
    def testOutputAndValue(self):
        exe = sys.executable
        scriptFile = self.makeSourceFile([
            "import sys",
            "sys.stdout.write('hello world!\\n')",
            "sys.stderr.write('goodbye world!\\n')",
            "sys.exit(1)"
            ])
        def gotOutputAndValue((out, err, code)):
            self.assertEquals(out, "hello world!" + os.linesep)
            self.assertEquals(err, "goodbye world!" + os.linesep)
            self.assertEquals(code, 1)
        d = utils.getProcessOutputAndValue(exe, [scriptFile])
        return d.addCallback(gotOutputAndValue)
    def testOutputSignal(self):
        exe = sys.executable
        scriptFile = self.makeSourceFile([
            "import sys, os, signal",
            "sys.stdout.write('stdout bytes\\n')",
            "sys.stderr.write('stderr bytes\\n')",
            "sys.stdout.flush()",
            "sys.stderr.flush()",
            "os.kill(os.getpid(), signal.SIGKILL)"
            ])
        def gotOutputAndValue(err):
            (out, err, sig) = err.value # XXX Sigh wtf
            self.assertEquals(out, "stdout bytes" + os.linesep)
            self.assertEquals(err, "stderr bytes" + os.linesep)
            self.assertEquals(sig, signal.SIGKILL)
        d = utils.getProcessOutputAndValue(exe, ['-u', scriptFile])
        return d.addErrback(gotOutputAndValue)
class WarningSuppression(unittest.TestCase):
    def setUp(self):
        self.warnings = []
        self.originalshow = warnings.showwarning
        warnings.showwarning = self.showwarning
    def tearDown(self):
        warnings.showwarning = self.originalshow
    def showwarning(self, *a, **kw):
        self.warnings.append((a, kw))
    def testSuppressWarnings(self):
        def f(msg):
            warnings.warn(msg)
        g = utils.suppressWarnings(f, (('ignore',), dict(message="This is message")))
        f("Sanity check message")
        self.assertEquals(len(self.warnings), 1)
        g("This is message")
        self.assertEquals(len(self.warnings), 1)
        g("Unignored message")
        self.assertEquals(len(self.warnings), 2)
if interfaces.IReactorProcess(reactor, None) is None:
    UtilsTestCase.skip = "reactor doesn't implement IReactorProcess"
