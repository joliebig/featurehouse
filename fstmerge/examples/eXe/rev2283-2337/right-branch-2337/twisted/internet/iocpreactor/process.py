"""Support for IReactorProcess for the IOCP proactor.
API Stability: unstable
Maintainer: U{Justin Johnson<mailto:justinjohnson@gmail.com>}
This code is potentially unstable.  I have performed numerous tests
but couldn't get someone who was knowledgable of win32 to review it.
If you run into problems please submit a bug report to
http://twistedmatrix.com/bugs.
"""
import win32api
import win32gui
import win32con
import win32file
import win32pipe
import win32process
import win32security
from win32event import CreateEvent, SetEvent, WaitForSingleObject
from win32event import MsgWaitForMultipleObjects, WAIT_OBJECT_0
from win32event import WAIT_TIMEOUT, INFINITE, QS_ALLINPUT, QS_POSTMESSAGE
from win32event import QS_ALLEVENTS
from zope.interface import implements
from twisted.internet import error
from twisted.python import failure, components
from twisted.python.win32 import cmdLineQuote
from twisted.internet.interfaces import IProcessTransport, IConsumer
import ops
import process_waiter
import os
import sys
import time
import itertools
counter = itertools.count(1)
class Process(object):
    """A process that integrates with the Twisted event loop.
    See http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/creating_a_child_process_with_redirected_input_and_output.asp
    for more info on how to create processes in Windows and access their
    stdout/err/in.  Another good source is http://www.informit.com/articles/article.asp?p=362660&seqNum=2.
    Issues:
    If your subprocess is a python program, you need to:
     - Run python.exe with the '-u' command line option - this turns on
       unbuffered I/O. Buffering stdout/err/in can cause problems, see e.g.
       http://support.microsoft.com/default.aspx?scid=kb;EN-US;q1903
     - (is this still true?) If you don't want Windows messing with data passed over
       stdin/out/err, set the pipes to be in binary mode::
        import os, sys, mscvrt
        msvcrt.setmode(sys.stdin.fileno(), os.O_BINARY)
        msvcrt.setmode(sys.stdout.fileno(), os.O_BINARY)
        msvcrt.setmode(sys.stderr.fileno(), os.O_BINARY)
    """
    implements(IProcessTransport, IConsumer)
    bufferSize = 2**2**2**2
    pipeBufferSize = bufferSize
    def __init__(self, reactor, protocol, command, args, environment, path):
        self.reactor = reactor
        self.protocol = protocol
        self.outBuffer = reactor.AllocateReadBuffer(self.bufferSize)
        self.errBuffer = reactor.AllocateReadBuffer(self.bufferSize)
        self.inBuffer = reactor.AllocateReadBuffer(self.bufferSize)
        self.readOutOp = ops.ReadOutOp(self)
        self.readErrOp = ops.ReadErrOp(self)
        self.readInOp = ops.ReadInOp(self)
        self.writeInOp = ops.WriteInOp(self)
        self.writeBuffer = ""
        self.writing = False
        self.finished = False
        self.offset = 0
        self.writeBufferedSize = 0
        self.closingStdin = False
        self.closedStdin = False
        self.closedStdout = False
        self.closedStderr = False
        self.hChildStdinRd = None
        self.hChildStdinWr = None
        self.hChildStdinWrDup = None
        self.hChildStdoutRd = None
        self.hChildStdoutWr = None
        self.hChildStdoutRdDup = None
        self.hChildStderrRd = None
        self.hChildStderrWr = None
        self.hChildStderrRdDup = None
        self.closedNotifies = 0  # increments to 3 (for stdin, stdout, stderr)
        self.closed = False # set to true when all 3 handles close
        self.exited = False # set to true when WFMO thread gets signalled proc handle.  See doWaitForProcessExit.
        saAttr = win32security.SECURITY_ATTRIBUTES()
        saAttr.bInheritHandle = 1
        currentPid = win32api.GetCurrentProcess() # -1 which stands for current process
        self.pid = os.getpid() # unique pid for pipe naming
        self.stdoutPipeName = r"\\.\pipe\twisted-iocp-stdout-%d-%d-%d" % (self.pid, counter.next(), time.time())
        self.hChildStdoutRd = win32pipe.CreateNamedPipe(
                self.stdoutPipeName,
                win32con.PIPE_ACCESS_INBOUND | win32con.FILE_FLAG_OVERLAPPED, # open mode
                win32con.PIPE_TYPE_BYTE, # pipe mode
                1, # max instances
                self.pipeBufferSize, # out buffer size
                self.pipeBufferSize, # in buffer size
                0, # timeout 
                saAttr)
        self.hChildStdoutWr = win32file.CreateFile(
                self.stdoutPipeName,
                win32con.GENERIC_WRITE,
                win32con.FILE_SHARE_READ|win32con.FILE_SHARE_WRITE,
                saAttr,
                win32con.OPEN_EXISTING,
                win32con.FILE_FLAG_OVERLAPPED,
                0);
        self.hChildStdoutRdDup = win32api.DuplicateHandle(
                currentPid, self.hChildStdoutRd,
                currentPid, 0,
                0,
                win32con.DUPLICATE_SAME_ACCESS)
        win32api.CloseHandle(self.hChildStdoutRd);
        self.hChildStdoutRd = self.hChildStdoutRdDup
        self.stderrPipeName = r"\\.\pipe\twisted-iocp-stderr-%d-%d-%d" % (self.pid, counter.next(), time.time())
        self.hChildStderrRd = win32pipe.CreateNamedPipe(
                self.stderrPipeName,
                win32con.PIPE_ACCESS_INBOUND | win32con.FILE_FLAG_OVERLAPPED, # open mode
                win32con.PIPE_TYPE_BYTE, # pipe mode
                1, # max instances
                self.pipeBufferSize, # out buffer size
                self.pipeBufferSize, # in buffer size
                0, # timeout 
                saAttr)
        self.hChildStderrWr = win32file.CreateFile(
                self.stderrPipeName,
                win32con.GENERIC_WRITE,
                win32con.FILE_SHARE_READ|win32con.FILE_SHARE_WRITE,
                saAttr,
                win32con.OPEN_EXISTING,
                win32con.FILE_FLAG_OVERLAPPED,
                0);
        self.hChildStderrRdDup = win32api.DuplicateHandle(
                currentPid, self.hChildStderrRd,
                currentPid, 0,
                0,
                win32con.DUPLICATE_SAME_ACCESS)
        win32api.CloseHandle(self.hChildStderrRd)
        self.hChildStderrRd = self.hChildStderrRdDup
        self.stdinPipeName = r"\\.\pipe\twisted-iocp-stdin-%d-%d-%d" % (self.pid, counter.next(), time.time())
        self.hChildStdinWr = win32pipe.CreateNamedPipe(
                self.stdinPipeName,
                win32con.PIPE_ACCESS_DUPLEX | win32con.FILE_FLAG_OVERLAPPED, # open mode
                win32con.PIPE_TYPE_BYTE, # pipe mode
                1, # max instances
                self.pipeBufferSize, # out buffer size
                self.pipeBufferSize, # in buffer size
                0, # timeout 
                saAttr)
        self.hChildStdinRd = win32file.CreateFile(
                self.stdinPipeName,
                win32con.GENERIC_READ,
                win32con.FILE_SHARE_READ|win32con.FILE_SHARE_WRITE,
                saAttr,
                win32con.OPEN_EXISTING,
                win32con.FILE_FLAG_OVERLAPPED,
                0);
        self.hChildStdinWrDup = win32api.DuplicateHandle(
                currentPid, self.hChildStdinWr, 
                currentPid, 0, 
                0,
                win32con.DUPLICATE_SAME_ACCESS)
        win32api.CloseHandle(self.hChildStdinWr)
        self.hChildStdinWr = self.hChildStdinWrDup
        StartupInfo = win32process.STARTUPINFO()
        StartupInfo.hStdOutput = self.hChildStdoutWr
        StartupInfo.hStdError  = self.hChildStderrWr
        StartupInfo.hStdInput  = self.hChildStdinRd
        StartupInfo.dwFlags = win32process.STARTF_USESTDHANDLES
        cmdline = ' '.join([cmdLineQuote(a) for a in args])
        self.hProcess, hThread, dwPid, dwTid = win32process.CreateProcess(
                command,     # name
                cmdline,     # command line
                None,        # process security attributes
                None,        # primary thread security attributes
                1,           # handles are inherited
                0,           # creation flags
                environment, # if NULL, use parent environment
                path,        # current directory
                StartupInfo) # STARTUPINFO pointer 
        win32file.CloseHandle(self.hChildStderrWr)
        win32file.CloseHandle(self.hChildStdoutWr)
        win32file.CloseHandle(self.hChildStdinRd)
        self.readOutOp.initiateOp(self.hChildStdoutRd, self.outBuffer)
        self.readErrOp.initiateOp(self.hChildStderrRd, self.errBuffer)
        self.readInOp.initiateOp(self.hChildStdinWr, self.inBuffer)
        self.reactor.processWaiter.beginWait(self.reactor, self.hProcess, self)
        self.protocol.makeConnection(self)
    def signalProcess(self, signalID):
        if signalID in ("INT", "TERM", "KILL"):
            win32process.TerminateProcess(self.hProcess, 1)
    def startWriting(self):
        if not self.writing:
            self.writing = True
            b = buffer(self.writeBuffer, self.offset, self.offset + self.bufferSize)
            self.writeInOp.initiateOp(self.hChildStdinWr, b)
    def stopWriting(self):
        self.writing = False
    def writeDone(self, bytes):
        self.writing = False
        self.offset += bytes
        self.writeBufferedSize -= bytes
        if self.offset == len(self.writeBuffer):
            self.writeBuffer = ""
            self.offset = 0
        if self.writeBuffer == "":
            self.writing = False
            if self.closingStdin:
                self._closeStdin()
                self.connectionLostNotify()
        else:
            self.startWriting()
    def write(self, data):
        """Write data to the process' stdin."""
        self.writeBuffer += data
        self.writeBufferedSize += len(data)
        if not self.writing:
            self.startWriting()
    def writeSequence(self, seq):
        """Write a list of strings to the physical connection.
        If possible, make sure that all of the data is written to
        the socket at once, without first copying it all into a
        single string.
        """
        self.write("".join(seq))
    def closeStdin(self):
        """Close the process' stdin."""
        if not self.closingStdin:
            self.closingStdin = True
            if not self.writing:
                self._closeStdin()
                self.connectionLostNotify()
    def _closeStdin(self):
        if hasattr(self, "hChildStdinWr"):
            win32file.CloseHandle(self.hChildStdinWr)
            del self.hChildStdinWr
            self.closingStdin = False
            self.closedStdin = True
    def closeStderr(self):
        if hasattr(self, "hChildStderrRd"):
            win32file.CloseHandle(self.hChildStderrRd)
            del self.hChildStderrRd
            self.closedStderr = True
            self.connectionLostNotify()
    def closeStdout(self):
        if hasattr(self, "hChildStdoutRd"):
            win32file.CloseHandle(self.hChildStdoutRd)
            del self.hChildStdoutRd
            self.closedStdout = True
            self.connectionLostNotify()
    def loseConnection(self):
        """Close the process' stdout, in and err."""
        self.closeStdin()
        self.closeStdout()
        self.closeStderr()
    def outConnectionLost(self):
        self.closeStdout() # in case process closed it, not us
        self.protocol.outConnectionLost()
    def errConnectionLost(self):
        self.closeStderr() # in case process closed it
        self.protocol.errConnectionLost()
    def inConnectionLost(self):
        self._closeStdin()
        self.protocol.inConnectionLost()
        self.connectionLostNotify()
    def connectionLostNotify(self):
        """Will be called 3 times, for stdout/err/in."""
        self.closedNotifies = self.closedNotifies + 1
        if self.closedNotifies == 3:
            self.closed = 1
            if self.exited:
                self.connectionLost()
    def processEnded(self):
        self.exited = True
        if self.closed:
            self.connectionLost()
    def connectionLost(self, reason=None):
        """Shut down resources."""
        exitCode = win32process.GetExitCodeProcess(self.hProcess)
        if exitCode == 0:
            err = error.ProcessDone(exitCode)
        else:
            err = error.ProcessTerminated(exitCode)
        self.protocol.processEnded(failure.Failure(err))
    def registerProducer(self, producer, streaming):
        pass
    def unregisterProducer(self):
        pass
components.backwardsCompatImplements(Process)
