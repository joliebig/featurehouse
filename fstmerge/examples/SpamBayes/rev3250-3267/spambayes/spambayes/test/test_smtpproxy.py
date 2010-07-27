"""Test that the SMTP proxy is working correctly.
The -t option runs a fake SMTP server on port 8025.  This is the
same server that the testing option uses, and may be separately run for
other testing purposes.
Usage:
    test_smtpproxy.py [options]
        options:
            -t      : Runs a fake SMTP server on port 8025 (for testing).
            -h      : Displays this help message.
Any other options runs this in the standard Python unittest form.
"""

__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>"

__credits__ = "Richie Hindle, Mark Hammond, all the SpamBayes folk."

spam1 = """From: friend@public.com
Subject: Make money fast
Hello tim_chandler , Want to save money ?
Now is a good time to consider refinancing. Rates are low so you can cut
your current payments and save money.
http://64.251.22.101/interest/index%38%30%300%2E%68t%6D
Take off list on site [s5]
"""

good1 = """From: chris@example.com
Subject: ZPT and DTML
Jean Jordaan wrote:
> 'Fraid so ;>  It contains a vintage dtml-calendar tag.
>   http://www.zope.org/Members/teyc/CalendarTag
>
> Hmm I think I see what you mean: one needn't manually pass on the
> namespace to a ZPT?
Yeah, Page Templates are a bit more clever, sadly, DTML methods aren't :-(
Chris
"""

import re

import sys

import socket

import getopt

import asyncore

import operator

import unittest

import _thread

import smtplib

import sb_test_support

sb_test_support.fix_sys_path()

from spambayes import Dibbler

from spambayes import tokenizer

from spambayes.Options import options

from sb_server import state, _recreateState

from spambayes.smtpproxy import BayesSMTPProxyListener, SMTPTrainer

from spambayes.ProxyUI import ProxyUserInterface

from spambayes.UserInterface import UserInterfaceServer

from spambayes.classifier import Classifier

class  TestListener (Dibbler.Listener) :
	"""Listener for TestSMTPServer."""
	    def __init__(self, socketMap=asyncore.socket_map):

        Dibbler.Listener.__init__(self, 8025, TestSMTPServer,
                              (socketMap,), socketMap=socketMap)

class  TestSMTPServer (Dibbler.BrighterAsyncChat) :
	"""Minimal SMTP server, for testing purposes.  Understands
    "MAIL FROM", "RCPT TO", "DATA" and "QUIT".  All mail is
    simply discarded. Also understands the 'KILL' command, to
    kill it."""
	    def __init__(self, clientSocket, socketMap):

        Dibbler.BrighterAsyncChat.__init__(self, map=socketMap)

        Dibbler.BrighterAsyncChat.set_socket(self, clientSocket, socketMap)

        self.set_terminator('\r\n')

        self.okCommands = ['MAIL FROM:', 'RCPT TO:', 'DATA', 'QUIT', 'KILL',]

        self.handlers = {'MAIL FROM:': self.onFrom,
                         'RCPT TO:': self.onTo,
                         'DATA': self.onData,
                         'QUIT': self.onQuit,
                         'KILL': self.onKill,
                         }

        self.push("220 SpamBayes test SMTP server ready\r\n")

        self.request = ''

        self.inData = False
 def collect_incoming_data(self, data):

        """Asynchat override."""

        self.request = self.request + data
 def push(self, data):

        Dibbler.BrighterAsyncChat.push(self, data)
 def recv(self, buffer_size):

        """Asynchat override."""

        try:

            return Dibbler.BrighterAsyncChat.recv(self, buffer_size)

        except socket.error as e:

            if e[0] == 10035:

                return ''

            raise
 def found_terminator(self):

        """Asynchat override."""

        if self.inData:

            if self.request.strip() == '.':

                self.inData = False

                self.push("250 Message accepted for delivery\r\n")

        else:

            self.request = self.request.upper()

            foundCmd = False

            for cmd in self.okCommands:

                if self.request.startswith(cmd):

                    handler = self.handlers[cmd]

                    cooked = handler(self.request[len(cmd):])

                    if cooked is not None:

                        self.push(cooked)

                    foundCmd = True

                    break

            if not foundCmd:

                self.push("250 Unknown command %s ok.\r\n" %
                          (self.request,))

        self.request = ''
 def onKill(self, args):

        self.push("221 Goodbye\n") 

        self.socket.shutdown(2)

        self.close()

        raise SystemExit
 def onQuit(self, args):

        self.push("221 Goodbye\r\n")

        self.close_when_done()
 def onFrom(self, args):

        return "250 %s... Sender ok\r\n" % (args.lower(),)
 def onTo(self, args):

        if args == options["smtpproxy", "ham_address"].upper():

            return "504 This command should not have got to the server\r\n"

        elif args == options["smtpproxy", "spam_address"].upper():

            return "504 This command should not have got to the server\r\n"

        return "250 %s... Recipient ok\r\n" % (args.lower(),)
 def onData(self, args):

        self.inData = True

        return '354 Enter mail, end with "." on a line by itself\r\n'

class  SMTPProxyTest (unittest.TestCase) :
	"""Runs a self-test using TestSMTPServer, a minimal SMTP server
    that receives mail and discards it."""
	    def __init__(self, *args):

        unittest.TestCase.__init__(self, *args)

        self.bayes = Classifier()
 def setUp(self):

        pass
 def tearDown(self):

        pass
 def test_direct_connection(self):

        smtpServer = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        smtpServer.connect(('localhost', 8025))

        try:

            response = smtpServer.recv(100)

        except socket.error as e:

            if e[0] == 10035:

                pass

            else:

                raise

        self.assertEqual(response, "220 SpamBayes test SMTP server ready\r\n",
                         "Couldn't connect to test SMTP server")

        smtpServer.send('quit\r\n')
 def test_proxy_connection(self):

        proxy = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        proxy.connect(('localhost', 8026))

        try:

            response = proxy.recv(100)

        except socket.error as e:

            if e[0] == 10035:

                pass

            else:

                raise

        self.assertEqual(response, "220 SpamBayes test SMTP server ready\r\n",
                         "Couldn't connect to proxy server")

        proxy.send('quit\r\n')
 def test_disconnection(self):

        proxy = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        proxy.connect(('localhost', 8025))

        try:

            response = proxy.recv(100)

        except socket.error as e:

            if e[0] == 10053:

                pass

            else:

                raise

        proxy.send("quit\r\n")

        try:

            response = proxy.recv(100)

        except socket.error as e:

            if e[0] == 10053:

                pass

            else:

                raise

        self.assertEqual(response, "221 Goodbye\r\n",
                         "Couldn't disconnect from SMTP server")
 def test_sendmessage(self):

        s = smtplib.SMTP('localhost', 8026)

        s.sendmail("ta-meyer@ihug.co.nz", "ta-meyer@ihug.co.nz", good1)

        s.quit()
 def test_ham_intercept(self):

        pre_ham_trained = self.bayes.nham

        s = smtplib.SMTP('localhost', 8026)

        s.sendmail("ta-meyer@ihug.co.nz",
                   options["smtpproxy", "ham_address"], good1)

        s.quit()

        post_ham_trained = self.bayes.nham

        self.assertEqual(pre_ham_trained+1, post_ham_trained)

def suite():

    suite = unittest.TestSuite()

    suite.addTest(unittest.makeSuite(SMTPProxyTest))

    return suite
 def run():

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'ht')

    except getopt.error as msg:

        print(str(msg) + '\n\n' + __doc__, file=sys.stderr)

        sys.exit()

    for opt, arg in opts:

        if opt == '-h':

            print(__doc__, file=sys.stderr)

            sys.exit()

        elif opt == '-t':

            state.isTest = True

            state.runTestServer = True

    state.createWorkers()

    if state.runTestServer:

        print("Running a test SMTP server on port 8025...")

        TestListener()

        asyncore.loop()

    else:

        state.isTest = True

        state.buildServerStrings()

        testSocketMap = {}

        def runTestServer():

            TestListener(socketMap=testSocketMap)

            asyncore.loop(map=testSocketMap)

        def runProxy():

            trainer = SMTPTrainer(Classifier(), state)

            BayesSMTPProxyListener('localhost', 8025, ('', 8026), trainer)

            Dibbler.run()

        _thread.start_new_thread(runTestServer, ())

        _thread.start_new_thread(runProxy, ())

        sb_test_support.unittest_main(argv=sys.argv + ['suite'])
 if __name__ == '__main__':

    run()

 if __name__ == '__main__':

    run()





