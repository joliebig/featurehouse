import sys
import email
import time
import thread
import imaplib
import unittest
import sb_test_support
sb_test_support.fix_sys_path()
from spambayes import Dibbler
from spambayes.Options import options
from spambayes.classifier import Classifier
from spambayes.message import SBHeaderMessage
from sb_pop3dnd import IMAPMessage, DynamicIMAPMessage, IMAPFileMessage
from sb_pop3dnd import IMAPFileMessageFactory
from test_sb_server import TestListener, good1, spam1
POP_PORT = 8110
class IMAPMessageTest(unittest.TestCase):
    def testIMAPMessage(self):
        msg = IMAPMessage()
        self.assertEqual(msg.date, None)
        msg = IMAPMessage("fake date")
        self.assertEqual(msg.date, "fake date")
        for att in ["date", "deleted", "flagged", "seen", "draft",
                    "recent", "answered"]:
            self.assert_(att in msg.stored_attributes)
        for flag in ["deleted", "answered", "flagged", "seen", "draft",
                     "recent"]:
            self.assertEqual(getattr(msg, flag), False)
    def testGetAllHeaders(self):
        msg = email.message_from_string(good1, _class=IMAPMessage)
        correct_msg = email.message_from_string(good1)
        headers = msg.getHeaders(False)
        for k, v in correct_msg.items():
            self.assertEqual(headers[k.lower()], v)
        headers = msg.getHeaders(True)
        for k, v in correct_msg.items():
            self.assertEqual(headers[k.lower()], v)
    def testGetIndividualHeaders(self):
        msg = email.message_from_string(good1, _class=IMAPMessage)
        correct_msg = email.message_from_string(good1)
        headers = msg.getHeaders(False, "SUBJECT")
        self.assertEqual(headers["subject"], correct_msg["Subject"])
        headers = msg.getHeaders(True, "SUBJECT")
        self.assert_("subject" not in headers)
        for k, v in correct_msg.items():
            if k == "Subject":
                continue
            self.assertEqual(headers[k.lower()], v)
    def testGetFlags(self):
        msg = IMAPMessage()
        all_flags = ["deleted", "answered", "flagged", "seen", "draft",
                     "recent"]
        for flag in all_flags:
            setattr(msg, flag, True)
        flags = list(msg.getFlags())
        for flag in all_flags:
            self.assert_("\\%s" % (flag.upper(),) in flags)
        for flag in all_flags:
            setattr(msg, flag, False)
        flags = list(msg.getFlags())
        self.assertEqual(flags, [])
    def testGetInternalDate(self):
        msg = IMAPMessage()
        self.assertRaises(AssertionError, msg.getInternalDate)
        msg = IMAPMessage("fake date")
        self.assertEqual(msg.getInternalDate(), "fake date")
    def testGetBodyFile(self):
        msg = email.message_from_string(spam1, _class=IMAPMessage)
        correct_msg = email.message_from_string(spam1)
        body = msg.getBodyFile()
        self.assertEqual(body.read().replace('\r\n', '\n'),
                         correct_msg.get_payload())
    def testGetSize(self):
        msg = email.message_from_string(spam1, _class=IMAPMessage)
        correct_msg = email.message_from_string(spam1)
        self.assertEqual(msg.getSize(),
                         len(correct_msg.as_string().replace('\n', '\r\n')))
    def testGetUID(self):
        msg = IMAPMessage()
        msg.id = "fake id" # Heh
        self.assertEqual(msg.getUID(), "fake id")
    def testIsMultipart(self):
        msg = IMAPMessage()
        self.assertEqual(msg.isMultipart(), False)
    def testGetSubPart(self):
        msg = IMAPMessage()
        self.assertRaises(NotImplementedError, msg.getSubPart, None)
    def testClearFlags(self):
        msg = IMAPMessage()
        all_flags = ["deleted", "answered", "flagged", "seen", "draft",
                     "recent"]
        for flag in all_flags:
            setattr(msg, flag, True)
        msg.clear_flags()
        for flag in all_flags:
            self.assertEqual(getattr(msg, flag), False)
    def testFlags(self):
        msg = IMAPMessage()
        all_flags = ["deleted", "answered", "flagged", "seen", "draft",
                     "recent"]
        for flag in all_flags:
            setattr(msg, flag, True)
        flags = list(msg.flags())
        for flag in all_flags:
            self.assert_("\\%s" % (flag.upper(),) in flags)
        for flag in all_flags:
            setattr(msg, flag, False)
        flags = list(msg.flags())
        self.assertEqual(flags, [])
    def testTrain(self):
        pass
    def testStructure(self):
        pass
    def testBody(self):
        msg = email.message_from_string(good1, _class=IMAPMessage)
        correct_msg = email.message_from_string(good1)
        body = msg.body()
        self.assertEqual(body.replace('\r\n', '\n'),
                         correct_msg.get_payload())
    def testHeaders(self):
        msg = email.message_from_string(good1, _class=IMAPMessage)
        correct_msg = email.message_from_string(good1)
        headers = msg.headers()
        correct_headers = "\r\b".join(["%s: %s" % (k, v) \
                                       for k, v in correct_msg.items()])
class DynamicIMAPMessageTest(unittest.TestCase):
    def setUp(self):
        def fakemsg(body=False, headers=False):
            msg = []
            if headers:
                msg.append("Header: Fake")
                if body:
                    msg.append("\r\n")
            if body:
                msg.append("Fake Body")
            return "\r\n".join(msg)
        self.msg = DynamicIMAPMessage(fakemsg)
    def testDate(self):
        date = imaplib.Time2Internaldate(time.time())[1:-1]
        self.assertEqual(self.msg.date, date)
    def testLoad(self):
        self.assertEqual(self.msg.as_string(),
                         "Header: Fake\r\n\r\nFake Body")
class IMAPFileMessageTest(unittest.TestCase):
    def setUp(self):
        self.msg = IMAPFileMessage("filename", "directory")
    def testID(self):
        self.assertEqual(self.msg.id, "filename")
    def testDate(self):
        date = imaplib.Time2Internaldate(time.time())[1:-1]
        self.assertEqual(self.msg.date, date)
class IMAPFileMessageFactoryTest(unittest.TestCase):
    def testCreateNoContent(self):
        factory = IMAPFileMessageFactory()
        msg = factory.create("key", "directory")
        self.assertEqual(msg.id, key)
        self.assert_(isinstance(msg, type(IMAPFileMessage())))
def suite():
    suite = unittest.TestSuite()
    for cls in (IMAPMessageTest,
                DynamicIMAPMessageTest,
                IMAPFileMessageTest,
               ):
        suite.addTest(unittest.makeSuite(cls))
    return suite
if __name__=='__main__':
    def runTestServer():
        import asyncore
        asyncore.loop()
    TestListener()
    thread.start_new_thread(runTestServer, ())
    sb_test_support.unittest_main(argv=sys.argv + ['suite'])
