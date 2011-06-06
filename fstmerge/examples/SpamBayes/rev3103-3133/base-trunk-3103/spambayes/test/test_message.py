import os
import sys
import math
import time
import email
import unittest
import sb_test_support
sb_test_support.fix_sys_path()
from spambayes.Options import options
from spambayes.tokenizer import tokenize
from spambayes.classifier import Classifier
from spambayes.message import MessageInfoDB, insert_exception_header
from spambayes.message import Message, SBHeaderMessage, MessageInfoPickle
from test_sb_server import good1, spam1, malformed1
try:
    __file__
except NameError:
    __file__ = sys.argv[0]
TEMP_PICKLE_NAME = os.path.join(os.path.dirname(__file__), "temp.pik")
TEMP_DBM_NAME = os.path.join(os.path.dirname(__file__), "temp.dbm")
for fn in [TEMP_PICKLE_NAME, TEMP_DBM_NAME]:
    if os.path.exists(fn):
        print fn, "already exists.  Please remove this file before " \
              "running these tests (a file by that name will be " \
              "created and destroyed as part of the tests)."
        sys.exit(1)
class MessageTest(unittest.TestCase):
    def setUp(self):
        self.msg = email.message_from_string(spam1, _class=Message)
    def test_persistent_state(self):
        self.assertEqual(self.msg.stored_attributes, ['c', 't',
                                                      'date_modified'])
    def test_initialisation(self):
        self.assertEqual(self.msg.id, None)
        self.assertEqual(self.msg.c, None)
        self.assertEqual(self.msg.t, None)
    def test_setId(self):
        self.msg.id = "test"
        self.assertRaises(ValueError, self.msg.setId, "test2")
        self.msg.id = None
        self.assertRaises(ValueError, self.msg.setId, None)
        self.assertRaises(TypeError, self.msg.setId, 1)
        self.assertRaises(TypeError, self.msg.setId, False)
        self.assertRaises(TypeError, self.msg.setId, [])
        id = "Test"
        self.msg.setId(id)
        self.assertEqual(self.msg.id, id)
        self.msg.id = None
        saved = self.msg.message_info_db.load_msg
        self.done = False
        try:
            self.msg.message_info_db.load_msg = self._fake_setState
            self.msg.setId(id)
            self.assertEqual(self.done, True)
        finally:
            self.msg.message_info_db.load_msg = saved
    def test_getId(self):
        self.assertEqual(self.msg.getId(), None)
        id = "test"
        self.msg.id = id
        self.assertEqual(self.msg.getId(), id)
    def test_tokenize(self):
        toks = self.msg.tokenize()
        self.assertEqual(tuple(tokenize(spam1)), tuple(toks))
    def test_force_CRLF(self):
        self.assert_('\r' not in good1)
        lines = self.msg._force_CRLF(good1).split('\n')
        for line in lines:
            if line:
                self.assert_(line.endswith('\r'))
    def test_as_string_endings(self):
        self.assert_('\r' not in spam1)
        lines = self.msg.as_string().split('\n')
        for line in lines:
            if line:
                self.assert_(line.endswith('\r'))
    def _fake_setState(self, state):
        self.done = True
    def test_modified(self):
        saved = self.msg.message_info_db.store_msg
        try:
            self.msg.message_info_db.store_msg = self._fake_setState
            self.done = False
            self.msg.modified()
            self.assertEqual(self.done, False)
            self.msg.id = "Test"
            self.msg.modified()
            self.assertEqual(self.done, True)
        finally:
            self.msg.message_info_db.store_msg = saved
    def test_GetClassification(self):
        self.msg.c = 's'
        self.assertEqual(self.msg.GetClassification(),
                         options['Headers','header_spam_string'])
        self.msg.c = 'h'
        self.assertEqual(self.msg.GetClassification(),
                         options['Headers','header_ham_string'])
        self.msg.c = 'u'
        self.assertEqual(self.msg.GetClassification(),
                         options['Headers','header_unsure_string'])
        self.msg.c = 'a'
        self.assertEqual(self.msg.GetClassification(), None)
    def test_RememberClassification(self):
        self.msg.RememberClassification(options['Headers',
                                                'header_spam_string'])
        self.assertEqual(self.msg.c, 's')
        self.msg.RememberClassification(options['Headers',
                                                'header_ham_string'])
        self.assertEqual(self.msg.c, 'h')
        self.msg.RememberClassification(options['Headers',
                                                'header_unsure_string'])
        self.assertEqual(self.msg.c, 'u')
        self.assertRaises(ValueError, self.msg.RememberClassification, "a")
        saved = self.msg.modified
        self.done = False
        try:
            self.msg.modified = self._fake_modified
            self.msg.RememberClassification(options['Headers',
                                                    'header_unsure_string'])
            self.assertEqual(self.done, True)
        finally:
            self.msg.modified = saved
    def _fake_modified(self):
        self.done = True
    def test_GetAndRememberTrained(self):
        t = "test"
        saved = self.msg.modified
        self.done = False
        try:
            self.msg.modified = self._fake_modified
            self.msg.RememberTrained(t)
            self.assertEqual(self.done, True)
        finally:
            self.msg.modified = saved
        self.assertEqual(self.msg.GetTrained(), t)
class SBHeaderMessageTest(unittest.TestCase):
    def setUp(self):
        self.msg = email.message_from_string(spam1, _class=SBHeaderMessage)
        c = Classifier()
        self.u_prob, clues = c.spamprob(tokenize(good1), True)
        c.learn(tokenize(good1), False)
        self.g_prob, clues = c.spamprob(tokenize(good1), True)
        c.unlearn(tokenize(good1), False)
        c.learn(tokenize(spam1), True)
        self.s_prob, self.clues = c.spamprob(tokenize(spam1), True)
        self.ham = options['Headers','header_ham_string']
        self.spam = options['Headers','header_spam_string']
        self.unsure = options['Headers','header_unsure_string']
        self.to = "tony.meyer@gmail.com;ta-meyer@ihug.co.nz"
        self.msg["to"] = self.to
    def test_setIdFromPayload(self):
        id = self.msg.setIdFromPayload()
        self.assertEqual(id, None)
        self.assertEqual(self.msg.id, None)
        msgid = "test"
        msg = "".join((options['Headers','mailid_header_name'], ": ",
                       msgid, "\r\n", good1))
        msg = email.message_from_string(msg, _class=SBHeaderMessage)
        id = msg.setIdFromPayload()
        self.assertEqual(id, msgid)
        self.assertEqual(msg.id, msgid)
    def test_disposition_header_ham(self):
        name = options['Headers','classification_header_name']
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.assertEqual(self.msg[name], self.ham)
        self.assertEqual(self.msg.GetClassification(), self.ham)
    def test_disposition_header_spam(self):
        name = options['Headers','classification_header_name']
        self.msg.addSBHeaders(self.s_prob, self.clues)
        self.assertEqual(self.msg[name], self.spam)
        self.assertEqual(self.msg.GetClassification(), self.spam)
    def test_disposition_header_unsure(self):
        name = options['Headers','classification_header_name']
        self.msg.addSBHeaders(self.u_prob, self.clues)
        self.assertEqual(self.msg[name], self.unsure)
        self.assertEqual(self.msg.GetClassification(), self.unsure)
    def test_score_header_off(self):
        options['Headers','include_score'] = False
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.assertEqual(self.msg[options['Headers', 'score_header_name']],
                         None)
    def test_score_header(self):
        options['Headers','include_score'] = True
        options["Headers", "header_score_digits"] = 21
        options["Headers", "header_score_logarithm"] = False
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.assertEqual(self.msg[options['Headers', 'score_header_name']],
                         "%.21f" % (self.g_prob,))
    def test_score_header_log(self):
        options['Headers','include_score'] = True
        options["Headers", "header_score_digits"] = 21
        options["Headers", "header_score_logarithm"] = True
        self.msg.addSBHeaders(self.s_prob, self.clues)
        self.assert_(self.msg[options['Headers', 'score_header_name']].\
                     startswith("%.21f" % (self.s_prob,)))
        self.assert_(self.msg[options['Headers', 'score_header_name']].\
                     endswith(" (%d)" % (-math.log10(1.0-self.s_prob),)))
    def test_thermostat_header_off(self):
        options['Headers','include_thermostat'] = False
        self.msg.addSBHeaders(self.u_prob, self.clues)
        self.assertEqual(self.msg[options['Headers',
                                          'thermostat_header_name']], None)
    def test_thermostat_header_unsure(self):
        options['Headers','include_thermostat'] = True
        self.msg.addSBHeaders(self.u_prob, self.clues)
        self.assertEqual(self.msg[options['Headers',
                                          'thermostat_header_name']],
                         "*****")
    def test_thermostat_header_spam(self):
        options['Headers','include_thermostat'] = True
        self.msg.addSBHeaders(self.s_prob, self.clues)
        self.assertEqual(self.msg[options['Headers',
                                          'thermostat_header_name']],
                         "*********")
    def test_thermostat_header_ham(self):
        options['Headers','include_thermostat'] = True
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.assertEqual(self.msg[options['Headers',
                                          'thermostat_header_name']], "")
    def test_evidence_header(self):
        options['Headers', 'include_evidence'] = True
        options['Headers', 'clue_mailheader_cutoff'] = 0.5 # all
        self.msg.addSBHeaders(self.g_prob, self.clues)
        header = self.msg[options['Headers', 'evidence_header_name']]
        header_clues = [s.split(':') for s in \
                        [s.strip() for s in header.split(';')]]
        header_clues = dict([(":".join(clue[:-1])[1:-1], float(clue[-1])) \
                             for clue in header_clues])
        for word, score in self.clues:
            self.assert_(word in header_clues)
            self.assertEqual(round(score, 2), header_clues[word])
    def test_evidence_header_partial(self):
        options['Headers', 'include_evidence'] = True
        options['Headers', 'clue_mailheader_cutoff'] = 0.1
        self.msg.addSBHeaders(self.g_prob, self.clues)
        header = self.msg[options['Headers', 'evidence_header_name']]
        header_clues = [s.split(':') for s in \
                        [s.strip() for s in header.split(';')]]
        header_clues = dict([(":".join(clue[:-1])[1:-1], float(clue[-1])) \
                             for clue in header_clues])
        for word, score in self.clues:
            if score <= 0.1 or score >= 0.9:
                self.assert_(word in header_clues)
                self.assertEqual(round(score, 2), header_clues[word])
            else:
                self.assert_(word not in header_clues)
    def test_evidence_header_empty(self):
        options['Headers', 'include_evidence'] = True
        options['Headers', 'clue_mailheader_cutoff'] = 0.0
        self.msg.addSBHeaders(self.g_prob, self.clues)
        header = self.msg[options['Headers','evidence_header_name']]
        header_clues = [s.split(':') for s in \
                        [s.strip() for s in header.split(';')]]
        header_clues = dict([(":".join(clue[:-1])[1:-1], float(clue[-1])) \
                             for clue in header_clues])
        for word, score in self.clues:
            if word == "*H*" or word == "*S*":
                self.assert_(word in header_clues)
                self.assertEqual(round(score, 2), header_clues[word])
            else:
                self.assert_(word not in header_clues)
    def test_evidence_header_off(self):
        options['Headers', 'include_evidence'] = False
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.assertEqual(self.msg[options['Headers',
                                          'evidence_header_name']], None)
    def test_notate_to_off(self):
        options["Headers", "notate_to"] = ()
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.msg.addSBHeaders(self.u_prob, self.clues)
        self.msg.addSBHeaders(self.s_prob, self.clues)
        self.assertEqual(self.msg["To"], self.to)
    def test_notate_to_ham(self):
        options["Headers", "notate_to"] = (self.ham,)
        self.msg.addSBHeaders(self.g_prob, self.clues)
        disp, orig = self.msg["To"].split(',', 1)
        self.assertEqual(orig, self.to)
        self.assertEqual(disp, "%s@spambayes.invalid" % (self.ham,))
    def test_notate_to_unsure(self):
        options["Headers", "notate_to"] = (self.ham, self.unsure)
        self.msg.addSBHeaders(self.u_prob, self.clues)
        disp, orig = self.msg["To"].split(',', 1)
        self.assertEqual(orig, self.to)
        self.assertEqual(disp, "%s@spambayes.invalid" % (self.unsure,))
    def test_notate_to_spam(self):
        options["Headers", "notate_to"] = (self.ham, self.spam, self.unsure)
        self.msg.addSBHeaders(self.s_prob, self.clues)
        disp, orig = self.msg["To"].split(',', 1)
        self.assertEqual(orig, self.to)
        self.assertEqual(disp, "%s@spambayes.invalid" % (self.spam,))
    def test_notate_subject_off(self):
        subject = self.msg["Subject"]
        options["Headers", "notate_subject"] = ()
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.msg.addSBHeaders(self.u_prob, self.clues)
        self.msg.addSBHeaders(self.s_prob, self.clues)
        self.assertEqual(self.msg["Subject"], subject)
    def test_notate_subject_ham(self):        
        subject = self.msg["Subject"]
        options["Headers", "notate_subject"] = (self.ham,)
        self.msg.addSBHeaders(self.g_prob, self.clues)
        disp, orig = self.msg["Subject"].split(',', 1)
        self.assertEqual(orig, subject)
        self.assertEqual(disp, self.ham)
    def test_notate_subject_unsure(self):
        subject = self.msg["Subject"]
        options["Headers", "notate_subject"] = (self.ham, self.unsure)
        self.msg.addSBHeaders(self.u_prob, self.clues)
        disp, orig = self.msg["Subject"].split(',', 1)
        self.assertEqual(orig, subject)
        self.assertEqual(disp, self.unsure)
    def test_notate_subject_spam(self):
        subject = self.msg["Subject"]
        options["Headers", "notate_subject"] = (self.ham, self.spam,
                                                self.unsure)
        self.msg.addSBHeaders(self.s_prob, self.clues)
        disp, orig = self.msg["Subject"].split(',', 1)
        self.assertEqual(orig, subject)
        self.assertEqual(disp, self.spam)
    def test_notate_to_changed(self):
        saved_ham = options["Headers", "header_ham_string"]
        notate_to = options.get_option("Headers", "notate_to")
        saved_to = notate_to.allowed_values
        try:
            options["Headers", "header_ham_string"] = "bacon"
            header_strings = (options["Headers", "header_ham_string"],
                              options["Headers", "header_spam_string"],
                              options["Headers", "header_unsure_string"])
            notate_to = options.get_option("Headers", "notate_to")
            notate_to.allowed_values = header_strings
            self.ham = options["Headers", "header_ham_string"]
            result = self.test_notate_to_ham()
            self.assertEqual(self.msg["To"].split(',', 1)[0],
                             "bacon@spambayes.invalid")
        finally:
            options["Headers", "header_ham_string"] = saved_ham
            self.ham = saved_ham
            notate_to.allowed_values = saved_to
        return result
    def test_id_header(self):
        options['Headers','add_unique_id'] = True
        id = "test"
        self.msg.id = id
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.assertEqual(self.msg[options['Headers',
                                          'mailid_header_name']], id)
    def test_id_header_off(self):
        options['Headers','add_unique_id'] = False
        id = "test"
        self.msg.id = id
        self.msg.addSBHeaders(self.g_prob, self.clues)
        self.assertEqual(self.msg[options['Headers',
                                          'mailid_header_name']], None)
    def test_currentSBHeaders(self):
        sbheaders = self.msg.currentSBHeaders()
        self.assertEqual({}, sbheaders)
        headers = {options['Headers', 'classification_header_name'] : '1',
                   options['Headers', 'mailid_header_name'] : '2',
                   options['Headers',
                           'classification_header_name'] + "-ID" : '3',
                   options['Headers', 'thermostat_header_name'] : '4',
                   options['Headers', 'evidence_header_name'] : '5',
                   options['Headers', 'score_header_name'] : '6',
                   options['Headers', 'trained_header_name'] : '7',
                   }
        for name, val in headers.items():
            self.msg[name] = val
        sbheaders = self.msg.currentSBHeaders()
        self.assertEqual(headers, sbheaders)
    def test_delSBHeaders(self):
        headers = (options['Headers', 'classification_header_name'],
                   options['Headers', 'mailid_header_name'],
                   options['Headers',
                           'classification_header_name'] + "-ID",
                   options['Headers', 'thermostat_header_name'],
                   options['Headers', 'evidence_header_name'],
                   options['Headers', 'score_header_name'],
                   options['Headers', 'trained_header_name'],)
        for header in headers:
            self.msg[header] = "test"
        for header in headers:
            self.assert_(header in self.msg.keys())
        self.msg.delSBHeaders()
        for header in headers:
            self.assert_(header not in self.msg.keys())
    def test_delNotations(self):
        for headername in ["subject", "to"]:
            for disp in (self.ham, self.spam, self.unsure):
                header = self.msg[headername]
                self.assertEqual(header.find(disp), -1)
                options["Headers", "notate_%s" % (headername,)] = \
                                   (self.ham, self.unsure, self.spam)
                prob = {self.ham:self.g_prob, self.spam:self.s_prob,
                        self.unsure:self.u_prob}[disp]
                self.msg.addSBHeaders(prob, self.clues)
                self.assertNotEqual(self.msg[headername].find(disp), -1)
                self.msg.delNotations()
                self.assertEqual(self.msg[headername], header)
    def test_delNotations_missing(self):
        for headername in ["subject", "to"]:
            for disp in (self.ham, self.spam, self.unsure):
                header = self.msg[headername]
                self.assertEqual(header.find(disp), -1)
                options["Headers", "notate_%s" % (headername,)] = ()
                prob = {self.ham:self.g_prob, self.spam:self.s_prob,
                        self.unsure:self.u_prob}[disp]
                self.msg.addSBHeaders(prob, self.clues)
                self.assertEqual(self.msg[headername].find(disp), -1)
                self.msg.delNotations()
                self.assertEqual(self.msg[headername], header)
    def test_delNotations_no_header(self):
        for headername in ["subject", "to"]:
            for disp in (self.ham, self.spam, self.unsure):
                del self.msg[headername]
                options["Headers", "notate_%s" % (headername,)] = \
                                   (self.ham, self.unsure, self.spam)
                self.msg.delNotations()
                self.assertEqual(self.msg[headername], None)
    def test_delNotations_only_once_subject(self):
        self._test_delNotations_only_once("subject")
    def test_delNotations_only_once_to(self):
        self._test_delNotations_only_once("to")
    def _test_delNotations_only_once(self, headername):
        for disp in (self.ham, self.spam, self.unsure):
            header = self.msg[headername]
            self.assertEqual(header.find(disp), -1)
            options["Headers", "notate_%s" % (headername,)] = \
                               (self.ham, self.unsure, self.spam)
            prob = {self.ham:self.g_prob, self.spam:self.s_prob,
                    self.unsure:self.u_prob}[disp]
            self.msg.addSBHeaders(prob, self.clues)
            self.assertNotEqual(self.msg[headername].find(disp), -1)
            header2 = self.msg[headername]
            self.msg.addSBHeaders(prob, self.clues)
            self.assertNotEqual(self.msg[headername].\
                                replace(disp, "", 1).find(disp), -1)
            self.msg.delNotations()
            self.assertEqual(self.msg[headername], header2)
            self.msg.replace_header(headername, header)
class MessageInfoBaseTest(unittest.TestCase):
    def setUp(self, fn=TEMP_PICKLE_NAME):
        self.db = self.klass(fn, self.mode)
    def test_mode(self):
        self.assertEqual(self.mode, self.db.mode)
    def test_load_msg_missing(self):
        msg = email.message_from_string(good1, _class=Message)
        msg.id = "Test"
        dummy_values = "a", "b"
        msg.c, msg.t = dummy_values
        self.db.load_msg(msg)
        self.assertEqual((msg.c, msg.t), dummy_values)
    def test_load_msg_compat(self):
        msg = email.message_from_string(good1, _class=Message)
        msg.id = "Test"
        dummy_values = "a", "b"
        self.db.db[msg.id] = dummy_values
        self.db.load_msg(msg)
        self.assertEqual((msg.c, msg.t), dummy_values)
    def test_load_msg(self):
        msg = email.message_from_string(good1, _class=Message)
        msg.id = "Test"
        dummy_values = [('a', 1), ('b', 2)]
        self.db.db[msg.id] = dummy_values
        self.db.load_msg(msg)
        for att, val in dummy_values:
            self.assertEqual(getattr(msg, att), val)
    def test_store_msg(self):
        msg = email.message_from_string(good1, _class=Message)
        msg.id = "Test"
        saved = self.db.store
        self.done = False
        try:
            self.db.store = self._fake_store
            self.db.store_msg(msg)
        finally:
            self.db.store = saved
        self.assertEqual(self.done, True)
        correct = [(att, getattr(msg, att)) \
                   for att in msg.stored_attributes]
        db_version = dict(self.db.db[msg.id])
        correct_version = dict(correct)
        correct_version["date_modified"], time.time()
        self.assertEqual(db_version, correct_version)
    def _fake_store(self):
        self.done = True
    def test_remove_msg(self):
        msg = email.message_from_string(good1, _class=Message)
        msg.id = "Test"
        self.db.db[msg.id] = "test"
        saved = self.db.store
        self.done = False
        try:
            self.db.store = self._fake_store
            self.db.remove_msg(msg)
        finally:
            self.db.store = saved
        self.assertEqual(self.done, True)
        self.assertRaises(KeyError, self.db.db.__getitem__, msg.id)
    def test_load(self):
        data = {"1" : ('a', 'b', 'c'),
                "2" : ('d', 'e', 'f'),
                "3" : "test"}
        for k, v in data.items():
            self.db.db[k] = v
        self.db.store()
        fn = self.db.db_name
        self.db.close()
        db2 = self.klass(fn, self.mode)
        try:
            self.assertEqual(len(db2.db.keys()), len(data.keys()))
            for k, v in data.items():
                self.assertEqual(db2.db[k], v)
        finally:
            db2.close()
    def test_load_new(self):
        self.assertEqual(self.db.db.keys(), [])
class MessageInfoPickleTest(MessageInfoBaseTest):
    def setUp(self):
        self.mode = 1
        self.klass = MessageInfoPickle
        MessageInfoBaseTest.setUp(self, TEMP_PICKLE_NAME)
    def tearDown(self):
        try:
            os.remove(TEMP_PICKLE_NAME)
        except OSError:
            pass
    def store(self):
        if self.db is not None:
            self.db.sync()
class MessageInfoDBTest(MessageInfoBaseTest):
    def setUp(self):
        self.mode = 'c'
        self.klass = MessageInfoDB
        MessageInfoBaseTest.setUp(self, TEMP_DBM_NAME)
    def tearDown(self):
        self.db.close()
        try:
            os.remove(TEMP_DBM_NAME)
        except OSError:
            pass
    def store(self):
        if self.db is not None:
            self.db.sync()
    def _fake_close(self):
        self.done += 1
    def test_close(self):
        saved_db = self.db.db.close
        saved_dbm = self.db.dbm.close
        try:
            self.done = 0
            self.db.db.close = self._fake_close
            self.db.dbm.close = self._fake_close
            self.db.close()
            self.assertEqual(self.done, 2)
        finally:
            self.db.db.close = saved_db
            self.db.dbm.close = saved_dbm
class UtilitiesTest(unittest.TestCase):
    def _verify_details(self, details):
        loc = details.find(__file__)
        self.assertNotEqual(loc, -1)
        loc = details.find("Exception: Test")
        self.assertNotEqual(loc, -1)
    def _verify_exception_header(self, msg, details):        
        msg = email.message_from_string(msg)
        details = "\r\n.".join(details.strip().split('\n'))
        headerName = 'X-Spambayes-Exception'
        header = email.Header.Header(details, header_name=headerName)
        self.assertEqual(msg[headerName].replace('\r\n', '\n'),
                         str(header).replace('\r\n', '\n'))
    def test_insert_exception_header(self):
        try:
            raise Exception("Test")
        except Exception:
            pass
        msg, details = insert_exception_header(good1)
        self._verify_details(details)
        self._verify_exception_header(msg, details)
    def test_insert_exception_header_and_id(self):
        try:
            raise Exception("Test")
        except Exception:
            pass
        id = "Message ID"
        msg, details = insert_exception_header(good1, id)
        self._verify_details(details)
        self._verify_exception_header(msg, details)
        msg = email.message_from_string(msg)
        headerName = options["Headers", "mailid_header_name"]
        header = email.Header.Header(id, header_name=headerName)
        self.assertEqual(msg[headerName], str(header).replace('\n', '\r\n'))
    def test_insert_exception_header_no_separator(self):
        try:
            raise Exception("Test")
        except Exception:
            pass
        msg, details = insert_exception_header(malformed1)
        self._verify_details(details)
        self._verify_exception_header(msg, details)
def suite():
    suite = unittest.TestSuite()
    classes = (MessageTest,
               SBHeaderMessageTest,
               MessageInfoPickleTest,
               UtilitiesTest,
               )
    from spambayes import dbmstorage
    try:
        dbmstorage.open_best()
    except dbmstorage.error:
        print "Skipping MessageInfoDBTest - no dbm module available"
        from spambayes import message
        def always_pickle():
            return "__test.pik", "pickle"
        message.database_type = always_pickle
    except TypeError:
        classes += (MessageInfoDBTest,)
    for cls in classes:
        suite.addTest(unittest.makeSuite(cls))
    return suite
if __name__=='__main__':
    sb_test_support.unittest_main(argv=sys.argv + ['suite'])
