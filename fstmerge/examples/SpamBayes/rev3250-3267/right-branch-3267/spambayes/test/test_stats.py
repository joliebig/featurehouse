import os
import sys
import time
import unittest
import sb_test_support
sb_test_support.fix_sys_path()
from spambayes.Stats import Stats
from spambayes.Options import options
from spambayes.message import MessageInfoPickle, Message
class StatsTest(unittest.TestCase):
    def setUp(self):
        self.messageinfo_db_name = "__unittest.pik"
        self.messageinfo_db = MessageInfoPickle(self.messageinfo_db_name)
        self.s = Stats(options, self.messageinfo_db)
        Message.message_info_db = self.messageinfo_db
    def tearDown(self):
        if os.path.exists(self.messageinfo_db_name):
            os.remove(self.messageinfo_db_name)
    def test_from_date_unset(self):
        self.assertEqual(None, self.s.from_date)
    def test_set_date(self):
        now = time.time()
        self.s.ResetTotal(permanently=True)
        self.assertEqual(now, self.s.from_date)
        for stat in ["num_ham", "num_spam", "num_unsure",
                     "num_trained_spam", "num_trained_spam_fn",
                     "num_trained_ham", "num_trained_ham_fp",]:
            self.assertEqual(self.s.totals[stat], 0)
        self.messageinfo_db.close()
        self.messageinfo_db = MessageInfoPickle(self.messageinfo_db_name)
        self.s = Stats(options, self.messageinfo_db)
        self.assertEqual(now, self.s.from_date)
    def test_no_messages(self):
        self.assertEqual(self.s.GetStats(), ["Messages classified: 0"])
    def test_reset_session(self):
        self.s.RecordClassification(.2)
        self.s.RecordClassification(.1)
        self.s.RecordClassification(.4)
        self.s.RecordClassification(.91)
        self.s.RecordTraining(True, 0.1)
        self.s.RecordTraining(True, 0.91)
        self.s.RecordTraining(False, 0.1)
        self.s.RecordTraining(False, 0.91)
        self.assertNotEqual(self.s.num_ham, 0)
        self.assertNotEqual(self.s.num_spam, 0)
        self.assertNotEqual(self.s.num_unsure, 0)
        self.assertNotEqual(self.s.num_trained_spam, 0)
        self.assertNotEqual(self.s.num_trained_spam_fn, 0)
        self.assertNotEqual(self.s.num_trained_ham, 0)
        self.assertNotEqual(self.s.num_trained_ham_fp, 0)
        self.s.Reset()
        self.assertEqual(self.s.num_ham, 0)
        self.assertEqual(self.s.num_spam, 0)
        self.assertEqual(self.s.num_unsure, 0)
        self.assertEqual(self.s.num_trained_spam, 0)
        self.assertEqual(self.s.num_trained_spam_fn, 0)
        self.assertEqual(self.s.num_trained_ham, 0)
        self.assertEqual(self.s.num_trained_ham_fp, 0)
    def test_record_ham(self):
        self.s.RecordClassification(0.0)
        self.assertEqual(self.s.num_ham, 1)
        self.s.RecordClassification(0.0)
        self.assertEqual(self.s.num_ham, 2)
    def test_record_spam(self):
        self.s.RecordClassification(1.0)
        self.assertEqual(self.s.num_spam, 1)
        self.s.RecordClassification(1.0)
        self.assertEqual(self.s.num_spam, 2)
    def test_record_unsure(self):
        self.s.RecordClassification(0.5)
        self.assertEqual(self.s.num_unsure, 1)
        self.s.RecordClassification(0.5)
        self.assertEqual(self.s.num_unsure, 2)
    def test_record_fp(self):
        self.s.RecordTraining(True, 1.0)
        self.assertEqual(self.s.num_trained_ham, 1)
        self.assertEqual(self.s.num_trained_ham_fp, 1)
    def test_record_fn(self):
        self.s.RecordTraining(False, 0.0)
        self.assertEqual(self.s.num_trained_spam, 1)
        self.assertEqual(self.s.num_trained_spam_fn, 1)
    def test_record_fp_class(self):
        self.s.RecordTraining(True,
                              old_class=options["Headers",
                                                "header_spam_string"])
        self.assertEqual(self.s.num_trained_ham, 1)
        self.assertEqual(self.s.num_trained_ham_fp, 1)
    def test_record_fn_class(self):
        self.s.RecordTraining(False,
                              old_class=options["Headers",
                                                "header_ham_string"])
        self.assertEqual(self.s.num_trained_spam, 1)
        self.assertEqual(self.s.num_trained_spam_fn, 1)
    def test_no_record_fp(self):
        self.s.RecordTraining(True)
        self.assertEqual(self.s.num_trained_ham, 1)
        self.assertEqual(self.s.num_trained_ham_fp, 0)
    def test_no_record_fn(self):
        self.s.RecordTraining(False)
        self.assertEqual(self.s.num_trained_spam, 1)
        self.assertEqual(self.s.num_trained_spam_fn, 0)
    def test_record_train_spam(self):
        self.s.RecordTraining(False, 1.0)
        self.assertEqual(self.s.num_trained_spam, 1)
        self.assertEqual(self.s.num_trained_spam_fn, 0)
    def test_record_train_ham(self):
        self.s.RecordTraining(True, 0.0)
        self.assertEqual(self.s.num_trained_ham, 1)
        self.assertEqual(self.s.num_trained_ham_fp, 0)
    def test_calculate_persistent_stats(self):
        for stat in ["num_ham", "num_spam", "num_unsure",
                     "num_trained_spam", "num_trained_spam_fn",
                     "num_trained_ham", "num_trained_ham_fp",]:
            self.assertEqual(self.s.totals[stat], 0)
        msg = Message('0')
        msg.RememberTrained(True)
        msg.RememberClassification(options['Headers','header_spam_string'])
        msg = Message('1')
        msg.RememberTrained(False)
        msg.RememberClassification(options['Headers','header_spam_string'])
        msg = Message('2')
        msg.RememberTrained(True)
        msg.RememberClassification(options['Headers','header_ham_string'])
        msg = Message('3')
        msg.RememberTrained(False)
        msg.RememberClassification(options['Headers','header_ham_string'])
        msg = Message('4')
        msg.RememberClassification(options['Headers','header_ham_string'])
        msg = Message('5')
        msg.RememberTrained(False)
        msg.RememberClassification(options['Headers','header_unsure_string'])
        msg = Message('6')
        msg.RememberTrained(True)
        msg.RememberClassification(options['Headers','header_unsure_string'])
        msg = Message('7')
        msg.RememberClassification(options['Headers','header_unsure_string'])
        msg = Message('8')
        msg.RememberClassification(options['Headers','header_unsure_string'])
        self.s.CalculatePersistentStats()
        self.assertEqual(self.s.totals["num_ham"], 3)
        self.assertEqual(self.s.totals["num_spam"], 2)
        self.assertEqual(self.s.totals["num_unsure"], 4)
        self.assertEqual(self.s.totals["num_trained_spam"], 1)
        self.assertEqual(self.s.totals["num_trained_spam_fn"], 1)
        self.assertEqual(self.s.totals["num_trained_ham"], 1)
        self.assertEqual(self.s.totals["num_trained_ham_fp"], 1)
    def test_CalculateAdditional(self):
        data = {}
        data["num_seen"] = 45
        data["num_ham"] = 23
        data["num_spam"] = 10
        data["num_unsure"] = 12
        data["num_trained_spam_fn"] = 4
        data["num_trained_ham_fp"] = 3
        data["num_trained_ham"] = 7
        data["num_trained_spam"] = 5
        data["num_unsure_trained_ham"] = 2
        data["num_unsure_trained_spam"] = 1
        new_data = self.s._CalculateAdditional(data)
        self.assertEqual(new_data["perc_ham"], 100.0 * data["num_ham"] /
                         data["num_seen"])
        self.assertEqual(new_data["perc_spam"], 100.0 * data["num_spam"] /
                         data["num_seen"])
        self.assertEqual(new_data["perc_unsure"], 100.0 *
                         data["num_unsure"] / data["num_seen"])
        self.assertEqual(new_data["num_ham_correct"], data["num_ham"] -
                         data["num_trained_spam_fn"])
        self.assertEqual(new_data["num_spam_correct"], data["num_spam"] -
                         data["num_trained_ham_fp"])
        self.assertEqual(new_data["num_correct"],
                         new_data["num_ham_correct"] +
                         new_data["num_spam_correct"])
        self.assertEqual(new_data["num_incorrect"],
                         data["num_trained_spam_fn"] +
                         data["num_trained_ham_fp"])
        self.assertEqual(new_data["perc_correct"], 100.0 *
                         new_data["num_correct"] / data["num_seen"])
        self.assertEqual(new_data["perc_incorrect"], 100.0 *
                         new_data["num_incorrect"] / data["num_seen"])
        self.assertEqual(new_data["perc_fp"], 100.0 *
                         data["num_trained_ham_fp"] / data["num_seen"])
        self.assertEqual(new_data["perc_fn"], 100.0 *
                         data["num_trained_spam_fn"] / data["num_seen"])
        self.assertEqual(new_data["num_unsure_trained_ham"],
                         data["num_trained_ham"] -
                         data["num_trained_ham_fp"])
        self.assertEqual(new_data["num_unsure_trained_spam"],
                         data["num_trained_spam"] -
                         data["num_trained_spam_fn"])
        self.assertEqual(new_data["num_unsure_not_trained"],
                         data["num_unsure"] -
                         data["num_unsure_trained_ham"] -
                         data["num_unsure_trained_spam"])
        self.assertEqual(new_data["perc_unsure_trained_ham"], 100.0 *
                         data["num_unsure_trained_ham"] /
                         data["num_unsure"])
        self.assertEqual(new_data["perc_unsure_trained_spam"], 100.0 *
                         data["num_unsure_trained_spam"] /
                         data["num_unsure"])
        self.assertEqual(new_data["perc_unsure_not_trained"], 100.0 *
                         new_data["num_unsure_not_trained"] /
                         data["num_unsure"])
        self.assertEqual(new_data["total_ham"],
                         new_data["num_ham_correct"] +
                         data["num_trained_ham"])
        self.assertEqual(new_data["total_spam"],
                         new_data["num_spam_correct"] +
                         data["num_trained_spam"])
        self.assertEqual(new_data["perc_ham_incorrect"], 100.0 *
                         data["num_trained_ham_fp"] /
                         data["total_ham"])
        self.assertEqual(new_data["perc_ham_unsure"], 100.0 *
                         data["num_unsure_trained_ham"] /
                         data["total_ham"])
        self.assertEqual(new_data["perc_ham_incorrect_or_unsure"], 100.0 *
                         (data["num_trained_ham_fp"] +
                          data["num_unsure_trained_ham"]) /
                         data["total_ham"])
        self.assertEqual(new_data["perc_spam_correct"], 100.0 *
                         data["num_spam_correct"] /
                         data["total_spam"])
        self.assertEqual(new_data["perc_spam_unsure"], 100.0 *
                         data["num_unsure_trained_spam"] /
                         data["total_spam"])
        self.assertEqual(new_data["perc_spam_correct_or_unsure"], 100.0 *
                         (data["num_spam_correct"] +
                          data["num_unsure_trained_spam"]) /
                         data["total_spam"])
        self.assertEqual(new_data["total_cost"],
                         data["num_trained_ham_fp"] *
                         options["TestDriver", "best_cutoff_fp_weight"] + \
                         data["num_trained_spam_fn"] *
                         options["TestDriver", "best_cutoff_fn_weight"] + \
                         data["num_unsure"] *
                         options["TestDriver", "best_cutoff_unsure_weight"])
        self.assertEqual(new_data["cost_savings"], data["num_spam"] *
                         options["TestDriver", "best_cutoff_fn_weight"] -
                         data["total_cost"])
    def test_AddPercentStrings(self):
        for i in range(10):
            self._test_AddPercentStrings(i)
    def _test_AddPercentStrings(self, dp):
        data = self.s._AddPercentStrings({}, dp)
        self.assertEqual(data["perc_ham_s"],
                         "%%(perc_ham).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_spam_s"],
                         "%%(perc_spam).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_unsure_s"],
                         "%%(perc_unsure).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_correct_s"],
                         "%%(perc_correct).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_incorrect_s"],
                         "%%(perc_incorrect).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_fp_s"],
                         "%%(perc_fp).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_fn_s"],
                         "%%(perc_fn).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_spam_correct_s"],
                         "%%(perc_spam_correct).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_spam_unsure_s"],
                         "%%(perc_spam_unsure).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_spam_correct_or_unsure_s"],
                         "%%(perc_spam_correct_or_unsure).%df%%(perc)s" %
                         (dp,))
        self.assertEqual(data["perc_ham_incorrect_s"],
                         "%%(perc_ham_incorrect).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_ham_unsure_s"],
                         "%%(perc_ham_unsure).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_ham_incorrect_or_unsure_s"],
                         "%%(perc_ham_incorrect_or_unsure).%df%%(perc)s" %
                         (dp,))
        self.assertEqual(data["perc_unsure_trained_ham_s"],
                         "%%(perc_unsure_trained_ham).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc_unsure_trained_spam_s"],
                         "%%(perc_unsure_trained_spam).%df%%(perc)s" %
                         (dp,))
        self.assertEqual(data["perc_unsure_not_trained_s"],
                         "%%(perc_unsure_not_trained).%df%%(perc)s" % (dp,))
        self.assertEqual(data["perc"], "%")
    def _test_no_recovery(self, score, one, two, three, five):
        self.s.RecordClassification(score)
        s = self.s.GetStats()
        self.assertEqual(s[0], "Messages classified: 1")
        self.assertEqual(s[1], one)
        self.assertEqual(s[2], two)
        self.assertEqual(s[3], three)
        self.assertEqual(s[4], "")
        self.assertEqual(s[5], five)
        self.assertEqual(s[6], "Classified incorrectly:\t0 (0.0% of total)")
        self.assertEqual(s[7], "")
        self.assertEqual(s[8], "Manually classified as good:\t0")
        self.assertEqual(s[9], "Manually classified as spam:\t0")
        self.assertEqual(s[10], "")
        if options["Categorization", "ham_cutoff"] <= score < \
           options["Categorization", "spam_cutoff"]:
            self.assertEqual(s[11], "Unsures trained as good:\t0 (0.0% of unsures)")
            self.assertEqual(s[12], "Unsures trained as spam:\t0 (0.0% of unsures)")
            self.assertEqual(s[13], "Unsures not trained:\t\t1 (100.0% of unsures)")
            self.assertEqual(s[14], "")
            counter = 15
        else:
            counter = 11
        try:
            return s[counter]
        except IndexError:
            return
    def test_no_recovery_unsure(self):
        score = 0.2
        one = "\tGood:\t0 (0.0%)"
        two = "\tSpam:\t0 (0.0%)"
        three = "\tUnsure:\t1 (100.0%)"
        five = "Classified correctly:\t0 (0.0% of total)"
        self._test_no_recovery(score, one, two, three, five)
    def test_no_recovery_ham(self):
        score = 0.0
        one = "\tGood:\t1 (100.0%)"
        two = "\tSpam:\t0 (0.0%)"
        three = "\tUnsure:\t0 (0.0%)"
        five = "Classified correctly:\t1 (100.0% of total)"
        final = self._test_no_recovery(score, one, two, three, five)
        self.assertEqual(final, "Good incorrectly identified:\t0.0% (+ 0.0% unsure)")
    def test_no_recovery_spam(self):
        score = 1.0
        one = "\tGood:\t0 (0.0%)"
        two = "\tSpam:\t1 (100.0%)"
        three = "\tUnsure:\t0 (0.0%)"
        five = "Classified correctly:\t1 (100.0% of total)"
        final = self._test_no_recovery(score, one, two, three, five)
        self.assertEqual(final, "Spam correctly identified:\t100.0% (+ 0.0% unsure)")
    def test_get_stats_session_only(self):
        self.s.RecordClassification(0.0)
        self.s.RecordClassification(0.2)
        self.s.RecordClassification(0.1)
        self.s.RecordClassification(0.4)
        self.s.RecordClassification(0.5)
        self.s.RecordClassification(0.95)
        self.s.RecordClassification(1.0)
        self.s.RecordTraining(True, 0.1)
        self.s.RecordTraining(True, 1.0)
        self.s.RecordTraining(False, 0.1)
        self.s.RecordTraining(False, 1.0)
        for stat in ["num_ham", "num_spam", "num_unsure",
                     "num_trained_spam", "num_trained_spam_fn",
                     "num_trained_ham", "num_trained_ham_fp",]:
            self.s.totals[stat] = 4
        s = self.s.GetStats(session_only=True)
        self.assertEqual(s[0], "Messages classified: 7")
        self.assertEqual(s[1], "\tGood:\t2 (28.6%)")
        self.assertEqual(s[2], "\tSpam:\t2 (28.6%)")
        self.assertEqual(s[3], "\tUnsure:\t3 (42.9%)")
        self.assertEqual(s[4], "")
        self.assertEqual(s[5], "Classified correctly:\t2 (28.6% of total)")
        self.assertEqual(s[6], "Classified incorrectly:\t2 (28.6% of total)")
        self.assertEqual(s[7], "\tFalse positives:\t1 (14.3% of total)")
        self.assertEqual(s[8], "\tFalse negatives:\t1 (14.3% of total)")
        self.assertEqual(s[9], "")
        self.assertEqual(s[10], "Manually classified as good:\t2")
        self.assertEqual(s[11], "Manually classified as spam:\t2")
        self.assertEqual(s[12], "")
        self.assertEqual(s[13], "Unsures trained as good:\t1 (33.3% of unsures)")
        self.assertEqual(s[14], "Unsures trained as spam:\t1 (33.3% of unsures)")
        self.assertEqual(s[15], "Unsures not trained:\t\t1 (33.3% of unsures)")
        self.assertEqual(s[16], "")
        self.assertEqual(s[17], "Spam correctly identified:\t33.3% (+ 33.3% unsure)")
        self.assertEqual(s[18], "Good incorrectly identified:\t33.3% (+ 33.3% unsure)")
        self.assertEqual(s[19], "")
        self.assertEqual(s[20], "Total cost of spam:\t$11.60")
        self.assertEqual(s[21], "SpamBayes savings:\t$-9.60")
        self.assertEqual(len(s), 22)
    def test_get_all_stats(self):
        s = self._stuff_with_data()
        self.assertEqual(s[0], "Messages classified: 16")
        self.assertEqual(s[1], "\tGood:\t5 (31.3%)")
        self.assertEqual(s[2], "\tSpam:\t4 (25.0%)")
        self.assertEqual(s[3], "\tUnsure:\t7 (43.8%)")
        self.assertEqual(s[4], "")
        self.assertEqual(s[5], "Classified correctly:\t5 (31.3% of total)")
        self.assertEqual(s[6], "Classified incorrectly:\t4 (25.0% of total)")
        self.assertEqual(s[7], "\tFalse positives:\t2 (12.5% of total)")
        self.assertEqual(s[8], "\tFalse negatives:\t2 (12.5% of total)")
        self.assertEqual(s[9], "")
        self.assertEqual(s[10], "Manually classified as good:\t3")
        self.assertEqual(s[11], "Manually classified as spam:\t3")
        self.assertEqual(s[12], "")
        self.assertEqual(s[13], "Unsures trained as good:\t1 (14.3% of unsures)")
        self.assertEqual(s[14], "Unsures trained as spam:\t1 (14.3% of unsures)")
        self.assertEqual(s[15], "Unsures not trained:\t\t5 (71.4% of unsures)")
        self.assertEqual(s[16], "")
        self.assertEqual(s[17], "Spam correctly identified:\t40.0% (+ 20.0% unsure)")
        self.assertEqual(s[18], "Good incorrectly identified:\t33.3% (+ 16.7% unsure)")
        self.assertEqual(s[19], "")
        self.assertEqual(s[20], "Total cost of spam:\t$23.40")
        self.assertEqual(s[21], "SpamBayes savings:\t$-19.40")
        self.assertEqual(len(s), 22)
    def _stuff_with_data(self, use_html=False):
        self._stuff_with_session_data()
        self._stuff_with_persistent_data()
        self.s.CalculatePersistentStats()
        return self.s.GetStats(use_html=use_html)
    def _stuff_with_session_data(self):
        self.s.RecordClassification(0.0)
        self.s.RecordClassification(0.2)
        self.s.RecordClassification(0.1)
        self.s.RecordClassification(0.4)
        self.s.RecordClassification(0.5)
        self.s.RecordClassification(0.95)
        self.s.RecordClassification(1.0)
        self.s.RecordTraining(True, 0.1)
        self.s.RecordTraining(True, 1.0)
        self.s.RecordTraining(False, 0.1)
        self.s.RecordTraining(False, 1.0)
    def _stuff_with_persistent_data(self):
        msg = Message('0')
        msg.RememberTrained(True)
        msg.RememberClassification(options['Headers','header_spam_string'])
        msg = Message('1')
        msg.RememberTrained(False)
        msg.RememberClassification(options['Headers','header_spam_string'])
        msg = Message('2')
        msg.RememberTrained(True)
        msg.RememberClassification(options['Headers','header_ham_string'])
        msg = Message('3')
        msg.RememberTrained(False)
        msg.RememberClassification(options['Headers','header_ham_string'])
        msg = Message('4')
        msg.RememberClassification(options['Headers','header_ham_string'])
        msg = Message('5')
        msg.RememberTrained(False)
        msg.RememberClassification(options['Headers','header_unsure_string'])
        msg = Message('6')
        msg.RememberTrained(True)
        msg.RememberClassification(options['Headers','header_unsure_string'])
        msg = Message('7')
        msg.RememberClassification(options['Headers','header_unsure_string'])
        msg = Message('8')
        msg.RememberClassification(options['Headers','header_unsure_string'])
    def test_with_html(self):
        s = self._stuff_with_data(True)
        for line in s:
            self.assert_('\t' not in line)
    def test_without_html(self):
        s = self._stuff_with_data(False)
        for line in s:
            self.assertEqual(line.find('&nbsp;'), -1)
    def test_from_date_empty(self):
        self._stuff_with_persistent_data()
        time.sleep(0.1)
        self.s.ResetTotal(permanently=True)
        self.s.CalculatePersistentStats()
        self.assertEqual(self.s.GetStats(), ["Messages classified: 0"])
    def test_from_specified_date(self):
        self._stuff_with_persistent_data()
        time.sleep(0.1)
        self.s.from_date = time.time()
        time.sleep(0.1)
        msg = Message('0')
        msg.RememberTrained(True)
        msg.RememberClassification(options['Headers','header_spam_string'])
        msg = Message('7')
        msg.RememberTrained(False)
        msg.RememberClassification(options['Headers','header_spam_string'])
        msg = Message('2')
        msg.RememberTrained(True)
        msg.RememberClassification(options['Headers','header_ham_string'])
        self.s.CalculatePersistentStats()
        self.assertEqual(self.s.GetStats()[0], "Messages classified: 3")
def suite():
    suite = unittest.TestSuite()
    for cls in (StatsTest,
               ):
        suite.addTest(unittest.makeSuite(cls))
    return suite
if __name__=='__main__':
    sb_test_support.unittest_main(argv=sys.argv + ['suite'])
