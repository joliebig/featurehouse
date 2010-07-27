"""Testing Tools Web Interface
Classes:
    TestToolsUserInterface - Interface class for testing tools.
Abstract:
This module implements a browser based Spambayes user interface for the
various testing tools.  Users may use it to interface with the tools.
The following functions are currently included:
  onCV - cross-validation testing
To do:
 o Add interface to Alex's incremental test setup.
 o Suggestions?
"""
from __future__ import generators
__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>"
__credits__ = "All the Spambayes folk."
import os
import sys
import cgi
import glob
import random
import StringIO
from spambayes import ProxyUI
from spambayes import oe_mailbox
from spambayes import msgs
from spambayes import TestDriver
from spambayes import OptionsClass
from spambayes.Options import options
testtools_ini_map = (
    ('Experimental Options', None),
)
for opt in options.options(True):
    _sect, _opt = opt[1:].split(']', 1)
    if opt[:2].lower() == "x-":
        testtools_ini_map += ((_sect, _opt),)
class TestToolsUserInterface(ProxyUI.ProxyUserInterface):
    """Serves the HTML user interface for the test tools."""
    def onCv(self):
        global testtools_ini_map
        self._writePreamble("CV Test")
        configTable = self.html.configForm.clone()
        del configTable.configTextRow1
        del configTable.configTextRow2
        del configTable.configCbRow1
        del configTable.configRow2
        del configTable.blankRow
        del configTable.folderRow
        sect = 'TestToolsUI'
        for newopt in [('source', 'Messages source', 'Standard test setup',
                        'Select the source of the messages to test on.',
                        ('Standard test setup', 'Cache', 'Outlook Express'),
                        False),
                       ('n', 'Number of runs', 10,
                        'Select the number of cross-validation runs.',
                        OptionsClass.INTEGER, False),]:
            options._options[sect, newopt[0]] = OptionsClass.Option(*newopt)
        testtools_ini_map += (('Testing Options', None),
                              ('TestToolsUI', 'source'),
                              ('TestToolsUI', 'n'),)
        option_choice = self._buildConfigPageBody(configTable,
                                                  testtools_ini_map)
        option_choice.action_page.action = "cvresults"
        option_choice.introduction = "Select the options for your test " \
                                     "(these will be run against the " \
                                     "defaults)."
        option_choice.optionsPathname = "memory only"
        del option_choice.restore_form
        del option_choice.adv_button
        option_choice.config_submit.value = "Run Test"
        self.write(option_choice)
        self._writePostamble()
    def onCvresults(self, *args, **kwargs):
        del kwargs["how"]
        self._writePreamble("CV Test Results")
        text = "Display the results of a cross-validation test with the " \
               "current settings against the defaults."
        nsets = options["TestToolsUI", "n"]
        self.write("<p>Testing with defaults...</p>")
        saved = {}
        for opt in options.options(True):
            sect, opt = opt[1:].split(']', 1)
            saved[(sect, opt)] = options[(sect, opt)]
            if not options.no_restore(sect, opt):
                options.set(sect, opt, options.default(sect, opt))
        options["TestToolsUI", "source"] = kwargs["TestToolsUI_source"]
        cv_out, errors = self.timCV(nsets)
        defaults = self.rates(cv_out)
        self.write("<p>Testing with selected settings...</p>")
        for opt in options.options(True):
            sect, opt = opt[1:].split(']', 1)
            try:
                value = kwargs["%s_%s" % (sect, opt)]
            except KeyError:
                pass
            else:
                options.set(sect, opt, value)
        cv_out, errors = self.timCV(nsets)
        current = self.rates(cv_out)
        for opt in options.options(True):
            sect, opt = opt[1:].split(']', 1)
            options.set(sect, opt, saved[(sect, opt)])
        comp, errors = self.compare(defaults, current)
        comp = comp.read()
        box = self._buildBox('Cross-validation test', None,
                             cgi.escape(comp).replace("\n", "<br />"))
        self.write(box)
        self._writePostamble()
    def timCV(self, nsets):
        cout, cerr = sys.stdout, sys.stderr
        sys.stdout = StringIO.StringIO()
        sys.stderr = StringIO.StringIO()
        if options["TestToolsUI", "source"] == "Standard test setup":
            hamdirs  = [options["TestDriver", "ham_directories"] % \
                        i for i in range(1, nsets+1)]
            spamdirs = [options["TestDriver", "spam_directories"] % \
                        i for i in range(1, nsets+1)]
            hstream = msgs.HamStream
            sstream = msgs.SpamStream
        elif options["TestToolsUI", "source"] == "Cache":
            hamdirs  = ["%s%s%s/%s" % (options["Storage", "ham_cache"],
                                       os.pathsep, i, nsets)
                        for i in range(1, nsets+1)]
            spamdirs = ["%s%s%s/%s" % (options["Storage", "spam_cache"],
                                       os.pathsep, i, nsets)
                        for i in range(1, nsets+1)]
            hstream = HamCacheStream
            sstream = SpamCacheStream
        elif options["TestToolsUI", "source"] == "Outlook Express":
            dbx_dir = oe_mailbox.OEStoreRoot()
            dbxes = glob.glob(os.path.join(dbx_dir, "*.dbx"))
            spamdirs = []
            hamdirs = []
            for dbx in dbxes:
                if os.path.splitext(os.path.basename(dbx))[0].lower() in \
                   ["deleted items", "drafts", "folders",
                    "offline", "outbox", "pop3uidl",]:
                    continue
                elif dbx.lower().find("spam") == -1:
                    spamdirs.append(dbx)
                else:
                    hamdirs.append(dbx)
            hstream = oe_mailbox.OEHamStream
            sstream = oe_mailbox.OESpamStream
        d = TestDriver.Driver()
        h = hstream("%s-%d" % (hamdirs[1], nsets), hamdirs[1:], train=1)
        d.train(hstream("%s-%d" % (hamdirs[1], nsets), hamdirs[1:],
                        train=1),
                sstream("%s-%d" % (spamdirs[1], nsets), spamdirs[1:],
                        train=1))
        for i in range(nsets):
            h = hamdirs[i]
            s = spamdirs[i]
            hamstream = hstream(h, [h], train=0)
            spamstream = sstream(s, [s], train=0)
            if i > 0:
                if options["CV Driver", "build_each_classifier_from_scratch"]:
                    d.new_classifier()
                    hname = "%s-%d, except %d" % (hamdirs[0], nsets, i+1)
                    h2 = hamdirs[:]
                    del h2[i]
                    sname = "%s-%d, except %d" % (spamdirs[0], nsets, i+1)
                    s2 = spamdirs[:]
                    del s2[i]
                    d.train(hstream(hname, h2, train=1),
                            sstream(sname, s2, train=1))
                else:
                    d.untrain(hamstream, spamstream)
            d.test(hamstream, spamstream)
            d.finishtest()
            if i < nsets - 1 and not options["CV Driver",
                                             "build_each_classifier_from_scratch"]:
                d.train(hamstream, spamstream)
        d.alldone()
        sys.stdout.seek(0)
        sys.stderr.seek(0)
        out, err = sys.stdout, sys.stderr
        sys.stdout = cout
        sys.stderr = cerr
        return out, err
    def rates(self, ifile):
        """This is essentially rates.py from the testtools directory."""
        cout = sys.stdout
        cerr = sys.stderr
        sys.stdout = StringIO.StringIO()
        sys.stderr = StringIO.StringIO()
        interesting = filter(lambda line: line.startswith('-> '), ifile)
        ifile.close()
        ofile = StringIO.StringIO()
        def dump(*stuff):
            msg = ' '.join(map(str, stuff))
            print msg
            print >> ofile, msg
        ntests = nfn = nfp = 0
        sumfnrate = sumfprate = 0.0
        for line in interesting:
            dump(line[:-1])
            fields = line.split()
            if line.startswith('-> <stat> tested '):
                ntests += 1
                continue
            if line.startswith('-> <stat> false '):
                kind = fields[3]
                percent = float(fields[-1])
                if kind == 'positive':
                    sumfprate += percent
                    lastval = percent
                else:
                    sumfnrate += percent
                    dump('    %7.3f %7.3f' % (lastval, percent))
                continue
            if len(fields) >= 5 and fields[3] == 'new' and fields[4] == 'false':
                kind = fields[-1]
                count = int(fields[2])
                if kind == 'positives':
                    nfp += count
                else:
                    nfn += count
        dump('total unique false pos', nfp)
        dump('total unique false neg', nfn)
        dump('average fp %', sumfprate / ntests)
        dump('average fn %', sumfnrate / ntests)
        ofile.seek(0)
        sys.stdout = cout
        sys.stderr = cerr
        return ofile
    def compare(self, f1, f2):
        """This is essentially cmp.py from the testtools directory."""
        cout, cerr = sys.stdout, sys.stderr
        sys.stdout = StringIO.StringIO()
        sys.stderr = StringIO.StringIO()
        def suck(f):
            fns = []
            fps = []
            hamdev = []
            spamdev = []
            hamdevall = spamdevall = (0.0, 0.0)
            get = f.readline
            while 1:
                line = get()
                if line.startswith('-> <stat> tested'):
                    print line,
                if line.find(' items; mean ') != -1:
                    vals = line.split(';')
                    mean = float(vals[1].split()[-1])
                    sdev = float(vals[2].split()[-1])
                    val = (mean, sdev)
                    typ = vals[0].split()[2]
                    if line.find('for all runs') != -1:
                        if typ == 'Ham':
                            hamdevall = val
                        else:
                            spamdevall = val
                    elif line.find('all in this') != -1:
                        if typ == 'Ham':
                            hamdev.append(val)
                        else:
                            spamdev.append(val)
                    continue
                if line.startswith('-> '):
                    continue
                if line.startswith('total'):
                    break
                if len(line) == 0:
                    continue
                p, n = map(float, line.split())
                fps.append(p)
                fns.append(n)
            fptot = int(line.split()[-1])
            fntot = int(get().split()[-1])
            fpmean = float(get().split()[-1])
            fnmean = float(get().split()[-1])
            return (fps, fns, fptot, fntot, fpmean, fnmean,
                    hamdev, spamdev, hamdevall, spamdevall)
        def tag(p1, p2):
            if p1 == p2:
                t = "tied          "
            else:
                t = p1 < p2 and "lost " or "won  "
                if p1:
                    p = (p2 - p1) * 100.0 / p1
                    t += " %+7.2f%%" % p
                else:
                    t += " +(was 0)"
            return t
        def mtag(m1, m2):
            mean1, dev1 = m1
            mean2, dev2 = m2
            t = "%7.2f %7.2f " % (mean1, mean2)
            if mean1:
                mp = (mean2 - mean1) * 100.0 / mean1
                t += "%+7.2f%%" % mp
            else:
                t += "+(was 0)"
            t += "     %7.2f %7.2f " % (dev1, dev2)
            if dev1:
                dp = (dev2 - dev1) * 100.0 / dev1
                t += "%+7.2f%%" % dp
            else:
                t += "+(was 0)"
            return t
        def dump(p1s, p2s):
            alltags = ""
            for p1, p2 in zip(p1s, p2s):
                t = tag(p1, p2)
                print "    %5.3f  %5.3f  %s" % (p1, p2, t)
                alltags += t + " "
            print
            for t in "won", "tied", "lost":
                print "%-4s %2d times" % (t, alltags.count(t))
            print
        def dumpdev(meandev1, meandev2):
            for m1, m2 in zip(meandev1, meandev2):
                print mtag(m1, m2)
        (fp1, fn1, fptot1, fntot1, fpmean1, fnmean1,
         hamdev1, spamdev1, hamdevall1, spamdevall1) = suck(f1)
        (fp2, fn2, fptot2, fntot2, fpmean2, fnmean2,
         hamdev2, spamdev2, hamdevall2, spamdevall2) = suck(f2)
        print
        print "false positive percentages"
        dump(fp1, fp2)
        print "total unique fp went from", fptot1, "to", fptot2, tag(fptot1, fptot2)
        print "mean fp % went from", fpmean1, "to", fpmean2, tag(fpmean1, fpmean2)
        print
        print "false negative percentages"
        dump(fn1, fn2)
        print "total unique fn went from", fntot1, "to", fntot2, tag(fntot1, fntot2)
        print "mean fn % went from", fnmean1, "to", fnmean2, tag(fnmean1, fnmean2)
        print
        if len(hamdev1) == len(hamdev2) and len(spamdev1) == len(spamdev2):
            print "ham mean                     ham sdev"
            dumpdev(hamdev1, hamdev2)
            print
            print "ham mean and sdev for all runs"
            dumpdev([hamdevall1], [hamdevall2])
            print
            print "spam mean                    spam sdev"
            dumpdev(spamdev1, spamdev2)
            print
            print "spam mean and sdev for all runs"
            dumpdev([spamdevall1], [spamdevall2])
            print
            diff1 = spamdevall1[0] - hamdevall1[0]
            diff2 = spamdevall2[0] - hamdevall2[0]
            print "ham/spam mean difference: %2.2f %2.2f %+2.2f" % (diff1,
                                                                    diff2,
                                                                    diff2 - diff1)
        else:
            print "[info about ham & spam means & sdevs not available in both files]"
        sys.stdout.seek(0)
        sys.stderr.seek(0)
        out, err = sys.stdout, sys.stderr
        sys.stdout = cout
        sys.stderr = cerr
        return out, err
class CacheStream(msgs.MsgStream):
    def produce(self):
        base_check = None
        for directory in self.directories:
            directory, portion = directory.split(os.pathsep)
            if base_check is None:
                base_check = directory
            assert directory == base_check
            set_num, nsets = portion.split('/')
            files = os.listdir(directory)
            random.seed(hash(max(files)) ^ msgs.SEED)
            random.shuffle(files)
            set_size = len(files) // int(nsets)
            set_num = int(set_num)
            fileset = files[set_num*set_size:((set_num+1)*set_size)-1]
            fileset.sort()
            for fname in fileset:
                yield msgs.Msg(directory, fname)
class HamCacheStream(CacheStream):
    def __init__(self, tag, directories, train=0):
        if train:
            CacheStream.__init__(self, tag, directories, msgs.HAMTRAIN)
        else:
            CacheStream.__init__(self, tag, directories, msgs.HAMTEST)
class SpamCacheStream(CacheStream):
    def __init__(self, tag, directories, train=0):
        if train:
            CacheStream.__init__(self, tag, directories, msgs.SPAMTRAIN)
        else:
            CacheStream.__init__(self, tag, directories, msgs.SPAMTEST)
