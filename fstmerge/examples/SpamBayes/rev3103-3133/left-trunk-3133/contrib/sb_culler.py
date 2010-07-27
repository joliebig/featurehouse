"""sb_culler.py -- remove spam from POP3 servers, leave ham.
I get about 150 spams a day and 12 viruses as background noise.  I use
Apple's Mail.app on my laptop, which filters out most of them.  But
when I travel my mailbox starts to accumulate crap, which is annoying
over dial-up.  Even at home, during peak periods of a recent virus
shedding I got about 30 viruses an hour, and my 10MB mailbox filled up
while I slept!
I have a server machine at home, which can stay up full time.  This
program, sb_culler, uses SpamBayes to run a POP3 email culler.  It
connects to my email servers every few minutes, downloads the emails,
classifies each one, and deletes the spam and viruses.  (It makes a
local copy of the spam, just in case.)
This program is designed for me, a programmer.  The structure should
be helpful enough for other programmers, but even configuration must
be done by editing the code.
The virus identification and POP3 manipulation code is based on Kevin
Altis' virus killer code, which I've been gratefully using for the
last several months.
Written by Andrew Dalke, November 2003.
Released into the public domain on 2003/11/22.
Updated 2004/10/26
  == NO copyright protection asserted for this code.  Share and enjoy! ==
This program requires Python 2.3 or newer.
"""
import sets, traceback, md5, os
import poplib
import posixpath
from email import Header, Utils
from spambayes import mboxutils, hammie
import socket
socket.setdefaulttimeout(10)
DO_ACTIONS = 1
VERBOSE_LEVEL = 1
APPEND_TO_FILE = "append_to_file"
DELETE = "delete"
KEEP_IN_MAILBOX = "keep in mailbox"
SPAM = "spam"
VIRUS = "virus"
class Logger:
    def __init__(self):
        self.tests = {}
        self.actions = {}
    def __nonzero__(self):
        return bool(self.tests) and bool(self.actions)
    def pass_test(self, name):
        self.tests[name] = self.tests.get(name, 0) + 1
    def do_action(self, name):
        self.actions[name] = self.actions.get(name, 0) + 1
    def accept(self, text):
        print text
    def info(self, text):
        print text
class MessageInfo:
    """reference to an email message in a mailbox"""
    def __init__(self, mailbox, i, msg, text):
        self.mailbox = mailbox
        self.i = i
        self.msg = msg
        self.text = text
class Filter:
    """if message passes test then do the given action"""
    def __init__(self, test, action):
        self.test = test
        self.action = action
    def process(self, mi, log):
        result = self.test(mi, log)
        if result:
            self.action(mi, log)
            return self.action.descr + " because " + result
        return False
class AppendFile:
    """Action: append message text to the given filename"""
    def __init__(self, filename):
        self.filename = filename
        self.descr = "save to %r then delete" % self.filename
    def __call__(self, mi,  log):
        log.do_action(APPEND_TO_FILE)
        if not DO_ACTIONS:
            return
        f = open(self.filename, "a")
        try:
            f.write(mi.text)
        finally:
            f.close()
        mi.mailbox.dele(mi.i)
def DELETE(mi, log):
    """Action: delete message from mailbox"""
    log.do_action(DELETE)
    if not DO_ACTIONS:
        return
    mi.mailbox.dele(mi.i)
DELETE.descr = "delete"
def KEEP(mi, log):
    """Action: keep message in mailbox"""
    log.do_action(KEEP_IN_MAILBOX)
KEEP.descr = "keep in mailbox"
class Duplicate:
    def __init__(self):
        self.unique = {}
    def __call__(self, mi, log):
        digest = md5.md5(mi.text).digest()
        if digest in self.unique:
            log.pass_test(SPAM)
            return "duplicate"
        self.unique[digest] = 1
        return False
class IllegalDeliveredTo:
    def __init__(self, names):
        self.names = names
    def __call__(self, mi, log):
        fields = mi.msg.get_all("Delivered-To")
        if fields is None:
            return False
        for field in fields:
            field = field.lower()
            for name in self.names:
                if name in field:
                    return False
        log.pass_test(SPAM)
        return "sent to random email"
class SpamAssassin:
    def __init__(self, level = 8):
        self.level = level
    def __call__(self, mi, log):
        if ("*" * self.level) in mi.msg.get("X-Spam-Status", ""):
            log.pass_test(SPAM)
            return "assassinated!"
        return False
class WhiteListFrom:
    """Test: Read a list of email addresses to use a 'from' whitelist"""
    def __init__(self, filename):
        self.filename = filename
        self._mtime = 0
        self._load_if_needed()
    def _load(self):
        lines = [line.strip().lower() for line in
                           open(self.filename).readlines()]
        self.addresses = sets.Set(lines)
    def _load_if_needed(self):
        mtime = os.path.getmtime(self.filename)
        if mtime != self._mtime:
            print "Reloading", self.filename
            self._mtime = mtime
            self._load()
    def __call__(self, mi, log):
        self._load_if_needed()
        frm = mi.msg["from"]
        realname, frm = Utils.parseaddr(frm)
        status = (frm is not None) and (frm.lower() in self.addresses)
        if status:
            log.pass_test(SPAM)
            return "it is in 'from' white list"
        return False
class WhiteListSubstrings:
    """Test: Whitelist message if named field contains one of the substrings"""
    def __init__(self, field, substrings):
        self.field = field
        self.substrings = substrings
    def __call__(self, mi, log):
        data = mi.msg[self.field]
        if data is None:
            return False
        for s in self.substrings:
            if s in data:
                log.pass_test("'%s' white list" % (self.field,))
                return "it matches '%s' white list" % (self.field,)
        return False
class IsSpam:
    """Test: use SpamBayes to tell if something is spam"""
    def __init__(self, sb_hammie, spam_cutoff = None):
        self.sb_hammie = sb_hammie
        if spam_cutoff is None:
            spam_cutoff = options["Categorization", "spam_cutoff"]
        self.spam_cutoff = spam_cutoff
    def __call__(self, mi, log):
        prob = self.sb_hammie.score(mi.msg)
        if prob > self.spam_cutoff:
            log.pass_test(SPAM)
            return "it is spam (%4.3f)" % prob
        if VERBOSE_LEVEL > 1:
            print "not spam (%4.3f)" % prob
        return False
def IsVirus(mi, log):
    """Test: a virus is any message with an attached executable
    I've also noticed the viruses come in as wav and midi attachements
    so I trigger on those as well.
    This is a very paranoid detector, since someone might send me a
    binary for valid reasons.  I white-list everyone who's sent me
    email before so it doesn't affect me.
    """
    for part in mi.msg.walk():
        if part.get_main_type() == 'multipart':
            continue
        filename = part.get_filename()
        if filename is None:
            if part.get_type() in ["application/x-msdownload",
                                   "audio/x-wav", "audio/x-midi"]:
                log.pass_test(VIRUS)
                return ("it has a virus-like content-type (%s)" %
                        part.get_type())
        else:
            extensions = "bat com exe pif ref scr vbs wsh".split()
            base, ext = posixpath.splitext(filename)
            if ext[1:].lower() in extensions:
                log.pass_test(VIRUS)
                return "it has a virus-like attachment (%s)" % ext[1:]
    return False
def open_mailbox(server, username, password, debuglevel = 0):
    mailbox = poplib.POP3(server)
    try:
        mailbox.user(username)
        mailbox.pass_(password)
        mailbox.set_debuglevel(debuglevel)
        if VERBOSE_LEVEL > 1:
            count, size = mailbox.stat()
            print "Message count:   ", count
            print "Total bytes  :   ", size
    except:
        mailbox.quit()
        raise
    return mailbox
def _log_subject(mi, log):
    encoded_subject = mi.msg.get('subject')
    try:
        subject, encoding = Header.decode_header(encoded_subject)[0]
    except Header.HeaderParseError:
        log.info("%s Subject cannot be parsed" % (mi.i,))
        return
    if encoding is None or encoding == 'iso-8859-1':
        s = subject
    else:
        s = encoded_subject
    log.info("%s Subject: %r" % (mi.i, s))
class Filters(list):
    def add(self, test, action):
        """short-cut to make a Filter given the test and action"""
        self.append(Filter(test, action))
    def process_mailbox(self, mailbox):
        count, size = mailbox.stat()
        log = Logger()
        for i in range(1, count+1):
            if (i-1) % 10 == 0:
                print " == %d/%d ==" % (i, count)
            message_tuple = mailbox.top(i, 1000000)
            text = "\n".join(message_tuple[1])
            msg = mboxutils.get_message(text)
            mi = MessageInfo(mailbox, i, msg, text)
            _log_subject(mi, log)
            for filter in self:
                result = filter.process(mi, log)
                if result:
                    log.accept(result)
                    break
            else:
                log.pass_test("unknown")
                log.do_action(KEEP_IN_MAILBOX)
                log.accept("unknown")
        return log
def filter_server( (server, user, pwd), filters):
    if VERBOSE_LEVEL:
        print "=" * 78
        print "Processing %s on %s" % (user, server)
    mailbox = open_mailbox(server, user, pwd)
    try:
        log = filters.process_mailbox(mailbox)
    finally:
        mailbox.quit()
    return log
import time, sys, urllib
def _unix_stop():
    pass
def _ms_stop():
    if msvcrt.kbhit():
        raise SystemExit()
try:
    import msvcrt
    _check_for_stop = _ms_stop
except ImportError:
    _check_for_stop = _unix_stop
def restart_network():
    print "Network appears to be down.  Bringing Linksys down then up..."
    try:
        urllib.urlopen("http://:admin@192.168.1.1/Gozila.cgi?pppoeAct=2").read()
        urllib.urlopen("http://:admin@192.168.1.1/Gozila.cgi?pppoeAct=1").read()
    except KeyboardInterrupt:
        raise
    except:
        traceback.print_exc()
def wait(t, delta = 10):
    """Wait for 't' seconds"""
    assert delta > 0, delta
    assert t >= 1
    first = True
    for i in range(t, -1, -delta):
        if VERBOSE_LEVEL:
            if not first:
                print "..",
            print i,
            sys.stdout.flush()
        time.sleep(min(i, delta))
        _check_for_stop()
        first = False
    print
def main():
    filters = Filters()
    duplicate = Duplicate()
    filters.add(duplicate, AppendFile("spam2.mbox"))
    filters.add(WhiteListFrom("good_emails.txt"), KEEP)
    filters.add(WhiteListSubstrings("subject", [
                   'ABCD:',
                   '[Python-announce]',
                   '[Python]',
                   '[Bioinfo]',
                   '[EuroPython]',
                   ]),
                KEEP)
    filters.add(WhiteListSubstrings("to", [
        "president@whitehouse.gov",
        "ceo@big.com",
        ]),
                KEEP)
    names = ["john", "", "jon", "johnathan"]
    valid_emails = ([name + "@lectroid.com" for name in names] +
                    [name + "@bigboote.org" for name in names] +
                    ["buckeroo.bonzai@aol.earth"])
    filters.add(IllegalDeliveredTo(valid_emails), DELETE)
    filters.add(SpamAssassin(), AppendFile("spam2.mbox"))
    filters.add(IsVirus, DELETE)
    h = hammie.open("cull.spambayes", "dbm", "r")
    filters.add(IsSpam(h, 0.90), AppendFile("spam.mbox"))
    server_configs = [("mail.example.com",
                          "user@example.com", "password"),
                      ("popserver.big.com", "ceo", "12345"), ]
    error_count = 0
    cumulative_log = {SPAM: 0, VIRUS: 0}
    initial_log = None
    start_time = None  
    while 1:
        error_flag = False
        duplicate.unique.clear()  
        for server, user, pwd in server_configs:
            try:
                log = filter_server( (server, user, pwd), filters)
            except KeyboardInterrupt:
                raw_input("Press enter to continue. ")
            except StandardError:
                raise
            except:
                error_flag = True
                traceback.print_exc()
                continue
            if VERBOSE_LEVEL > 1 and log:
                print "  ** Summary **"
                for x in (log.tests, log.actions):
                    items = x.items()
                    if items:
                        items.sort()
                        for k, v in items:
                            print "  %s: %s" % (k, v)
                        print
            cumulative_log[SPAM] += log.tests.get(SPAM, 0)
            cumulative_log[VIRUS] += log.tests.get(VIRUS, 0)
        if initial_log is None:
            initial_log = cumulative_log.copy()
            start_time = time.time()
            if VERBOSE_LEVEL:
                print "Stats: %d spams, %d virus" % (
                    initial_log[SPAM], initial_log[VIRUS])
        else:
            if VERBOSE_LEVEL:
                delta_t = time.time() - start_time
                delta_t = max(delta_t, 1)  
                print "Stats: %d spams (%.2f/hr), %d virus (%.2f/hr)" % (
                    cumulative_log[SPAM],
                    (cumulative_log[SPAM] - initial_log[SPAM]) /
                             delta_t * 3600,
                    cumulative_log[VIRUS],
                    (cumulative_log[VIRUS] - initial_log[VIRUS]) /
                             delta_t * 3600)
        if error_flag:
            error_count += 1
        if error_count > 0:
            restart_network()
            error_count = 0
        delay = 10 * 60
        while delay:
            try:
                wait(delay)
                break
            except KeyboardInterrupt:
                print
                while 1:
                    cmd = raw_input("enter, delay, or quit? ")
                    if cmd in ("q", "quit"):
                        raise SystemExit(0)
                    elif cmd == "":
                        delay = 0
                        break
                    elif cmd.isdigit():
                        delay = int(cmd)
                        break
                    else:
                        print "Unknown command."
if __name__ == "__main__":
    main()
