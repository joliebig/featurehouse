"""\
To train:
    %(program)s -t ham.mbox spam.mbox
To filter mail (using .forward or .qmail):
    |%(program)s Maildir/ Mail/Spam/
To print the score and top evidence for a message or messages:
    %(program)s -s message [message ...]
"""
SPAM_CUTOFF = 0.57
SIZE_LIMIT = 5000000 # messages larger are not analyzed
BLOCK_SIZE = 10000
RC_DIR = "~/.spambayes"
DB_FILE = RC_DIR + "/wordprobs.cdb"
CONFIG_FILE = RC_DIR + "/bayescustomize.ini"
import sys
import os
import getopt
import email
import time
import signal
import socket
import errno
DB_FILE = os.path.expanduser(DB_FILE)
def import_spambayes():
    global mboxutils, CdbClassifier, tokenize
    if 'BAYESCUSTOMIZE' not in os.environ:
        os.environ['BAYESCUSTOMIZE'] = os.path.expanduser(CONFIG_FILE)
    from spambayes import mboxutils
    from spambayes.cdb_classifier import CdbClassifier
    from spambayes.tokenizer import tokenize
program = sys.argv[0] # For usage(); referenced by docstring above
def usage(code, msg=''):
    """Print usage message and sys.exit(code)."""
    if msg:
        print(msg, file=sys.stderr)
        print(file=sys.stderr)
    print(__doc__ % globals(), file=sys.stderr)
    sys.exit(code)
def maketmp(dir):
    hostname = socket.gethostname()
    pid = os.getpid()
    fd = -1
    for x in range(200):
        filename = "%d.%d.%s" % (time.time(), pid, hostname)
        pathname = "%s/tmp/%s" % (dir, filename)
        try:
            fd = os.open(pathname, os.O_WRONLY|os.O_CREAT|os.O_EXCL, 384)
        except IOError as exc:
            if exc[0] not in (errno.EINT, errno.EEXIST):
                raise
        else:
            break
        time.sleep(2)
    if fd == -1:
        raise SystemExit("could not create a mail file")
    return (os.fdopen(fd, "wb"), pathname, filename)
def train(bayes, msgs, is_spam):
    """Train bayes with all messages from a mailbox."""
    mbox = mboxutils.getmbox(msgs)
    for msg in mbox:
        bayes.learn(tokenize(msg), is_spam)
def train_messages(ham_name, spam_name):
    """Create database using messages."""
    rc_dir = os.path.expanduser(RC_DIR)
    if not os.path.exists(rc_dir):
        print("Creating", RC_DIR, "directory...")
        os.mkdir(rc_dir)
    bayes = CdbClassifier()
    print('Training with ham...')
    train(bayes, ham_name, False)
    print('Training with spam...')
    train(bayes, spam_name, True)
    print('Update probabilities and writing DB...')
    db = open(DB_FILE, "wb")
    bayes.save_wordinfo(db)
    db.close()
    print('done')
def filter_message(hamdir, spamdir):
    signal.signal(signal.SIGALRM, lambda s, f: sys.exit(1))
    signal.alarm(24 * 60 * 60)
    tmpfile, pathname, filename = maketmp(hamdir)
    try:
        tmpfile.write(os.environ.get("DTLINE", "")) # delivered-to line
        bytes = 0
        blocks = []
        while 1:
            block = sys.stdin.read(BLOCK_SIZE)
            if not block:
                break
            bytes += len(block)
            if bytes < SIZE_LIMIT:
                blocks.append(block)
            tmpfile.write(block)
        tmpfile.close()
        if bytes < SIZE_LIMIT:
            msgdata = ''.join(blocks)
            del blocks
            msg = email.message_from_string(msgdata)
            del msgdata
            bayes = CdbClassifier(open(DB_FILE, 'rb'))
            prob = bayes.spamprob(tokenize(msg))
        else:
            prob = 0.0
        if prob > SPAM_CUTOFF:
            os.rename(pathname, "%s/new/%s" % (spamdir, filename))
        else:
            os.rename(pathname, "%s/new/%s" % (hamdir, filename))
    except:
        os.unlink(pathname)
        raise
def print_message_score(msg_name, msg_fp):
    msg = email.message_from_file(msg_fp)
    bayes = CdbClassifier(open(DB_FILE, 'rb'))
    prob, evidence = bayes.spamprob(tokenize(msg), evidence=True)
    print(msg_name, prob)
    for word, prob in evidence:
        print('  ', repr(word), prob)
def main():
    global DB_FILE, CONFIG_FILE
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'tsd:c:')
    except getopt.error as msg:
        usage(2, msg)
    mode = 'sort'
    for opt, val in opts:
        if opt == '-t':
            mode = 'train'
        elif opt == '-s':
            mode = 'score'
        elif opt == '-d':
            DB_FILE = val
        elif opt == '-c':
            CONFIG_FILE = val
        else:
            assert 0, 'invalid option'
    import_spambayes()
    if mode == 'sort':
        if len(args) != 2:
            usage(2, 'wrong number of arguments')
        filter_message(args[0], args[1])
    elif mode == 'train':
        if len(args) != 2:
            usage(2, 'wrong number of arguments')
        train_messages(args[0], args[1])
    elif mode == 'score':
        if args:
            for msg in args:
                print_message_score(msg, open(msg))
        else:
            print_message_score('<stdin>', sys.stdin)
if __name__ == "__main__":
    main()
