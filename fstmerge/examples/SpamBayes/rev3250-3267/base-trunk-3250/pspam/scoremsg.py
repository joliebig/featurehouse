"""Score a message provided on stdin and show the evidence."""
import sys
import email
import locale
from types import UnicodeType
import pspam.database
from spambayes.tokenizer import tokenize
def main(fp):
    charset = locale.getdefaultlocale()[1]
    if not charset:
        charset = 'us-ascii'
    db = pspam.database.open()
    r = db.open().root()
    p = r["profile"]
    msg = email.message_from_file(fp)
    prob, evidence = p.classifier.spamprob(tokenize(msg), True)
    print "Score:", prob
    print
    print "Clues"
    print "-----"
    for clue, prob in evidence:
        if isinstance(clue, UnicodeType):
            clue = clue.encode(charset, 'replace')
        print clue, prob
if __name__ == "__main__":
    main(sys.stdin)
