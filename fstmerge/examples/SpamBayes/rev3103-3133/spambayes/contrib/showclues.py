"""Usage: showclues.py [options] [filenames]
Options can one or more of:
    -h
        show usage and exit
    -d DBFILE
        use database in DBFILE
    -p PICKLEFILE
        use pickle (instead of database) in PICKLEFILE
    -m
        markup output with HTML
    -o section:option:value
        set [section, option] in the options database to value
If no filenames are given on the command line, standard input will be
processed as a single message.  If one or more filenames are given on the
command line, each will be processed according to the following rules:
    * If the filename is '-', standard input will be processed as a single
      message (may only be usefully given once).
    * If the filename starts with '+' it will be processed as an MH folder.
    * If the filename is a directory and it contains a subdirectory named
      'cur', it will be processed as a Maildir.
    * If the filename is a directory and it contains a subdirectory named
      'Mail', it will be processed as an MH Mailbox.
    * If the filename is a directory and not a Maildir nor an MH Mailbox, it
      will be processed as a Mailbox directory consisting of just .txt and
      .lorien files.
    * Otherwise, the filename is treated as a Unix-style mailbox (messages
      begin on a line starting with 'From ').
"""

__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>"

__credits__ = "All the Spambayes folk."

try:

    True, False

except NameError:

    True, False = 1, 0



import cgi

import sys

import getopt

from spambayes import storage

from spambayes import mboxutils

from spambayes.classifier import Set

from spambayes.Options import options

from spambayes.tokenizer import tokenize

def ShowClues(bayes, msg, as_html=False):

    if as_html:

        heading = "<h2>", "</h2>"

        tt = "<tt>", "</tt>"

        br = "<br />"

        pre = "<pre>", "</pre>"

        strong = "<strong>", "</strong>"

        escape = cgi.escape

        code = "<code>", "</code>"

        wrapper = "<html>\n<head>\n<style>\n\n    h2 {color: green}\n" \
                  "</stytle>\n</head>\n<body>", "</body></html>"

    else:

        heading = '*' * 74 + "\n", "\n" + '*' * 74

        tt = "", ""

        br = ""

        pre = "", ""

        strong = "", ""

        escape = lambda a:a

        code = "", ""

        wrapper = "", ""

    tokens = list(tokenize(msg))

    toks = list(Set(tokens))

    toks.sort()

    score, clues = bayes.spamprob(iter(tokens), evidence=True)

    body = ["%sCombined Score: %d%% (%g)%s\n" %
            (heading[0], round(score*100), score, heading[1])]

    push = body.append

    word, score = clues.pop(0)

    push("Internal ham score (%s%s%s): %g%s\n" %
         (tt[0], word, tt[1], score, br))

    word, score = clues.pop(0)

    push("Internal spam score (%s%s%s): %g%s\n" %
         (tt[0], word, tt[1], score, br))

    push(br)

    push("\n")

    push("# ham trained on: %d%s\n" % (bayes.nham, br))

    push("# spam trained on: %d%s\n" % (bayes.nspam, br))

    push(br)

    push("\n")

    push("%s%s Significant Tokens%s\n%s" %
         (heading[0], len(clues), heading[1], pre[0]))

    push(strong[0])

    push("token                               spamprob         #ham  #spam\n")

    push(strong[1])

    push("\n")

    format = " %-12g %8s %6s\n"

    fetchword = bayes.wordinfo.get

    for word, prob in clues:

        record = fetchword(word)

        if record:

            nham = record.hamcount

            nspam = record.spamcount

        else:

            nham = nspam = "-"

        word = repr(word)

        push(escape(word) + " " * (35-len(word)))

        push(format % (prob, nham, nspam))

    push(pre[1])

    push("\n")

    push("%sMessage Stream%s\n%s\n" % (heading[0], heading[1], pre[0]))

    push(escape(msg.as_string()))

    push(pre[1])

    push("\n")

    push("%sAll Message Tokens%s\n" % (heading[0], heading[1]))

    push("%d unique tokens%s%s" % (len(toks), br, br))

    for token in toks:

        push("%s%s%s%s\n" % (code[0], repr(token), code[1], br))

    body = "%s%s%s" % (wrapper[0], ''.join(body), wrapper[1])

    return body
 if __name__ == "__main__":

    opts, args = getopt.getopt(sys.argv[1:], 'hmd:p:o:',
                               ['help', 'option=', 'markup'])

    markup = False

    for opt, arg in opts:

        if opt in ('-m', '--markup'):

            markup = True

        elif opt in ('-h', '--help'):

            print __doc__

            sys.exit()

        elif opt in ('-o', '--option'):

            options.set_from_cmdline(arg, sys.stderr)

    dbname, usedb = storage.database_type(opts)

    bayes = storage.open_storage(dbname, usedb)

    bayes.load()

    if not args:

        args = ["-"]

    for fname in args:

        mbox = mboxutils.getmbox(fname)

        for msg in mbox:

            print ShowClues(bayes, msg, markup)

 if __name__ == "__main__":

    opts, args = getopt.getopt(sys.argv[1:], 'hmd:p:o:',
                               ['help', 'option=', 'markup'])

    markup = False

    for opt, arg in opts:

        if opt in ('-m', '--markup'):

            markup = True

        elif opt in ('-h', '--help'):

            print __doc__

            sys.exit()

        elif opt in ('-o', '--option'):

            options.set_from_cmdline(arg, sys.stderr)

    dbname, usedb = storage.database_type(opts)

    bayes = storage.open_storage(dbname, usedb)

    bayes.load()

    if not args:

        args = ["-"]

    for fname in args:

        mbox = mboxutils.getmbox(fname)

        for msg in mbox:

            print ShowClues(bayes, msg, markup)



try:

    True, False

except NameError:

    True, False = 1, 0



