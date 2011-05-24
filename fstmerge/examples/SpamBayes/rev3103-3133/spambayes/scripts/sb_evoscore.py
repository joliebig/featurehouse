"""A shim for integrating Spambayes and Ximian Evolution.
Evolution is a free email client for Linux and Solaris, developed by Ximian.
See www.ximian.com for details.  Evolution is sometimes called 'Evo' for
short.
Actually Evo is more than that -- you can think of it as an Outlook-alike for
Linux -- but all we care about here is the mail reader.  Evo can connect to a
POP or IMAP server, so you can of course use Spambayes' normal POP or IMAP
filters.
For a variety of reasons, I don't like hooking things up this way.  In Evo,
you can specify filters which run on folders whenever a message shows up in
that folder.  The filter can have any number of criteria, and if the criteria
match, Evo will execute some number of actions.  One of the things a filter
can do is pipe the message to a program's standard in, and then check the exit
code of that program.  That's how we'll hook things together.
You'll need to start sb_xmlrpcserver.py, as this script is a client of that
server.  You'll use sb_imapfilter.py to train a database on the machine local
to your Evo client (yes, if you use many different workstations, you'll need
your database on all of them).
sb_evoscore.py takes the message from standard in, sends it to the xmlrpc
server and receives the float spam score for the message.  Then it compares
this to your ham_cutoff and spam_cutoff options.  It exits with a return code
that your Evo filter will check.  The return codes are:
   -1 - Error
    0 - Ham
    1 - Unsure
    2 - Spam
So, to hook things up do the following:
- In Evo, go to Tools -> Filters... to bring up the filter rules
- Select 'incoming' as the filter rule direction
- Add a new filter rule called 'Spambayes Spam'
- Select 'Pipe Message to Shell Command' as the first and only criteria.
  Point the command at this script, e.g. /usr/local/bin/sb_evoscore.py
- Select the match criteria to be 'returns 2'
- In the 'Then' section, select what you want to have happen for Spam.
  Personally, I have a folder called SBInbox, and inside that folder I have
  four subfolders: HamTrain, SpamTrain, Spam, and Unsure.  My action for the
  'Spambayes Spam' filter is then 'Move to Folder SBInbox/Spam'.
- Now do the same thing with a second filter rule called 'Spambayes Unsure',
  except this time, match a return value of 1, and move these messages to
  SBInbox/Unsure.
- Finally, make sure Evo will run filters automatically when your inbox
  receives new messages.
Now, what I do is throw a bunch of known ham in SBInbox/HamTrain and a bunch
of known spam in SBInbox/SpamTrain.  I use 'sb_imapfilter -t -v -p' to train a
database on my local machine.  Then I start up sb_xmlrpcserver.py.
NOTE: you must edit the variable RPCURL below to match how you invoke
sb_xmlrpcserver.py.
For a while, I watch the Unsure folder, moving mistakes to SpamTrain and
HamTrain respectively.  Every once in a while, I retrain my database, and copy
my database to all my other desktops.  One caveat: I've found that if I kill
the xmlrpc server while Evo is still running, it can cause Evo to hang, choke,
or start spewing endless error messages.  It's best to exit Evo before killing
sb_xmlrpcserver.py.
"""

import sys

import xmlrpclib

from spambayes.Options import options

RPCURL = 'http://localhost:8881'

def main():

    msg = sys.stdin.read()

    try:

        server = xmlrpclib.ServerProxy(RPCURL)

        score = server.score(xmlrpclib.Binary(msg))

    except:

        import traceback

        traceback.print_exc()

        return -1

    else:

        if score < options['Categorization', 'ham_cutoff']:

            return 0

        elif score < options['Categorization', 'spam_cutoff']:

            return 1

        return 2


status = main()

sys.exit(status)

