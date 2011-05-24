"""Usage: %(program)s [options] [filenames]
Options can one or more of:
    -h
        show usage and exit
    -v
        show version and exit
    -x
        show some usage examples and exit
    -d DBFILE
        use database in DBFILE
    -p PICKLEFILE
        use pickle (instead of database) in PICKLEFILE
    -n
        create a new database
*   -f
        filter (default if no processing options are given)
*   -g
        (re)train as a good (ham) message
*   -s
        (re)train as a bad (spam) message
*   -t
        filter and train based on the result -- you must
        make sure to untrain all mistakes later.  Not recommended.
*   -G
        untrain ham (only use if you've already trained this message)
*   -S
        untrain spam (only use if you've already trained this message)
    -o section:option:value
        set [section, option] in the options database to value
    -P
        Run under control of the Python profiler, if it is available
All options marked with '*' operate on stdin, and write the resultant
message to stdout.
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
Output is always to standard output as a Unix-style mailbox.
"""

import os

import sys

import getopt

from spambayes import hammie, Options, mboxutils, storage

from spambayes.Version import get_current_version

program = sys.argv[0]

example_doc = """_Examples_
filter a message on disk:
    %(program)s < message
(re)train a message as ham:
    %(program)s -g < message
(re)train a message as spam:
    %(program)s -s < message
procmail recipe to filter and train in one step:
    :0 fw
    | %(program)s -t
mutt configuration:  This binds the 'H' key to retrain the message as
ham, and prompt for a folder to move it to.  The 'S' key retrains as
spam, and moves to a 'spam' folder.  See contrib/muttrc in the spambayes
distribution for other neat mutt tricks.
  macro index S "|sb_filter.py -s | procmail\n"
  macro pager S "|sb_filter.py -s | procmail\n"
  macro index H "|sb_filter.py -g | procmail\n"
  macro pager H "|sb_filter.py -g | procmail\n"
  color index red black "~h 'X-Spambayes-Disposition: spam' ~F"
"""

def examples():

    print(example_doc % globals())

    sys.exit(0)
 def usage(code, msg=''):

    """Print usage message and sys.exit(code)."""

    v = get_current_version()

    print(v.get_long_version("SpamBayes Command Line Filter"), file=sys.stderr)

    print(file=sys.stderr)

    if msg:

        print(msg, file=sys.stderr)

        print(file=sys.stderr)

    print(__doc__ % globals(), file=sys.stderr)

    sys.exit(code)
 def version():

    v = get_current_version()

    print(v.get_long_version("SpamBayes Command Line Filter"), file=sys.stderr)

    sys.exit(0)
 class  HammieFilter (object) :
	def __init__(self):

        options = Options.options

        if options["Storage", "persistent_storage_file"] == \
           options.default("Storage", "persistent_storage_file"):

            options["Storage", "persistent_storage_file"] = \
                                    "~/.hammiedb"

        options.merge_files(['/etc/hammierc',
                            os.path.expanduser('~/.hammierc')])

        self.dbname, self.usedb = storage.database_type([])

        self.mode = self.h = None
 def open(self, mode):

        if self.h is None or self.mode != mode:

            if self.h is not None:

                if self.mode != 'r':

                    self.h.store()

                self.h.close()

            self.mode = mode

            self.h = hammie.open(self.dbname, self.usedb, self.mode)
 def close(self):

        if self.h is not None:

            if self.mode != 'r':

                self.h.store()

            self.h.close()

        self.h = None

	__del__ = close
	    def newdb(self):

        self.open('n')

        self.close()
 def filter(self, msg):

        if Options.options["Hammie", "train_on_filter"]:

            self.open('c')

        else:

            self.open('r')

        return self.h.filter(msg)
 def filter_train(self, msg):

        self.open('c')

        return self.h.filter(msg, train=True)
 def train_ham(self, msg):

        self.open('c')

        self.h.train_ham(msg, Options.options["Headers", "include_trained"])

        self.h.store()
 def train_spam(self, msg):

        self.open('c')

        self.h.train_spam(msg, Options.options["Headers", "include_trained"])

        self.h.store()
 def untrain_ham(self, msg):

        self.open('c')

        self.h.untrain_ham(msg)

        self.h.store()
 def untrain_spam(self, msg):

        self.open('c')

        self.h.untrain_spam(msg)

        self.h.store()

def main(profiling=False):

    h = HammieFilter()

    actions = []

    opts, args = getopt.getopt(sys.argv[1:], 'hvxd:p:nfgstGSo:P',
                               ['help', 'version', 'examples', 'option='])

    create_newdb = False

    do_profile = False

    for opt, arg in opts:

        if opt in ('-h', '--help'):

            usage(0)

        elif opt in ('-v', '--version'):

            version()

        elif opt in ('-x', '--examples'):

            examples()

        elif opt in ('-o', '--option'):

            Options.options.set_from_cmdline(arg, sys.stderr)

        elif opt == '-f':

            actions.append(h.filter)

        elif opt == '-g':

            actions.append(h.train_ham)

        elif opt == '-s':

            actions.append(h.train_spam)

        elif opt == '-t':

            actions.append(h.filter_train)

        elif opt == '-G':

            actions.append(h.untrain_ham)

        elif opt == '-S':

            actions.append(h.untrain_spam)

        elif opt == '-P':

            do_profile = True

            if not profiling:

                try:

                    import cProfile

                except ImportError:

                    pass

                else:

                    return cProfile.run("main(True)")

        elif opt == "-n":

            create_newdb = True

    h.dbname, h.usedb = storage.database_type(opts)

    if create_newdb or not os.path.exists(h.dbname):

        h.newdb()

        print("Created new database in", h.dbname, file=sys.stderr)

        if create_newdb:

            sys.exit(0)

    if actions == []:

        actions = [h.filter]

    if not args:

        args = ["-"]

    for fname in args:

        mbox = mboxutils.getmbox(fname)

        for msg in mbox:

            for action in actions:

                action(msg)

                if args == ["-"]:

                    unixfrom = msg.get_unixfrom() is not None

                else:

                    unixfrom = True

            result = mboxutils.as_string(msg, unixfrom=unixfrom)

            sys.stdout.write(result)
 if __name__ == "__main__":

    main()

 if __name__ == "__main__":

    main()



