"""Utilities for dealing with various types of mailboxes.
This is mostly a wrapper around the various useful classes in the
standard mailbox module, to do some intelligent guessing of the
mailbox type given a mailbox argument.
+foo      -- MH mailbox +foo
+foo,bar  -- MH mailboxes +foo and +bar concatenated
+ALL      -- a shortcut for *all* MH mailboxes
/foo/bar  -- (existing file) a Unix-style mailbox
/foo/bar/ -- (existing directory) a directory full of .txt and .lorien
             files
/foo/bar/ -- (existing directory with a cur/ subdirectory)
             Maildir mailbox
/foo/Mail/bar/ -- (existing directory with /Mail/ in its path)
             alternative way of spelling an MH mailbox
"""

from __future__ import generators

import os

import sys

import glob

import email

import mailbox

import email.Message

import re

import traceback

class  DirOfTxtFileMailbox :
	"""Directory of files each assumed to contain an RFC-822 message.
    If the filename ends with ".emlx", assumes that the file is an
    RFC-822 message wrapped in Apple Mail's proprietory .emlx format.
    The emlx format is simply the length of the message (as a string
    on the first line, then the raw message text, then the contents of
    a plist (XML) file that contains data that Mail uses (subject,
    flags, sender, and so forth).  We ignore this plist data).
    Subdirectories are traversed recursively.
    """
	    def __init__(self, dirname, factory):

        self.names = glob.glob(os.path.join(dirname, "*"))

        self.names.sort()

        self.factory = factory
 def __iter__(self):

        for name in self.names:

            if os.path.isdir(name):

                for mbox in DirOfTxtFileMailbox(name, self.factory):

                    yield mbox

            elif os.path.splitext(name)[1] == ".emlx":

                f = open(name)

                length = int(f.readline().rstrip())

                yield self.factory(f.read(length))

                f.close()

            else:

                try:

                    f = open(name)

                except IOError:

                    continue

                yield self.factory(f)

                f.close()

def full_messages(msgs):

    """A generator that transforms each message by calling its
    get_full_message() method.  Used for IMAP messages since they don't really
    have their content by default.
    """

    for x in msgs:

        yield x.get_full_message()
 def _cat(seqs):

    for seq in seqs:

        for item in seq:

            yield item
 def getmbox(name):

    """Return an mbox iterator given a file/directory/folder name."""

    if name == "-":

        return [get_message(sys.stdin)]

    if name.startswith("+"):

        name = name[1:]

        import mhlib

        mh = mhlib.MH()

        if name == "ALL":

            names = mh.listfolders()

        elif ',' in name:

            names = name.split(',')

        else:

            names = [name]

        mboxes = []

        mhpath = mh.getpath()

        for name in names:

            filename = os.path.join(mhpath, name)

            mbox = mailbox.MHMailbox(filename, get_message)

            mboxes.append(mbox)

        if len(mboxes) == 1:

            return iter(mboxes[0])

        else:

            return _cat(mboxes)

    elif name.startswith(":"):

        parts = re.compile(
':(?P<user>[^@:]+):(?P<pwd>[^@]+)@(?P<server>[^:]+(:[0-9]+)?):(?P<name>[^:]+)'
        ).match(name).groupdict()

        from scripts.sb_imapfilter import IMAPSession, IMAPFolder

        from spambayes import Stats, message

        from spambayes.Options import options

        session = IMAPSession(parts['server'])

        session.login(parts['user'], parts['pwd'])

        folder_list = session.folder_list()

        if name == "ALL":

            names = folder_list

        else:

            names = parts['name'].split(',')

        message_db = message.Message().message_info_db

        stats = Stats.Stats(options, message_db)

        mboxes = [ IMAPFolder(n,session,stats) for n in names ]

        if len(mboxes) == 1:

            return full_messages(mboxes[0])

        else:

            return _cat([full_messages(x) for x in mboxes])

    if os.path.isdir(name):

        if os.path.exists(os.path.join(name, 'cur')):

            mbox = mailbox.Maildir(name, get_message)

        elif name.find("/Mail/") >= 0:

            mbox = mailbox.MHMailbox(name, get_message)

        else:

            mbox = DirOfTxtFileMailbox(name, get_message)

    else:

        fp = open(name, "rb")

        mbox = mailbox.PortableUnixMailbox(fp, get_message)

    return iter(mbox)
 def get_message(obj):

    """Return an email Message object.
    The argument may be a Message object already, in which case it's
    returned as-is.
    If the argument is a string or file-like object (supports read()),
    the email package is used to create a Message object from it.  This
    can fail if the message is malformed.  In that case, the headers
    (everything through the first blank line) are thrown out, and the
    rest of the text is wrapped in a bare email.Message.Message.
    Note that we can't use our own message class here, because this
    function is imported by tokenizer, and our message class imports
    tokenizer, so we get a circular import problem.  In any case, this
    function does not need anything that our message class offers, so that
    shouldn't matter.
    """

    if isinstance(obj, email.Message.Message):

        return obj

    if hasattr(obj, "read"):

        obj = obj.read()

    try:

        msg = email.message_from_string(obj)

    except email.Errors.MessageParseError:

        headers = extract_headers(obj)

        obj = obj[len(headers):]

        msg = email.Message.Message()

        msg.set_payload(obj)

    return msg
 def as_string(msg, unixfrom=False):

    """Convert a Message object to a string in a safe-ish way.
    In email pkg version 2.5.4 and earlier, msg.as_string() can raise
    TypeError for some malformed messages.  This catches that and attempts
    to return something approximating the original message.
    To Do: This really should be done by subclassing email.Message.Message
    and making this function the as_string() method.  After 1.0.
    [Tony] Better: sb_filter & sb_mboxtrain should stop using this and
    start using the spambayes.Message classes.  They might need a little
    bit of rearranging, but that should work nicely, and mean that all
    this code is together in one place.
    """

    if isinstance(msg, str):

        return msg

    try:

        return msg.as_string(unixfrom)

    except TypeError:

        ty, val, tb = sys.exc_info()

        exclines = traceback.format_exception(ty, val, tb)[1:]

        excstr = "    ".join(exclines).strip()

        headers = []

        if unixfrom:

            headers.append(msg.get_unixfrom())

        for (hdr, val) in msg.items():

            headers.append("%s: %s" % (hdr, val))

        headers.append("X-Spambayes-Exception: %s" % excstr)

        parts = ["%s\n" % "\n".join(headers)]

        boundary = msg.get_boundary()

        for part in msg.get_payload():

            if boundary:

                parts.append(boundary)

            try:

                parts.append(part.as_string())

            except AttributeError:

                parts.append(str(part))

        if boundary:

            parts.append("--%s--" % boundary)

        return "\n".join(parts)+"\n"


header_break_re = re.compile(r"\r?\n(\r?\n)")

def extract_headers(text):

    """Very simple-minded header extraction:  prefix of text up to blank line.
    A blank line is recognized via two adjacent line-ending sequences, where
    a line-ending sequence is a newline optionally preceded by a carriage
    return.
    If no blank line is found, all of text is considered to be a potential
    header section.  If a blank line is found, the text up to (but not
    including) the blank line is considered to be a potential header section.
    The potential header section is returned, unless it doesn't contain a
    colon, in which case an empty string is returned.
    >>> extract_headers("abc")
    ''
    >>> extract_headers("abc\\n\\n\\n")  
    ''
    >>> extract_headers("abc: xyz\\n\\n\\n")
    'abc: xyz\\n'
    >>> extract_headers("abc: xyz\\r\\n\\r\\n\\r\\n")
    'abc: xyz\\r\\n'
    >>> extract_headers("a: b\\ngibberish\\n\\nmore gibberish")
    'a: b\\ngibberish\\n'
    """

    m = header_break_re.search(text)

    if m:

        eol = m.start(1)

        text = text[:eol]

    if ':' not in text:

        text = ""

    return text
 def _test():

    import doctest, mboxutils

    return doctest.testmod(mboxutils)
 if __name__ == "__main__":

    _test()

 if __name__ == "__main__":

    _test()



