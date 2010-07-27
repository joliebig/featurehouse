"""Convert Apple Mail's emlx files to plain text files usable
by SpamBayes's testtools.
Usage: %(program)s [options] path
Where:
    -h
        Show usage and exit.
    -e mailbox
        Name of mailbox or account to exclude.  Drafts and
        Sent Messages are always excluded.
    -q
        Don't print status indicators.
    -o section:option:value
        Set [section, option] in the options database to value.
"path" should be the path to the user's ~/Library/Mail directory.
(Tested on Windows XP remotely accessing the Mac filesystem.
I don't know if the bundling of the files in the Mail directory
would effect this script or not, and can't be bothered finding
out right now).
"""
import os
import sys
import getopt
from spambayes.Options import options
def usage(code, msg=''):
    """Print usage message and sys.exit(code)."""
    if msg:
        print(msg, file=sys.stderr)
        print(file=sys.stderr)
    print(__doc__ % globals(), file=sys.stderr)
    sys.exit(code)
def emlx_to_rfc2822(in_fn, out_fn):
    """Convert an individual file in Apple Mail's emlx format
    to a file with just the RFC2822 message.
    The emlx format is simply the length of the message (as a
    string) on the first line, then the raw message text, then
    the contents of a plist (XML) file that contains data that
    Mail uses (subject, flags, sender, and so forth).  We ignore
    this plist data).
    """
    fin = file(in_fn)
    fout = file(out_fn, "w")
    length = int(fin.readline().rstrip())
    fout.write(fin.read(length))
    plist = fin.read()
def export(mail_dir, exclude, quiet):
    """Scans through the specified directory, which should be
    the Apple Mail user's ~\Library\Mail folder, converting
    all found emlx files to simple RFC2822 messages suitable
    for use with the SpamBayes testtools.
    Messages are copied (the originals are left untouched) into
    the standard SpamBayes testtools setup (all files are put in the
    reservoir; use rebal.py to distribute).
    The script assumes that all messages outside of Mail's
    Junk folder are ham, and all messages inside the Junk folder
    are spam.
    Any messages in the "Sent Messages" folders are skipped.
    A simple extension of this function would allow only certain
    accounts/mailboxes to be exported.
    """
    for dirname in os.listdir(mail_dir):
        dirname = os.path.join(mail_dir, dirname)
        if os.path.isdir(dirname):
            export_directory(mail_dir, dirname, exclude, quiet)
def export_directory(parent, dirname, exclude, quiet):
    base_parent = os.path.splitext(os.path.basename(parent))[0]
    if base_parent == "Junk":
        dest_dir = os.path.join(\
            os.path.dirname(options["TestDriver", "spam_directories"]),
            "reservoir")
    elif base_parent in exclude:
        return
    else:
        dest_dir = os.path.join(\
            os.path.dirname(options["TestDriver", "ham_directories"]),
            "reservoir")
    dest_dir = os.path.normpath(dest_dir)
    for path in os.listdir(dirname):
        path = os.path.join(dirname, path)
        if os.path.isdir(path):
            export_directory(dirname, path, exclude, quiet)
        else:
            fn, ext = os.path.splitext(path)
            if ext == ".emlx":
                in_fn = os.path.join(dirname, path)
                out_fn = os.path.join(dest_dir,
                                      os.path.basename(fn) + ".txt")
                emlx_to_rfc2822(in_fn, out_fn)
                if not quiet:
                    sys.stdout.write('.')
    if not quiet:
        print()
def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hqe:o:')
    except getopt.error as msg:
        usage(1, msg)
    quiet = False
    exclude = ["Sent Messages", "Drafts"]
    for opt, arg in opts:
        if opt == '-h':
            usage(0)
        elif opt == '-e':
            exclude.append(arg)
        elif opt == '-q':
            quiet = True
        elif opt in ('-o', '--option'):
            options.set_from_cmdline(arg, sys.stderr)
    if len(args) != 1:
        usage(1, "Must specify exactly one path.")
    export(args[0], exclude, quiet)
if __name__ == "__main__":
    main()
