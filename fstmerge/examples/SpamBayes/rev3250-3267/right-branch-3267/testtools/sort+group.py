"""Usage: sort+group.py [options]
Where:
    -h
        Show usage and exit.
    -q
        Suppress verbose output.
    -a
        Run through all directories in the directories that the
        ham_directories and spam_directories are in.  This is
        similar (identical with default ham/spam directories)
        to the 1.0.x sort+group.py behaviour.
    -o section:option:value
        set [section, option] in the options database to value.
Sort and group the messages in the Data hierarchy.
Run this prior to mksets.py for setting stuff up for testing of
chronological incremental training.
"""
import sys
import os
import glob
import time
import getopt
from email.Utils import parsedate_tz, mktime_tz
from spambayes.Options import options
SECONDS_PER_DAY = 24 * 60 * 60
def get_time(fpath):
    fh = file(fpath, 'rb')
    lines = iter(fh)
    for line in lines:
        if line.lower().startswith("received:"):
            break
    else:
        print("\nNo Received header found.")
        fh.close()
        return None
    received = line
    for line in lines:
        if line[0] in ' \t':
            received += line
        else:
            break
    fh.close()
    i = received.rfind(';')
    if i < 0:
        print("\n" + received)
        print("No semicolon found in Received header.")
        return None
    datestring = received[i+1:]
    datestring = ' '.join(datestring.split())
    as_tuple = parsedate_tz(datestring)
    if as_tuple is None:
        print("\n" + received)
        print("Couldn't parse the date: %r" % datestring)
        return None
    return mktime_tz(as_tuple)
def usage(code, msg=''):
    """Print usage message and sys.exit(code)."""
    if msg:
        print(msg, file=sys.stderr)
        print(file=sys.stderr)
    print(__doc__ % globals(), file=sys.stderr)
    sys.exit(code)
def main():
    """Main program; parse options and go."""
    from os.path import join, split
    import getopt
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hqao:', ['option='])
    except getopt.error as msg:
        usage(1, msg)
    loud = True
    all_data = False
    for opt, arg in opts:
        if opt == '-h':
            usage(0)
        elif opt == '-q':
            loud = False
        elif opt == '-a':
            all_data = True
        elif opt in ('-o', '--option'):
            options.set_from_cmdline(arg, sys.stderr)
    data = []   
    if loud:
        print("Scanning everything")
    now = time.time()
    hdir = os.path.dirname(options["TestDriver", "ham_directories"])
    sdir = os.path.dirname(options["TestDriver", "spam_directories"])
    if all_data:
        hdir = os.path.dirname(hdir)
        sdir = os.path.dirname(sdir)
        files = glob.glob(os.path.join(hdir, "*", "*", "*"))
        if sdir != hdir:
            files.extend(glob.glob(os.path.join(sdir, "*", "*", "*")))
    else:
        files = glob.glob(os.path.join(hdir, "*", "*"))
        files.extend(glob.glob(os.path.join(sdir, "*", "*")))
    for name in files:
        if loud:
            sys.stdout.write("%-78s\r" % name)
            sys.stdout.flush()
        when_received = get_time(name) or now
        data.append((when_received,) + split(name))
    if loud:
        print("")
        print("Sorting ...")
    data.sort()
    if loud:
        print("Renaming first pass ...")
    for dummy, dirname, basename in data:
        os.rename(join(dirname, basename),
                  join(dirname, "-" + basename))
    if loud:
        print("Renaming second pass ...")
    earliest = data[0][0]  
    i = 0
    for when_received, dirname, basename in data:
        extension = os.path.splitext(basename)[-1]
        group = int((when_received - earliest) / SECONDS_PER_DAY)
        newbasename = "%04d-%06d" % (group, i)
        os.rename(join(dirname, "-" + basename),
                  join(dirname, newbasename + extension))
        i += 1
if __name__ == "__main__":
    main()
