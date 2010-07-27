"""Clean up an .mbox archive file.
The archiver looks for Unix-From lines separating messages in an mbox archive
file.  For compatibility, it specifically looks for lines that start with
"From " -- i.e. the letters capital-F, lowercase-r, o, m, space, ignoring
everything else on the line.
Normally, any lines that start "From " in the body of a message should be
escaped such that a > character is actually the first on a line.  It is
possible though that body lines are not actually escaped.  This script
attempts to fix these by doing a stricter test of the Unix-From lines.  Any
lines that start "From " but do not pass this stricter test are escaped with a
> character.
Usage: cleanarch [options] < inputfile > outputfile
Options:
    -s n
    --status=n
        Print a 
    -q / --quiet
        Don't print changed line information to standard error.
    -n / --dry-run
        Don't actually output anything.
    -h / --help
        Print this message and exit
"""
import sys
import re
import getopt
import mailbox
cre = re.compile(mailbox.UnixMailbox._fromlinepattern)
fre = re.compile(r'[\041-\071\073-\0176]+')

def usage(code, msg=''):
    print(__doc__, file=sys.stderr)
    if msg:
        print(msg, file=sys.stderr)
    sys.exit(code)

def escape_line(line, lineno, quiet, output):
    if output:
        sys.stdout.write('>' + line)
    if not quiet:
        print('[%d]' % lineno, line[:-1], file=sys.stderr)

def main():
    try:
        opts, args = getopt.getopt(
            sys.argv[1:], 'hqns:',
            ['help', 'quiet', 'dry-run', 'status='])
    except getopt.error as msg:
        usage(1, msg)
    quiet = 0
    output = 1
    status = -1
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            usage(0)
        elif opt in ('-q', '--quiet'):
            quiet = 1
        elif opt in ('-n', '--dry-run'):
            output = 0
        elif opt in ('-s', '--status'):
            try:
                status = int(arg)
            except ValueError:
                usage(1, 'Bad status number: %s' % arg)
    if args:
        usage(1)
    lineno = 0
    statuscnt = 0
    messages = 0
    while 1:
        lineno += 1
        line = sys.stdin.readline()
        if not line:
            break
        if line.startswith('From '):
            if cre.match(line):
                nextline = sys.stdin.readline()
                lineno += 1
                if not nextline:
                    escape_line(line, lineno, quiet, output)
                    break
                fieldname = nextline.split(':', 1)
                if len(fieldname) < 2 or not fre.match(nextline):
                    escape_line(line, lineno, quiet, output)
                    if output:
                        sys.stdout.write(nextline)
                else:
                    messages += 1
                    if output:
                        sys.stdout.write(line)
                        sys.stdout.write(nextline)
            else:
                escape_line(line, lineno, quiet, output)
        elif output:
            sys.stdout.write(line)
        if status > 0 and (lineno % status) == 0:
            sys.stderr.write('#')
            statuscnt += 1
            if statuscnt > 50:
                print(file=sys.stderr)
                statuscnt = 0
    print(messages, 'messages found', file=sys.stderr)

if __name__ == '__main__':
    main()
