"""Test program for processes."""
import sys, os
test_file_match = "process_test.log.*"
test_file = "process_test.log.%d" % os.getpid()
def main():
    f = open(test_file, 'wb')
    bytes = sys.stdin.read(4)
    f.write("one: %r\n" % bytes)
    sys.stdout.write(bytes)
    sys.stdout.flush()
    os.close(sys.stdout.fileno())
    bytes = sys.stdin.read(4)
    f.write("two: %r\n" % bytes)
    sys.stderr.write(bytes)
    sys.stderr.flush()
    os.close(sys.stderr.fileno())
    bytes = sys.stdin.read(4)
    f.write("three: %r\n" % bytes)
    sys.exit(23)
if __name__ == '__main__':
    main()
