"""Script used by test_process.TestTwoProcesses"""
import sys
while 1:
    d = sys.stdin.read()
    if len(d) == 0:
        sys.exit(0)
