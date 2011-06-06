"""Module to help _buildbot_*.py scripts."""
import shutil, os, sys
from subprocess import Popen, PIPE, STDOUT
def check_call(args,**kwargs):
    p = Popen(args, shell=True,
              close_fds=(sys.platform!='win32'),**kwargs)
    p.wait()
    if p.returncode!=0:
        raise RuntimeError('returncode is %s'%p.returncode)
