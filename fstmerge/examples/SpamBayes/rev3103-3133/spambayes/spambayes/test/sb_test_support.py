import sys, os

import unittest

def fix_sys_path():

    """Fix sys.path so that the core SpamBayes package,
    *and* the SpamBayes scripts can be imported.
    """

    this_dir = os.path.dirname(__file__)

    try:

        import spambayes.Version

    except ImportError:

        sb_dir = os.path.abspath(
                     os.path.join(this_dir, "..", ".."))

        sys.path.insert(0, sb_dir)

        import spambayes.Version

    try:

        import sb_server

    except ImportError:

        script_dir = os.path.abspath(
                     os.path.join(this_dir, "..", "..", "scripts"))

        sys.path.insert(0, script_dir)

        import sb_server
 def unittest_main(*args, **kwargs):

    unittest.main(*args, **kwargs)


