"""
Twisted: The Framework Of Your Internet.
"""
import sys
if not hasattr(sys, "version_info") or sys.version_info < (2,3):
    raise RuntimeError("Twisted requires Python 2.3 or later.")
del sys
try:
    from zope.interface import Interface
    del Interface
except ImportError:
    raise ImportError, "you need zope.interface installed (http://zope.org/Products/ZopeInterface/)"
from twisted.python import compat
del compat
__version__ = '2.2.0'
