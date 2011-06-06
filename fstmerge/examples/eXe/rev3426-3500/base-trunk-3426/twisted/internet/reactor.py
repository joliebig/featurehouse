"""
See twisted.internet.interfaces.IReactor*.
"""
import sys
del sys.modules['twisted.internet.reactor']
from twisted.internet import selectreactor
selectreactor.install()
