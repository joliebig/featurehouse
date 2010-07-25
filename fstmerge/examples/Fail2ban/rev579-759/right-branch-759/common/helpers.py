__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:35 $"
__copyright__ = "Copyright (c) 2009 Cyril Jaquier"
__license__ = "GPL"
def formatExceptionInfo():
    """ Author: Arturo 'Buanzo' Busleiman """
    import sys
    cla, exc = sys.exc_info()[:2]
    excName = cla.__name__
    try:
        excArgs = exc.__dict__["args"]
    except KeyError:
        excArgs = str(exc)
    return (excName, excArgs)
