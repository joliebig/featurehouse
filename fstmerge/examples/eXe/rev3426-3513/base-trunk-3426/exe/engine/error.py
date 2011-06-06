"""
This module contains exception classes used for eXe specific errors
"""
import logging
log = logging.getLogger(__name__)
class Error(Exception):
    """
    Exception class used for eXe specific errors
    """
    def __init__(self, value):
        """
        Initialize 
        """
        Exception.__init__(self)
        self.value = value
        log.debug("init", self.value)
    def __str__(self):
        """
        return the error string
        """
        return repr(self.value)
