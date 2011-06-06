"""
Utility class to create (reasonably) unique identifiers
"""
import logging
import re
import time
from os.path import getmtime
log = logging.getLogger(__name__)
class UniqueIdGenerator(object):
    """
    Utility class to create (reasonably) unique identifiers
    """
    nextId = 1
    def __init__(self, packageName, exePath):
        """Initialize the generator"""
        self.prefix  = u"eXe" 
        self.prefix += re.sub(ur"\W", u"", packageName)[-10:]
        if exePath:
            self.prefix += u"%x" % int(getmtime(exePath))
    def generate(self):
        """
        Generate the next identifier
        Identifier is made up of
        "eXe" + last 10 alphanumeric characters of packageName +
         timestamp of eXe program + current timestamp + a sequential number 
        """
        uniqueId  = self.prefix
        uniqueId += u"%x" % int(time.time()*100)
        uniqueId += u"%x" % UniqueIdGenerator.nextId
        UniqueIdGenerator.nextId += 1
        return uniqueId
