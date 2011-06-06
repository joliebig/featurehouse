"""
The ForumsCache is a cache of existing forum iDevices.
"""
from exe.engine.persist import Persistable
import logging
log = logging.getLogger(__name__)
class ForumsCache(Persistable):
    """
    The ForumsCache is a cache of existing forum iDevices.
    """
    def __init__(self):
        """
        Initialize
        """
        self.forums       = []
    def addForum(self, newForum):
        """
        adds a new forum.  If the forum already exists increments
        a reference count
        """
        if newForum in self.forums:
            newForum.refCount += 1
        else:
            self.forums.append(newForum)
    def deleteForum(self, forum):
        """
        decrements the reference count on a forum.  If the count
        is 0 delete the forum
        """
        forum.refCount -= 1
        if forum.refCount == 0 and forum in self.forums:
            self.forums.remove(forum)
    def getForums(self):
        """
        return a list of all the forums
        """
        return self.forums
