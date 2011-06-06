"""
A Forum Idevice is one built up from forum imformation ans discussion.
"""
import copy
import logging
from exe.engine.idevice   import Idevice
from exe.engine.persist   import Persistable
from exe.engine.translate import lateTranslate
log = logging.getLogger(__name__)
class ForumIdevice(Idevice):
    """
    A Forum Idevice is one built up from forum imformation and discussions.
    """
    persistenceVersion = 3
    def __init__(self):
        Idevice.__init__(self, x_(u"Discussion Activity"), 
                         x_(u"University of Auckland"), 
                         "", "", u"discussion")
        self.forum               = Forum()
        self.discussion          = Discussion()
        self.isNewForum          = False
        self.noForum             = True
        self.isNewTopic          = False
        self.message             = ""
        self.refCount            = 1
        self.forumsCache         = None
        self.isAdded             = False
        self.emphasis            = Idevice.SomeEmphasis
        self.isForum             = True
    def clone(self):
        """
        Clone an iDevice and share one forums cache
        """
        newIdevice             = Idevice.clone(self)
        newIdevice.forumsCache = self.forumsCache
        return newIdevice
    def delete(self):
        """
        delete an iDevice from it's parentNode and forums cache
        """
        if self.isAdded:
            self.forum.deleteDiscussion(self.discussion)
            self.forumsCache.deleteForum(self.forum)
        Idevice.delete(self)
    def upgradeToVersion1(self):
        """
        Upgrades exe to v0.10
        """
        self._upgradeIdeviceToVersion1()
    def upgradeToVersion2(self):
        """
        Upgrades to v0.12
        """
        self._upgradeIdeviceToVersion2()
    def upgradeToVersion3(self):
        """
        Upgrades to v1.0
        """
        self.isForum = True
class Forum(Persistable):
    def __init__(self):
        """
        Initialize 
        """
        self.forumName            = ""
        self.lms                  = ""
        self.introduction         = ""
        self._nameInstruc         = x_(u"Type a forum name here")
        self.lms                  = Lms()
        self.lms.idevice          = self
        self.subjectInstruc       = ""
        self.messageInstruc       = ""
        self.discussions          = []
        self._lmsInstruc           = x_("Choose a LMS")
        self.refCount            = 1
    nameInstruc = lateTranslate('nameInstruc')
    lmsInstruc  = lateTranslate('lmsInstruc')
    def addDiscussion(self, discussion):
        """
        adds a new topic.  If the topic already exists increments
        a reference count
        """
        if discussion in self.discussions: 
            discussion.refCount += 1
        else:    
            self.discussions.append(discussion)
    def deleteDiscussion(self, discussion):
        """
        decrements the reference count on a discussion.  If the count
        is 0 delete the discussion
        """
        discussion.refCount -= 1
        if discussion.refCount == 0 and discussion in self.discussions:
            self.discussions.remove(discussion)
class Discussion(Persistable):
    def __init__(self):
        """
        Initialize 
        """
        self._name = x_(u"Discussion topic/Thread")
        self.isNone = True
        self.topic = ""
        self.intro = ""
        self._instruc = x_(u"Type a discussion topic here.")
        self.isAdded = False
        self.refCount = 1
    name    = lateTranslate('name')
    instruc = lateTranslate('instruc')
class Lms(Persistable):
    def __init__(self):
        """
        Initialize 
        """
        self._name = x_("Learning Management System")
        self.lms = ""        
        self.otherUrl = ""
        self._otherLabel = x_("Link to the forum")
        self.type                = "general"
        self.studentpost         = "2"
        self.subscription        = "0"
        self.groupmode           = "0"
        self.visible             = "1"
        self.typeInstruc         = ""
        self.introInstruc        = ""
        self.postInstruc         = ""
        self.subscInstruc        = ""
        self.groupInstruc        = ""
        self.visibleInstruc      = ""
    name       = lateTranslate('name')
    otherLabel = lateTranslate('otherLabel')
def register(ideviceStore):
    """Register with the ideviceStore"""
    from forumscache import ForumsCache
    forumsCache  = ForumsCache()
    forumIdevice = ForumIdevice()
    forumIdevice.forumsCache = forumsCache
    ideviceStore.extended.append(forumIdevice)
