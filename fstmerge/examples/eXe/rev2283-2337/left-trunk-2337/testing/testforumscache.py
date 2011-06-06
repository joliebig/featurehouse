import unittest
from exe.engine.forumscache    import ForumsCache
from exe.engine.forumidevice   import ForumIdevice
class TestForumsCache(unittest.TestCase):
    def setUp(self):
        self.forumsCache = ForumsCache()
    def testAddForum(self):
        forum1 = ForumIdevice()
        forum1.forumName = "My first forum"
        forum2 = ForumIdevice()
        forum2.forumName = "My second forum"
        forum3 = ForumIdevice()
        forum3.forumName = "My third forum"
        forum1.forumsCache = self.forumsCache
        forum2.forumsCache = self.forumsCache
        forum3.forumsCache = self.forumsCache
        self.forumsCache.addForum(forum1)
        self.forumsCache.addForum(forum2)
        self.forumsCache.addForum(forum3)
        self.forumsCache.addForum(forum1)
        forums = self.forumsCache.forums
        self.assertEquals(len(forums), 3)
        for forum in forums:
            if forum.forumName == "My first forum":
                self.assertEquals(forum.refCount, 2)
            if forum.forumName == "My second forum":
                self.assertEquals(forum.refCount, 1)
    def testGetForums(self):
        forum1 = ForumIdevice()
        forum1.forumName = "My first forum"
        forum2 = ForumIdevice()
        forum2.forumName = "My second forum"
        forum3 = ForumIdevice()
        forum3.forumName = "My third forum"
        forum4 = ForumIdevice()
        forum4.forumName = "My 4th forum"
        forum1.forumsCache = self.forumsCache
        forum2.forumsCache = self.forumsCache
        forum3.forumsCache = self.forumsCache
        forum4.forumsCache = self.forumsCache
        self.forumsCache.addForum(forum1)
        self.forumsCache.addForum(forum2)
        self.forumsCache.addForum(forum3)
        self.forumsCache.addForum(forum4)
        self.forumsCache.addForum(forum1)
        forums = self.forumsCache.getForums()
        self.assertEquals(len(forums), 4)
        forums = forum1.forumsCache.getForums()
        self.assertEquals(len(forums), 4)
    def testDeleteForum(self):
        forum1 = ForumIdevice()
        forum1.forumName = "My first forum"
        forum2 = ForumIdevice()
        forum2.forumName = "My second forum"
        forum3 = ForumIdevice()
        forum3.forumName = "My third forum"
        forum4 = ForumIdevice()
        forum4.forumName = "My first forum"
        forum1.forumsCache = self.forumsCache
        forum2.forumsCache = self.forumsCache
        forum3.forumsCache = self.forumsCache
        forum4.forumsCache = self.forumsCache
        self.forumsCache.addForum(forum1)
        self.forumsCache.addForum(forum2)
        self.forumsCache.addForum(forum3)
        self.forumsCache.addForum(forum4)
        forums = self.forumsCache.getForums()
        for forum in forums:
            if forum.forumName == "My first forum":
                self.forumsCache.deleteForum(forum)
                break                
        self.assertEquals(len(forums), 3)
        for forum in forums:
            if forum.forumName == "My first forum":
                self.assertEquals(forum.refCount, 1)
                break
        for forum in forums:
            if forum.forumName == "My third forum":
                self.forumsCache.deleteForum(forum)
                break        
        self.assertEquals(len(forums), 2)
if __name__ == "__main__":
    unittest.main()
