import unittest
from testconfig        import TestConfig
from testconfigparser  import TestConfigParser, TestSections
from testnode          import TestNode
from testuniqueid      import TestUniqueId
from testpackage       import TestPackage
from testidevice       import TestIdevice
from testidevicestore  import TestIdeviceStore
from testpersist       import TestPersist
from testresource      import TestResource
from testforumscache   import TestForumsCache
if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(TestConfig))
    suite.addTest(unittest.makeSuite(TestConfigParser))
    suite.addTest(unittest.makeSuite(TestSections))
    suite.addTest(unittest.makeSuite(TestNode))
    suite.addTest(unittest.makeSuite(TestUniqueId))
    suite.addTest(unittest.makeSuite(TestPackage))
    suite.addTest(unittest.makeSuite(TestIdevice))
    suite.addTest(unittest.makeSuite(TestIdeviceStore))
    suite.addTest(unittest.makeSuite(TestPersist))
    suite.addTest(unittest.makeSuite(TestResource))
    suite.addTest(unittest.makeSuite(TestForumsCache))
    unittest.TextTestRunner(verbosity=2).run(suite)
