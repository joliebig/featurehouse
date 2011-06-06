from exe.engine.idevice      import Idevice
from exe.engine.idevicestore import IdeviceStore
from exe.engine.path         import Path
import unittest
import os
import utils
class TestIdeviceStore(utils.SuperTestCase):
    def testLoad(self):
        """
        Tests that idevices can be loaded
        """
        store = IdeviceStore(self.app.config)
        store.load()
        self.assert_(os.path.exists("tmp/idevices/generic.data"))
        os.remove("tmp/idevices/generic.data")
if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(TestIdeviceStore))
    unittest.TextTestRunner(verbosity=2).run(suite)
