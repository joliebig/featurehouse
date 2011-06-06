import unittest
from os.path                   import join
from exe.engine.package        import Package
from exe.engine.resource       import Resource
from exe.engine.config         import Config
from exe.engine.packagestore   import PackageStore
from exe.engine.node           import Node
from exe.engine.genericidevice import GenericIdevice
from exe.engine.path           import Path
class TestResource(unittest.TestCase):
    def setUp(self):
        self.packageStore = PackageStore()
        self.package      = self.packageStore.createPackage()
    def testCreateAndDelete(self):
        """
        Test we have a resource directory and resource files can be stored in
        """
        oliver = Resource(self.package, Path("oliver.jpg"))
        self.assert_((self.package.resourceDir/"oliver.jpg").exists())
        oliver.delete()
        self.assert_(not (self.package.resourceDir/"oliver.jpg").exists())
    def testSaveAndLoad(self):
        pass
if __name__ == "__main__":
    unittest.main()
