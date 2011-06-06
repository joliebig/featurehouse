import unittest
from exe.engine.node    import Node
from exe.engine.idevice import Idevice
from exe.engine.packagestore import PackageStore
class TestIdevice(unittest.TestCase):
    def setUp(self):
        self.packageStore = PackageStore()
        self.package      = self.packageStore.createPackage()
    def testIdevice(self):
        myIdevice = Idevice("My Idevice", "UoA", "Testing", "Help tip", "icon")
        self.assertEquals(myIdevice.title, "My Idevice")
        self.assertEquals(myIdevice.author, "UoA")
        self.assertEquals(myIdevice.purpose, "Testing")
        self.assertEquals(myIdevice.tip, "Help tip")
    def testSetParentNode(self):
        parentNode = Node(self.package)
        idevice0 = Idevice("FirstIdevice", "", "", "", "")
        idevice0.setParentNode(parentNode)
        self.assert_(idevice0.parentNode is parentNode)
    def testIsfirstAndIsLast(self):
        parentNode = Node(self.package)
        idevice0 = Idevice("FirstIdevice", "", "", "", "")
        idevice0.setParentNode(parentNode)
        idevice1 = Idevice("SecondIdevice", "", "", "", "")
        idevice1.setParentNode(parentNode)
        idevice2 = Idevice("ThirdIdevice", "", "", "", "")
        idevice2.setParentNode(parentNode)
        self.assert_(idevice0.isFirst)
        self.assert_(idevice2.isLast)
    def testCmp(self):
        idevice0 = Idevice("FirstIdevice", "", "", "", "")
        idevice1 = Idevice("SecondIdevice", "", "", "", "")
        idevice2 = Idevice("ThirdIdevice", "", "", "", "")
        self.assertEquals(idevice2.__cmp__(idevice1), 1)
        self.assertEquals(idevice1.__cmp__(idevice0), 1)
        self.assertEquals(idevice0.__cmp__(idevice2), -1)
    def testDelete(self):
        parentNode = Node(self.package)
        idevice0 = Idevice("FirstIdevice", "", "", "", "")
        idevice0.setParentNode(parentNode)
        idevice1 = Idevice("SecondIdevice", "", "", "", "")
        idevice1.setParentNode(parentNode)
        idevice2 = Idevice("ThirdIdevice", "", "", "", "")
        idevice2.setParentNode(parentNode)
        idevice1.delete()
        if idevice1 in parentNode.idevices:
            print "delete failed"
    def testMove(self):
        parentNode = Node(self.package)
        idevice0 = Idevice("FirstIdevice", "", "", "", "")
        idevice0.setParentNode(parentNode)
        idevice1 = Idevice("SecondIdevice", "", "", "", "")
        idevice1.setParentNode(parentNode)
        idevice2 = Idevice("ThirdIdevice", "", "", "", "")
        idevice2.setParentNode(parentNode)
        idevice0.moveNext()
        self.assertEquals(parentNode.idevices[1], idevice0)
        self.assertEquals(parentNode.idevices[0], idevice1)
        idevice2.movePrev()
        self.assertEquals(parentNode.idevices[1], idevice2)
        self.assertEquals(parentNode.idevices[2], idevice0)
if __name__ == "__main__":
    unittest.main()
