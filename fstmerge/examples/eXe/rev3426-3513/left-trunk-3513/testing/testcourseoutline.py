import unittest
from exe.engine.packagestore import PackageStore, g_packageStore
from exe.engine.package      import Package
from exe.engine.node         import Node
from exe.webui.outlinePaneForTest import OutlinePaneForTest
class TestCourseOutLine(unittest.TestCase):
    def setUp(self):
        pass
    def testOutline(self):
        root = Node()
        root.title = "level 1 root"
        child1 = root.createChild()
        child1.title = "level 2 child 1"
        grandChild11 = child1.createChild()
        grandChild11.title = "level 3 child 11"
        grandChild12 = child1.createChild()
        grandChild12.title = "level 3 child 12"
        child2 = root.createChild()
        child2.title = "level 2 child 2"
        grandChild21 = child2.createChild()
        grandChild21.title = "level 3 child 21"
        grandChild22 = child2.createChild()
        grandChild22.title = "level 3 child 22"
        child3 = root.createChild()
        child3.title = "level 2 child 3"
        child4 = root.createChild()
        child4.title = "level 2 child 4"
        outlinePane = OutlinePaneForTest(root)
        print outlinePane.render()
if __name__ == "__main__":
    unittest.main()
