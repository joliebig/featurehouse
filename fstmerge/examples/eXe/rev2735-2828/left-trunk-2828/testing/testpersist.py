from exe.engine import persist 
import unittest
class Foo:
    def __init__(self):
        self.number  = 99
        self.places  = ["Sandringham", "Roskill", "Onehunga"]
        self.ohYeah  = True
    def __eq__(self, other):
        return (self.number == other.number and 
                self.places == other.places and
                self.ohYeah == other.ohYeah)
class TestPersist(unittest.TestCase):
    def setUp(self):
        pass
    def testPersist(self):
        toEncode = Foo()
        encoded  = persist.encodeObject(toEncode)
        decoded  = persist.decodeObject(encoded)
        self.assertEqual(toEncode, decoded)
if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(TestPersist))
    unittest.TextTestRunner(verbosity=2).run(suite)
