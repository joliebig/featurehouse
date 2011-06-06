import sys
import os.path
import unittest
from exe.engine.uniqueidgenerator import UniqueIdGenerator
class TestUniqueId(unittest.TestCase):
    def setUp(self):
        self.generator = UniqueIdGenerator("David's Gen", sys.argv[0])
    def testGenerate(self):
        howMany = 10000
        values  = {}
        for x in range(howMany):
            id = self.generator.generate()
            self.assert_(id.isalnum())
            values[id] = 1
        self.assertEqual(howMany, len(values))
if __name__ == "__main__":
    unittest.main()
