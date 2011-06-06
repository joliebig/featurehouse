from twisted.trial import unittest
from twisted.python import failure
class Assertions(unittest.TestCase):
    def testExceptions(self):
        exc = self.assertRaises(ZeroDivisionError, lambda: 1/0)
        assert isinstance(exc, ZeroDivisionError), "ZeroDivisionError instance not returned"
        for func in [lambda: 1/0, lambda: None]:
            try:
                self.assertRaises(ValueError, func)
            except unittest.FailTest:
                pass
            except:
                raise unittest.FailTest("FailTest not raised", failure.Failure().getTraceback())
            else:
                raise unittest.FailTest("FailTest not raised")
