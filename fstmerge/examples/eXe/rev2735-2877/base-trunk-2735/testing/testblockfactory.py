import unittest
from exe.webui.block        import Block
from exe.engine.idevice     import Idevice
from exe.webui.blockfactory import g_blockFactory
class DummyBlock(Block):
    def __init__(self, parent, idevice):
        pass
class DummyIdevice(Idevice):
    def __init__(self):
        pass
g_blockFactory.registerBlockType(DummyBlock, DummyIdevice)
class TestBlockFactory(unittest.TestCase):
    def setUp(self):
        pass
    def testBlockFactory(self):
        myidevice = DummyIdevice()
        myblock   = g_blockFactory.createBlock(None, myidevice)
        self.assertEquals(type(myblock), DummyBlock)
if __name__ == "__main__":
    unittest.main()
