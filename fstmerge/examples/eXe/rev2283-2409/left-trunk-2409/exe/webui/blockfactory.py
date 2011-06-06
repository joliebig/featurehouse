"""
BlockFactory is responsible for creating the right block object to match
a given Idevice.
"""
import logging
log = logging.getLogger(__name__)
class BlockFactory(object):
    """
    BlockFactory is responsible for creating the right block object to match
    a given Idevice.  Blocks register themselves with the factory, specifying
    which Idevices they can render
    """
    def __init__(self):
        """Initialize BlockFactory"""
        self.blockTypes = []
    def registerBlockType(self, blockType, ideviceType):
        """
        Block classes call this function when they are imported
        """
        log.debug(u"registerBlockType "+ 
                  blockType.__name__ + u"<=>" +  ideviceType.__name__)
        self.blockTypes.append((blockType, ideviceType))
    def createBlock(self, parent, idevice):
        """
        Returns a Block object which can render this Idevice
        """
        for blockType, ideviceType in self.blockTypes:
            if isinstance(idevice, ideviceType):
                log.info(u"createBlock "+blockType.__name__+u" for "+
                          idevice.__class__.__name__)
                return blockType(parent, idevice)
        log.error(u"No blocktype registered for "+ idevice.__class__.__name__)
        return None
g_blockFactory = BlockFactory()
