"""
A FlashWithText Idevice is one built up from a flash file and free text.
"""
import logging
from exe.engine.idevice   import Idevice
from exe.engine.field     import TextAreaField, FlashField
from exe.engine.translate import lateTranslate
log = logging.getLogger(__name__)
class FlashWithTextIdevice(Idevice):
    """
    A FlashWithText Idevice is one built up from a flash file and free text.
    """
    persistenceVersion = 3
    def __init__(self):
        Idevice.__init__(self, x_(u"Flash with Text"), 
                         x_(u"University of Auckland"), 
                         x_(u"""The flash with text idevice allows you to 
associate additional textual information to a flash file. This may be useful 
where you wish to provide educational instruction regarding the flash file 
the learners will view."""), u"", u"")
        self.emphasis          = Idevice.NoEmphasis
        self.flash             = FlashField(x_(u"Flash with Text"), u"")
        self.flash.idevice     = self
        self.text              = TextAreaField(x_(u"Description"),
                                 x_("""Enter the text you wish to 
                                 associate with the image."""))
        self.text.idevice      = self
        self.float             = u"left"
        self.caption           = u""
        self._captionInstruc   = x_(u"""Provide a caption for the flash you 
                                  have just inserted.""")
        self._dimensionInstruc = x_(u"""Enter the flash display 
dimensions (in pixels) and determine the alignment of the image on screen. 
The width and height dimensions will alter proportionally.""")
    captionInstruc   = lateTranslate('captionInstruc')
    dimensionInstruc = lateTranslate('dimensionInstruc')
    def upgradeToVersion1(self):
        """
        Upgrades exe to v0.10
        """
        self._upgradeIdeviceToVersion1()
    def upgradeToVersion2(self):
        """
        Upgrades to v0.12
        """
        self._upgradeIdeviceToVersion2()
        self.flash._upgradeFieldToVersion2()
    def upgradeToVersion3(self):
        """
        Upgrades to v0.13
        """
        self._captionInstruc   = x_(u"""Provide a caption for the flash you 
                                  have just inserted.""")
        self._dimensionInstruc = x_(u"""Enter the flash display 
dimensions (in pixels) and determine the alignment of the image on screen. 
The width and height dimensions will alter proportionally.""")
        self.flash._upgradeFieldToVersion3()
