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
    persistenceVersion = 4
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
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for this iDevice:
        """
        if hasattr(self, 'flash') and hasattr(self.flash, 'flashResource'):
            if this_resource == self.flash.flashResource:
                return self.flash
        if hasattr(self, 'text') and hasattr(self.text, 'images'):
            for this_image in self.text.images:
                if hasattr(this_image, '_imageResource') \
                and this_resource == this_image._imageResource:
                    return self.text
        return None
    def getRichTextFields(self):
        """
        Like getResourcesField(), a general helper to allow nodes to search 
        through all of their fields without having to know the specifics of each
        iDevice type.  
        """
        fields_list = []
        if hasattr(self, 'text'):
            fields_list.append(self.text)
        return fields_list
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
    def NoLonger_upgradeToVersion4(self):
        """
        NOTE: upgradeToVersion4 (from r3051) no longer enabled, because:
        FlashWithTextIdevice is now deprecated, leaving its functionality
        fully in place, but hidden from standard usage.  Before doing the
        deprecation, however, there had previously been a very brief period
        of converting FlashWithTextIdevice -> FreeTextIdevice,
        since FreeText can hold embeddded .swf Flash Object media. It was
        decided, though, that the behaviour was just different enough that
        FlashWithTextIdevice should still be available, if needed.
        Leaving the persistence version to 4, for those few intermediate elps.
        """         
        G.application.afterUpgradeHandlers.append(self.convertToFreeText)
