"""
A ImageWithText Idevice is one built up from an image and free text.
"""
import logging
from exe.engine.idevice   import Idevice
from exe.engine.field     import TextAreaField, ImageField
from exe.engine.translate import lateTranslate
log = logging.getLogger(__name__)
class ImageWithTextIdevice(Idevice):
    """
    A ImageWithText Idevice is one built up from an image and free text.
    """
    persistenceVersion = 6
    def __init__(self, defaultImage = None):
        Idevice.__init__(self, 
                         x_(u"Image with Text"), 
                         x_(u"University of Auckland"), 
                         x_(u"""<p>
The image with text iDevice can be used in a number of ways to support both
the emotional (affective) and learning task (cognitive) dimensions of eXe
content. 
</p><p>
<b>Integrating visuals with verbal summaries</b>
</p><p>
Cognitive psychologists indicate that presenting learners with a
representative image and corresponding verbal summary (that is presented
simultaneously) can reduce cognitive load and enhance learning retention.
This iDevice can be used to present an image (photograph, diagram or
graphic) with a brief verbal summary covering the main points relating to
the image. For example, if you were teaching the functions of a four-stroke
combustion engine, you could have a visual for each of the four positions of
the piston with a brief textual summary of the key aspects of each visual.
</p>"""), u"", u"")
        self.emphasis           = Idevice.NoEmphasis
        self.image              = ImageField(x_(u"Image"), u"")
        self.image.idevice      = self
        self.image.defaultImage = defaultImage
        self.text               = TextAreaField(x_(u"Text"),
                                                x_("""Enter the text you wish to 
                                                associate with the image."""))
        self.text.idevice       = self
        self.float              = u"left"
        self.caption            = u""
        self._captionInstruc    = x_(u"""Provide a caption for the image 
you have just inserted.""")
    captionInstruc = lateTranslate('captionInstruc')
    def upgradeToVersion1(self):
        """
        Called to upgrade from 0.5 release
        """
        self.float = u"left"
    def upgradeToVersion2(self):
        """
        Called to upgrade from 0.6 release
        """
        self.caption  = u""
        self.emphasis = Idevice.NoEmphasis
    def upgradeToVersion3(self):
        """
        Upgrades v0.6 to v0.7.
        """
        self.lastIdevice = False
    def upgradeToVersion4(self):
        """
        Upgrades to exe v0.10
        """
        self._upgradeIdeviceToVersion1()
    def upgradeToVersion5(self):
        """
        Upgrades to v0.12
        """
        log.debug("upgrade to version 5")
        self._upgradeIdeviceToVersion2()        
        self.image._upgradeFieldToVersion2()
    def upgradeToVersion6(self):
        """
        Called to upgrade from 0.13 release
        """
        self._captionInstruc  = x_(u"""Provide a caption for the image 
you have just inserted.""")
