"""
A FlashWithText Idevice is one built up from a flash file and free text.
"""
import logging
from exe.engine.idevice   import Idevice
from exe.engine.field     import TextAreaField, FlashMovieField
from exe.engine.translate import lateTranslate
log = logging.getLogger(__name__)
class FlashMovieIdevice(Idevice):
    """
    A FlashMovie Idevice is one built up from a flash file and free text.
    """
    persistenceVersion = 3
    def __init__(self):
        Idevice.__init__(self, x_(u"Flash Movie"), 
                         x_(u"University of Auckland"), 
                         x_(u"""\
This iDevice only supports the Flash Video File (.FLV) format, and will not
accept other video formats. You can however convert other movie formats
(e.g. mov, wmf etc) into the .FLV format using third party encoders. These
are not supplied with eXe. Users will also need to download the Flash 8
player from http://www.macromedia.com/ to play the video."""),
                         u"",
                         u"")
        self.emphasis         = Idevice.NoEmphasis
        self.flash            = FlashMovieField(x_(u"Flash Movie")) 
        self.flash.idevice    = self
        self.text             = TextAreaField(x_(u"Description"), x_("""Enter 
the text you wish to associate with the file."""))
        self.text.idevice     = self
        self.float            = u"left"
        self.caption          = ""
        self._captionInstruc  = x_(u"""Provide a caption for the flash movie 
you have just inserted.""")
        self.systemResources += ['videoContainer.swf']
    captionInstruc = lateTranslate('captionInstruc')
    def upgradeToVersion2(self):
        """
        Upgrades to v0.12
        """
        self._upgradeIdeviceToVersion2()
        self.flash._upgradeFieldToVersion2()
        self.systemResources += ['videoContainer.swf']
    def upgradeToVersion3(self):
        """
        Upgrades to v0.14
        """
        self._captionInstruc  = x_(u"""Provide a caption for the flash movie 
you have just inserted.""")
        self.flash._upgradeFieldToVersion3()
def register(ideviceStore):
    """Register with the ideviceStore"""
    ideviceStore.extended.append(FlashMovieIdevice())
