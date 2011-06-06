"""
RawTextIdevice: just has a block of raw html code
"""
import logging
from exe.engine.idevice import Idevice
from exe.engine.field   import TextField
log = logging.getLogger(__name__)
class RawIdevice(Idevice):
    """
    RawTextIdevice: just has a block of raw html code
    """
    def __init__(self, content=""):
        Idevice.__init__(self, x_(u"Raw iDevice"), 
                         x_(u"Auckland University of Technology"), 
                         "", "", "")
        self.emphasis = Idevice.NoEmphasis
        self.content  = TextField(x_(u"Raw Text"), 
                                      "",
                                      content)
        self.content.idevice = self
        self.icon            = u"inter"
        if content:
            self.edit = False
