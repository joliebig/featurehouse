"""
This is an example of a user created iDevice plugin.  If it is copied
into the user's ~/.exe/idevices dircectory it will be loaded along with
the system idevices.
"""
import logging
from exe.engine.idevice import Idevice
from exe.engine.field   import TextAreaField
log = logging.getLogger(__name__)
class ExampleIdevice(Idevice):
    """
    This is an example of a user created iDevice plugin.  If it is copied
    into the user's ~/.exe/idevices dircectory it will be loaded along with
    the system idevices.
    """
    def __init__(self, content=""):
        Idevice.__init__(self, _(u"Example Example"), 
                         _(u"University of Auckland"), 
                         _(u"""This is an example of a user created
iDevice plugin."""), "", "")
        self.emphasis = Idevice.NoEmphasis
        self.content  = TextAreaField(_(u"Example"), 
                                      _(u"This is a free text field."), 
                                      content)
        self.content.idevice = self
def register(ideviceStore):
    """Register with the ideviceStore"""
    ideviceStore.extended.append(ExampleIdevice())
