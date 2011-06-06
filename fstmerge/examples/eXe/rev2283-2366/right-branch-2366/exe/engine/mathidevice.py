"""
MathIdevice: just has a block of text
"""
import logging
from exe.engine.idevice import Idevice
from exe.engine.field   import MathField
log = logging.getLogger(__name__)
class MathIdevice(Idevice):
    """
    MathIdevice: just has a block of text
    """
    def __init__(self, instruc="", latex=""):
        Idevice.__init__(self, x_(u"Maths"), 
                         x_(u"University of Auckland"), 
                         x_("""The mathematical language LATEX has been 
                        used to enable your to insert mathematical formula 
                        into your content. It does this by translating 
                        LATEX into an image which is then displayed
                         within your eXe content. We would recommend that 
                        you use the Free Text iDevice to provide 
                        explanatory notes and learning instruction around 
                        this graphic."""),
                        "", 
                        "")
        self.emphasis = Idevice.NoEmphasis
        self.content  = MathField(x_(u"Maths"), 
                                      x_(u"""You can use the toolbar or enter latex manually into the textarea. """))
        self.content.idevice = self
