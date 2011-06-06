"""
ElementFactory is responsible for creating the right element object to match
a given field.
"""
import logging
from exe.engine.field  import TextField, TextAreaField, ImageField, FeedbackField
from exe.webui.element import TextElement, TextAreaElement, ImageElement
from exe.webui.element import FeedbackElement
log = logging.getLogger(__name__)
class ElementFactory(object):
    """
    ElementFactory is responsible for creating the right element object to match
    a given field.  Elements register themselves with the factory, specifying
    which fields they can render
    """
    def __init__(self):
        """
        Initialize
        """
        self.elementTypeMap = {TextField:      TextElement,
                               TextAreaField:  TextAreaElement,
                               ImageField:     ImageElement,
                               FeedbackField:  FeedbackElement}
    def createElement(self, field):
        """
        Returns a Element object which can render this field
        """
        elementType = self.elementTypeMap.get(field.__class__)
        if elementType:
            log.debug(u"createElement "+elementType.__class__.__name__+
                      u" for "+field.__class__.__name__)
            return elementType(field)
        else:
            log.error(u"No element type registered for " +
                      field.__class__.__name__)
            return None
g_elementFactory = ElementFactory()
