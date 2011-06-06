"""
ExampleBlock can render and process ExampleIdevices as XHTML
"""
import logging
from exe.webui.block            import Block
from exe.webui.element          import TextAreaElement
log = logging.getLogger(__name__)
class ExampleBlock(Block):
    """
    ExampleBlock can render and process ExampleIdevices as XHTML
    GenericBlock will replace it..... one day
    """
    def __init__(self, parent, idevice):
        Block.__init__(self, parent, idevice)
        self.contentElement = TextAreaElement(idevice.content)
        self.contentElement.height = 250
    def process(self, request):
        """
        Process the request arguments from the web server to see if any
        apply to this block
        """
        Block.process(self, request)
        content = self.contentElement.process(request)
        if content:
            self.idevice.content = content
    def renderEdit(self, style):
        """
        Returns an XHTML string with the form element for editing this block
        """
        html  = u"<div>\n"
        html += self.contentElement.renderEdit()
        html += self.renderEditButtons()
        html += u"</div>\n"
        return html
    def renderPreview(self, style):
        """
        Returns an XHTML string for previewing this block
        """
        html  = u"<div class=\"iDevice "
        html += u"emphasis"+unicode(self.idevice.emphasis)+"\" "
        html += u"ondblclick=\"submitLink('edit',"+self.id+", 0);\">\n"
        html += self.contentElement.renderView()
        html += self.renderViewButtons()
        html += "</div>\n"
        return html
    def renderView(self, style):
        """
        Returns an XHTML string for viewing this block
        """
        html  = u"<div class=\"iDevice "
        html += u"emphasis"+unicode(self.idevice.emphasis)+"\">\n"
        html += self.contentElement.renderView()
        html += u"</div>\n"
        return html
def register():
    """Register this block with the BlockFactory"""
    from exampleidevice import ExampleIdevice
    from exe.webui.blockfactory     import g_blockFactory
    g_blockFactory.registerBlockType(ExampleBlock, ExampleIdevice)    
