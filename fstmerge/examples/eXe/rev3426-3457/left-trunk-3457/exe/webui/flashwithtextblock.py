"""
FlashWithTextBlock can render and process FlashWithTextIdevices as XHTML
"""
import logging
from exe.webui.block   import Block
from exe.webui.element import TextAreaElement, FlashElement
from exe.webui         import common
log = logging.getLogger(__name__)
class FlashWithTextBlock(Block):
    """
    FlashWithTextBlock can render and process FlashWithTextIdevices as XHTML
    """
    name = 'flashWithText'
    def __init__(self, parent, idevice):
        """
        Initialize
        """
        Block.__init__(self, parent, idevice)
        self.flashElement = FlashElement(idevice.flash)
        if idevice.text.idevice is None: 
            idevice.text.idevice = idevice
        self.textElement  = TextAreaElement(idevice.text)
    def process(self, request):
        """
        Process the request arguments from the web server to see if any
        apply to this block
        """
        log.debug("process " + repr(request.args))
        Block.process(self, request)
        if (u"action" not in request.args or
            request.args[u"action"][0] != u"delete"):
            self.flashElement.process(request)
            self.textElement.process(request)
        if "float"+self.id in request.args:
            self.idevice.float = request.args["float"+self.id][0]
        if "caption"+self.id in request.args:
            self.idevice.caption = request.args["caption"+self.id][0]
    def renderEdit(self, style):
        """
        Returns an XHTML string with the form elements for editing this block
        """
        log.debug("renderEdit")
        html  = u"<div class=\"iDevice\">\n"
        html += self.flashElement.renderEdit()  
        floatArr    = [[_(u'Left'), 'left'],
                      [_(u'Right'), 'right'],
                      [_(u'None'),  'none']]
        this_package = None
        if self.idevice is not None and self.idevice.parentNode is not None:
            this_package = self.idevice.parentNode.package
        html += common.formField('select', this_package, _("Align:"),
                                 "float" + self.id,
                                 options = floatArr,
                                 selection = self.idevice.float)
        html += u"\n"
        html += u"<b>%s </b>" % _(u"Caption:")
        html += common.textInput("caption" + self.id, self.idevice.caption)
        html += common.elementInstruc(self.idevice.captionInstruc)
        html += "<br/>" + self.textElement.renderEdit()
        html += self.renderEditButtons()
        html += u"</div>\n"
        return html
    def renderPreview(self, style):
        """
        Returns an XHTML string for previewing this block
        """
        log.debug("renderPreview")
        html  = u"\n<!-- flash with text iDevice -->\n"
        html  = u"<div class=\"iDevice "
        html += u"emphasis"+unicode(self.idevice.emphasis)+"\" "
        html += "ondblclick=\"submitLink('edit',"+self.id+", 0);\">\n"
        html += u"<div class=\"flash_text\" style=\""
        html += u"width:" + str(self.idevice.flash.width) + "px; "
        html += u"float:%s;\">\n" % self.idevice.float
        html += u"<div class=\"flash\">\n"
        html += self.flashElement.renderPreview()
        html += u"" + self.idevice.caption + "</div>"
        html += u"</div>\n"
        html += self.textElement.renderPreview()
        html += u"<br/>\n"        
        html += u"<div style=\"clear:both;\">"
        html += u"</div>\n"
        html += self.renderViewButtons()
        html += u"</div>\n"
        return html
    def renderView(self, style):
        """
        Returns an XHTML string for viewing this block
        """        
        log.debug("renderView")
        html  = u"\n<!-- Flash with text iDevice -->\n"
        html += u"<div class=\"iDevice "
        html += u"emphasis"+unicode(self.idevice.emphasis)+"\">\n"
        html += u"<div class=\"flash_text\" style=\""
        html += u"width:" + str(self.idevice.flash.width) + "px; "
        html += u"float:%s;\">\n" % self.idevice.float
        html += u"<div class=\"flash\">\n"
        html += self.flashElement.renderView()
        html += u"<br/>" + self.idevice.caption + "</div>"
        html += u"</div>\n"
        html += self.textElement.renderView()
        html += u"<div style=\"clear:both;\">"
        html += u"</div>\n"
        html += u"</div><br/>\n"
        return html
from exe.engine.flashwithtextidevice import FlashWithTextIdevice
from exe.webui.blockfactory          import g_blockFactory
g_blockFactory.registerBlockType(FlashWithTextBlock, FlashWithTextIdevice)    
