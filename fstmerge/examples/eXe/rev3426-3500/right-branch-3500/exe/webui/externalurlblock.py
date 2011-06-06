"""
ExternalUrlBlock can render and process ExternalUrlIdevices as XHTML
"""
import logging
from exe.webui.block            import Block
from exe.webui                  import common
log = logging.getLogger(__name__)
class ExternalUrlBlock(Block):
    """
    ExternalUrlBlock can render and process ExternalUrlIdevices as XHTML
    """
    def __init__(self, parent, idevice):
        Block.__init__(self, parent, idevice)
    def process(self, request):
        """
        Process the request arguments from the web server to see if any
        apply to this block
        """
        Block.process(self, request)
        if "url"+self.id in request.args:
            self.idevice.url = request.args["url"+self.id][0]
            if (self.idevice.url and 
                not self.idevice.url.startswith("http://") and 
                not self.idevice.url.startswith("https://") and
                not self.idevice.url.startswith("ftp://")):
                self.idevice.url = "http://" + self.idevice.url
        if "height"+self.id in request.args:
            self.idevice.height = request.args["height"+self.id][0]
    def renderEdit(self, style):
        """
        Returns an XHTML string with the form element for editing this block
        """
        html  = u'<div class="block">\n'
        html += u"<strong>%s</strong> " % _('URL:')
        html += common.elementInstruc(self.idevice.urlInstruc)
        html += u"</div>\n"
        html += u'<div class="block">\n'
        html += common.textInput("url"+self.id, self.idevice.url) 
        heightArr = [['small',      '200'],
                     ['medium',     '300'],
                     ['large',      '500'],
                     ['super-size', '800']]
        html += u"</div>\n"
        html += u'<div class="block">\n'
        this_package = None
        if self.idevice is not None and self.idevice.parentNode is not None:
            this_package = self.idevice.parentNode.package
        html += common.formField('select', this_package, _('Frame Height:'), 
                                 "height"+self.id,
                                 options = heightArr,
                                 selection = self.idevice.height)
        html += u"</div>\n"
        html += self.renderEditButtons()
        return html
    def renderViewContent(self):
        """
        Returns an XHTML string for previewing this block
        """
        html = ""
        if self.idevice.url:
            html += u"<iframe src=\""+self.idevice.url+"\"\n"
            html += u"width=\"100%\""
            html += u" height=\""+self.idevice.height+"px\"></iframe>\n" 
        return html
from exe.engine.externalurlidevice import ExternalUrlIdevice
from exe.webui.blockfactory     import g_blockFactory
g_blockFactory.registerBlockType(ExternalUrlBlock, ExternalUrlIdevice)    
