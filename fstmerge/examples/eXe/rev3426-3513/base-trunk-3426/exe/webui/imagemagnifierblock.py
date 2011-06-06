"""
ImageMagnifierBlock can render and process ImageMagnifierIdevices as XHTML
"""
import logging
from exe.webui.block   import Block
from exe.webui.element import TextAreaElement, MagnifierElement
from exe.webui         import common
log = logging.getLogger(__name__)
class ImageMagnifierBlock(Block):
    """
    ImageMagnifierBlock can render and process ImageMagnifierIdevices as XHTML
    """
    name = 'imageManifier'
    def __init__(self, parent, idevice):
        """
        Initialize
        """
        Block.__init__(self, parent, idevice)
        self.imageMagnifierElement = MagnifierElement(idevice.imageMagnifier)
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
            self.imageMagnifierElement.process(request)
            self.textElement.process(request)
        if "float"+self.id in request.args:
            self.idevice.float = request.args["float"+self.id][0]
        if "caption"+self.id in request.args:
            self.idevice.caption = request.args["caption"+self.id][0]
        if "glass"+self.id in request.args:
            self.idevice.imageMagnifier.glassSize = \
                request.args["glass"+self.id][0]
        if "initial"+self.id in request.args:
            self.idevice.imageMagnifier.initialZSize = \
                request.args["initial"+self.id][0]
        if "maxZoom"+self.id in request.args:
            self.idevice.imageMagnifier.maxZSize = \
                request.args["maxZoom"+self.id][0]
    def renderEdit(self, style):
        """
        Returns an XHTML string with the form elements for editing this block
        """
        log.debug("renderEdit")
        floatArr        = [[_(u'Left'), 'left'],
                          [_(u'Right'), 'right'],
                          [_(u'None'),  'none']]
        html  = u"<div class=\"iDevice\">\n"
        html += '<div class="block">'
        html += u"<strong>%s</strong>" % _(u"Caption:")
        html += common.elementInstruc(self.idevice.captionInstruc)
        html += '</div>'
        html += '<div class="block">'
        html += common.textInput("caption" + self.id, self.idevice.caption)
        html += '</div>'
        html += '<div class="block">'
        html += self.textElement.renderEdit()
        html += '</div>'
        html += self.imageMagnifierElement.renderEdit()       
        this_package = None
        if self.idevice is not None and self.idevice.parentNode is not None:
            this_package = self.idevice.parentNode.package
        html += common.formField('select', this_package, _("Align:"),
                                 "float" + self.id, '',
                                 self.idevice.alignInstruc,
                                 floatArr, self.idevice.float)
        zoomOpts = []
        for i in range(100, 201, 10):
            zoomOpts.append(('%d%%' % (i), str(i)))
        selection = self.idevice.imageMagnifier.initialZSize
        html += common.formField('select', this_package, _(u"Initial Zoom"),
                                 "initial" + self.id, '',
                                 self.idevice.initialZoomInstruc,
                                 zoomOpts, selection)
        selection = self.idevice.imageMagnifier.maxZSize
        html += common.formField('select', this_package, _(u"Maximum zoom"),
                                 "maxZoom" + self.id, '',
                                 self.idevice.maxZoomInstruc,
                                 zoomOpts, selection)
        glassSizeArr    = [[_(u'Small'), '1'],
                          [_(u'Medium'),'2'],
                          [_(u'Large'),'3'],
                          [_(u'Extra large'),'4'],]
        html += common.formField('select', this_package, 
                                 _(u"Size of magnifying glass: "),
                                 "glass" + self.id, '',
                                 self.idevice.glassSizeInstruc,
                                 glassSizeArr, 
                                 self.idevice.imageMagnifier.glassSize)
        html += self.renderEditButtons()
        html += u"</div>\n"
        return html
    def renderPreview(self, style):
        """
        Returns an XHTML string for previewing this block
        """
        log.debug("renderPreview")
        html  = u"\n<!-- image with text iDevice -->\n"
        html += u"<div class=\"iDevice "
        html += u"emphasis"+unicode(self.idevice.emphasis)+"\" "
        html += "ondblclick=\"submitLink('edit',"+self.id+", 0);\">\n"
        html += u"  <div class=\"image_text\" style=\""
        html += u"width:" + str(self.idevice.imageMagnifier.width) + "px; "
        html += u"float:%s;\">\n" % self.idevice.float
        html += u"    <div class=\"image\">\n"
        html += self.imageMagnifierElement.renderPreview()
        html += u"" + self.idevice.caption
        html += u"    </div> <!-- class=\"image\" -->\n" 
        html += u"  </div> <!-- class=\"image_text\" -->\n" 
        text = self.textElement.renderPreview()
        if text:
            html += text
        else:
            html += '&nbsp;'
        html += u'\n<div style="clear:both;height:1px;overflow:hidden;"></div>\n'
        html += self.renderViewButtons()
        html += u"</div> <!-- class=\"iDevice emphasisX\" -->\n" 
        return html
    def renderView(self, style):
        """
        Returns an XHTML string for viewing this block
        """        
        log.debug("renderView")
        html  = u"\n<!-- image with text iDevice -->\n"
        html += u"<div class=\"iDevice "
        html += u"emphasis"+unicode(self.idevice.emphasis)+"\">\n"
        html += u"  <div class=\"image_text\" style=\""
        html += u"width:" + str(self.idevice.imageMagnifier.width) + "px; "
        html += u"float:%s;\">\n" % self.idevice.float
        html += u"    <div class=\"image\">\n"
        html += self.imageMagnifierElement.renderView()
        html += u"    <br/>" + self.idevice.caption
        html += u"    </div> <!-- class=\"image\" -->\n" 
        html += u"  </div> <!-- class=\"image_text\" -->\n" 
        text = self.textElement.renderView()
        if text:
            html += text
        else:
            html += '&nbsp;'
        html += u'\n<div style="clear:both;height:1px;overflow:hidden;"></div>\n'
        html += u"</div> <!-- class=\"iDevice emphasisX\" -->\n" 
        return html
from exe.engine.imagemagnifieridevice import ImageMagnifierIdevice
from exe.webui.blockfactory           import g_blockFactory
g_blockFactory.registerBlockType(ImageMagnifierBlock, ImageMagnifierIdevice)    
