"""
MultiSelectBlock can render and process MultiSelectIdevices as XHTML
"""
import logging
from exe.webui.block    import Block
from exe.webui.element  import SelectquestionElement
from exe.webui          import common
log = logging.getLogger(__name__)
class MultiSelectBlock(Block):
    """
    MultiSelectBlock can render and process MultiSelectIdevices as XHTML
    """
    def __init__(self, parent, idevice):
        """
        Initialize a new Block object
        """
        Block.__init__(self, parent, idevice)
        self.idevice           = idevice
        self.questionElements  = []
        if not hasattr(self.idevice,'undo'):
            self.idevice.undo = True
        for question in idevice.questions:
            self.questionElements.append(SelectquestionElement(question))
    def process(self, request):
        """
        Process the request arguments from the web server
        """
        Block.process(self, request)
        is_cancel = common.requestHasCancel(request)
        if ("addQuestion"+self.id) in request.args: 
            self.idevice.addQuestion()
            self.idevice.edit = True
            self.idevice.undo = False
        for element in self.questionElements:
            element.process(request)
        if "title"+self.id in request.args \
        and not is_cancel:
            self.idevice.title = request.args["title"+self.id][0]
        if ("action" in request.args and request.args["action"][0] == "done"
            or not self.idevice.edit):
            if hasattr(self.idevice,'undo'): 
                del self.idevice.undo
    def renderEdit(self, style):
        """
        Returns an XHTML string with the form element for editing this block
        """
        html  = "<div class=\"iDevice\">\n"
        html += common.textInput("title"+self.id, self.idevice.title)
        html += u"<br/><br/>\n"
        for element in self.questionElements:
            html += element.renderEdit() 
        value = _("Add another Question")    
        html += "<br/>" 
        html += common.submitButton("addQuestion"+self.id, value)
        html += "<br/><br/>" 
        html += self.renderEditButtons(undo=self.idevice.undo)
        html += "</div>\n"
        return html
    def renderPreview(self, style):
        """
        Returns an XHTML string for previewing this block
        """
        html  = u"<div class=\"iDevice "
        html += u"emphasis"+unicode(self.idevice.emphasis)+"\" "
        html += u"ondblclick=\"submitLink('edit',"+self.id+", 0);\">\n"
        html += u'<img alt="" class="iDevice_icon" '
        html += u"src=\"/style/"+style+"/icon_"+self.idevice.icon
        html += ".gif\" />\n"
        html += u"<span class=\"iDeviceTitle\">"       
        html += self.idevice.title+"</span>\n"
        html += u'<div class="iDevice_inner">\n'
        for element in self.questionElements:
            html += element.renderPreview() + "<br/>"
        html += u"</div>\n"
        html += self.renderViewButtons()
        html += u"</div>\n"
        return html
    def renderView(self, style):
        """
        Returns an XHTML string for viewing this block
        """
        html  = u'<div class="iDevice '
        html += u'emphasis'+unicode(self.idevice.emphasis)+'">\n'
        html += u'<img alt="" class="iDevice_icon" '
        html += u'src="icon_'+self.idevice.icon+'.gif" />\n'
        html += u'<span class="iDeviceTitle">'
        html += self.idevice.title+'</span>\n'
        html += u'<div class="iDevice_inner">\n'
        for element in self.questionElements:
            html += element.renderView() + "<br/>"  
        html += "</div></div>\n"
        return html
from exe.engine.multiselectidevice import MultiSelectIdevice
from exe.webui.blockfactory        import g_blockFactory
g_blockFactory.registerBlockType(MultiSelectBlock, MultiSelectIdevice)  
