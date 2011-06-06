"""
StyleMenu is responsible for creating the XHTML for the styles menu
"""
import logging
from exe.webui.renderable import Renderable
from nevow import stan
log = logging.getLogger(__name__)
class StyleMenu(Renderable):
    """
    StyleMenu is responsible for creating the XHTML for the styles menu
    """
    name = 'stylePane'
    def process(self, request):
        """ 
        Get current package
        """
        log.debug("process")
        if ("action" in request.args and 
            request.args["action"][0] == "ChangeStyle"):
            log.debug("changing style to "+request.args["object"][0])
            self.package.style = request.args["object"][0]
    def render(self, ctx, data):
        """
        Returns an XUL string for viewing this pane
        """
        log.debug("render")
        xul  = u"<!-- Styles Pane Start -->\n"
        xul += u"<menupopup>\n"
        for style in self.config.styles:
            xul += u"  <menuitem label=\""+style+"\" "
            xul += u"onclick=\"submitLink('ChangeStyle', '"+style+"', 1);\"/>\n"
        xul += u"</menupopup>\n"
        xul += u"<!-- Styles Pane End -->\n"
        return stan.xml(xul)
