"""
This class transforms an eXe node into a page on a single page website
"""
import logging
import re
from cgi                      import escape
from urllib                   import quote
from exe.webui.blockfactory   import g_blockFactory
from exe.engine.error         import Error
from exe.engine.path          import Path
from exe.export.pages         import Page, uniquifyNames
from exe.webui                import common
log = logging.getLogger(__name__)
class SinglePage(Page):
    """
    This class transforms an eXe node into a page on a single page website
    """
    def save(self, filename, for_print=0):
        """
        Save page to a file.  
        'outputDir' is the directory where the filenames will be saved
        (a 'path' instance)
        """
        outfile = open(filename, "w")
        outfile.write(self.render(self.node.package,for_print).encode('utf8'))
        outfile.close()
    def render(self, package, for_print=0):
	"""
        Returns an XHTML string rendering this page.
        """
	html  = self.renderHeader(package.name, for_print)
        if for_print:
            html += u'<body onload="print_page()">\n'
        else:
            html += u"<body>\n"
        html += u"<div id=\"content\">\n"
        html += u"<div id=\"header\">\n"
        html += escape(package.title)
        html += u"</div>\n"
        html += u"<div id=\"main\">\n"
        html += self.renderNode(package.root)
        html += u"</div>\n"
	html += self.renderLicense()
	html += self.renderFooter()
        html += u"</div>\n"
        html += u"</body></html>\n"
	
	return html
    def renderHeader(self, name, for_print=0):
        """
        Returns an XHTML string for the header of this page.
        """
        html  = u"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        html += u'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '
        html += u'Transitional//EN" '
        html += u'"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n'
        html += u"<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
        html += u"<head>\n"
        html += u"<style type=\"text/css\">\n"
        html += u"@import url(base.css);\n"
        html += u"@import url(content.css);\n"
        html += u"</style>"
        html += u"<title>"
        html += name
        html += "</title>\n"
        html += u"<meta http-equiv=\"Content-Type\" content=\"text/html; "
        html += u" charset=utf-8\" />\n";
        html += u'<script type="text/javascript" src="common.js"></script>\n'
        if for_print:
            html += u'<script type="text/javascript">\n'
            html += u'function print_page() {\n'
            html += u'     window.print();\n'
            html += u'     window.close();\n'
            html += u'}\n'
            html += u'</script>\n'
        html += u"</head>\n"
        return html
    def renderNode(self, node):
        """
        Returns an XHTML string for this node and recurse for the children
        """
        html = ""
        html += '<div class="node">\n'
        html += '  <div id=\"nodeDecoration\">'
        html += '<p id=\"nodeTitle\">'
        html += escape(node.titleLong)
        html += '</p></div>\n'
        style = self.node.package.style
        for idevice in node.idevices:
            html += u'  <div class="%s" id="id%s">\n' % (idevice.klass,
                    idevice.id)
            block = g_blockFactory.createBlock(None, idevice)
            if not block:
                log.critical("Unable to render iDevice.")
                raise Error("Unable to render iDevice.")
            if hasattr(idevice, "isQuiz"):
                html += block.renderJavascriptForWeb()
            html += self.processInternalLinks(block.renderView(style))
            html += u'  </div>\n'     # iDevice div
        html += '</div>\n'          # node div
        for child in node.children:
            html += self.renderNode(child)
        return html
    def processInternalLinks(self, html):
        """
        take care of any internal links which are in the form of:
           href="exe-node:Home:Topic:etc#Anchor"
        For this SinglePage Export, go ahead and keep the #Anchor portion,
        but remove the 'exe-node:Home:Topic:etc', since it is all 
        exported into the same file.
        """
        return common.removeInternalLinkNodes(html)
