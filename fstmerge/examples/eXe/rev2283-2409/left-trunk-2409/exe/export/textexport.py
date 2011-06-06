"""
TextPageExport will export a package as a text file.
"""
from cgi                      import escape
from exe.webui.blockfactory   import g_blockFactory
from exe.engine.error         import Error
from exe.engine.path          import Path
from exe.engine.htmlToText    import HtmlToText
import logging
log = logging.getLogger(__name__)
class TextExport(object):
    """
    TextExport will export a package as a text file
    """
    def __init__(self, filename):
        self.html         = ""
        self.filename         = filename
    def export(self, package):
        """ 
        Export web site
        Cleans up the previous packages pages and performs the export
        """
        self.html  = "***" + escape(package.title) + "***"
        self.renderNode(package.root)
        if package.license <> "None":
            self.html += "<br/>***" + _("Licensed under the ")
            self.html += package.license + "***<br/>"
        if package.footer <> "":
            self.html += "<p>" + package.footer + "</p>"
        self.save(self.filename)
    def renderNode(self, node):
        """
        Returns an XHTML string for this node and recurse for the children
        """
        self.html += "\r\n\r\n**" + escape(node.titleLong) + "**"
        for idevice in node.idevices:
            block = g_blockFactory.createBlock(None, idevice)
            if not block:
                log.critical("Unable to render iDevice.")
                raise Error("Unable to render iDevice.")
            self.html += block.renderView('default')
        for child in node.children:
            self.renderNode(child)
    def save(self, filename):
        """
        Save page to a file.  
        """
        converter = HtmlToText(self.html)
        text = converter.convertToText()
        outfile = open(filename, "w")
        outfile.write(text.encode('utf8'))
        outfile.close()
