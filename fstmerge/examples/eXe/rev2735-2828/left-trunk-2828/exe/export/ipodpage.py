"""
This class transforms an eXe node into a page on a self-contained website
"""
import logging
import re
from cgi                      import escape
from urllib                   import quote
from exe.webui.blockfactory   import g_blockFactory
from exe.engine.error         import Error
from exe.engine.path          import Path
from exe.export.pages         import Page, uniquifyNames
from exe.engine.htmlToText    import HtmlToText
log = logging.getLogger(__name__)
class IpodPage(Page):
    """
    This class transforms an eXe node into an iPod Note page
    """
    def save(self, outputDir, prevPage, nextPage, pages):
        """
        This is the main function. It will render the page and save it to a
        file.  'outputDir' is the directory where the filenames will be saved
        (a 'path' instance)
        """
        outfile = open(outputDir / self.name+".txt", "wt")
        outfile.write(self.render(prevPage, nextPage, pages))
        outfile.close()
    def render(self, prevPage, nextPage, pages):
        """
        Returns an XHTML string rendering this page.
        """
        html  = u"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        html += u"<title>" 
        html += escape(self.node.titleLong)
        html += u"</title>\n" 
        body = ""
        for idevice in self.node.idevices:
            block = g_blockFactory.createBlock(None, idevice)
            if not block:
                log.critical("Unable to render iDevice.")
                raise Error("Unable to render iDevice.")
            if hasattr(idevice, "isCloze"):
                body += block.renderText()
            if idevice.title != "Forum Discussion":
                body += block.renderView('default')
        converter = HtmlToText(body)
        text = converter.convertToText()
        text = text.replace('&', '&amp;')
        text = text.replace('>', '&gt;')
        text = text.replace('<', '&lt;')
        text = text.replace('\r\n', ' ')
        text = re.sub(r'^\n+', '', text)
        text = re.sub(r'\n{3,}', '\n\n', text)
        foot = self.getNavigationLink(prevPage, nextPage)
        bodylen = 4050 - len(html) - len(foot)
        if len(text) > bodylen:
            text = text[:text.rfind(' ', 1, bodylen)] + '...\n'
        html = html + text + foot
        html = html.encode('utf8')
        return html
    def getNavigationLink(self, prevPage, nextPage):
        """
        return the next link url of this page
        """
        html = ""
        if prevPage:
            html += '<a href="'+prevPage.name+'.txt" nopush>'
            html += "&laquo; %s</a>" % _('Previous')
        html += ' '
        if nextPage:
            html += '<a href="'+nextPage.name+'.txt" nopush>'
            html += "%s &raquo;</a>" % _('Next')
        return html
