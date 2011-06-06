"""
An RSS Idevice is one built from a RSS feed.
"""
import re
from exe.engine               import feedparser
from exe.engine.idevice       import Idevice
from exe.engine.field         import TextAreaField
from exe.engine.translate     import lateTranslate
class RssIdevice(Idevice):
    """
    An RSS Idevice is one built from a RSS feed.
    """
    def __init__(self):
        Idevice.__init__(self,
                         x_(u"RSS"), 
                         x_(u"Auckland University of Technology"), 
                         x_(u"""The RSS iDevice is used 
to provide new content to an individual users machine. Using this
iDevice you can provide links from a feed you select for learners to view."""), 
                         u"",
                         u"")
        self.emphasis         = Idevice.NoEmphasis
        self.rss              = TextAreaField(x_(u"RSS"))
        self.rss.idevice      = self
        self.icon             = u"inter"
        self._urlInstruc      = x_(u"""Enter an RSS URL for the RSS feed you 
want to attach to your content. Feeds are often identified by a small graphic
 icon (often like this <img src="/images/feed-icon.png" />) or the text "RSS". Clicking on the 
 icon or text label will display an RSS feed right in your browser. You can copy and paste the
URL into this field. Alternately, right clicking on the link or graphic will open a menu box;
click on COPY LINK LOCATION or Copy Shortcut. Back in eXe open the RSS bookmark iDevice and Paste the URL 
into the RSS URL field and click the LOAD button. This will extract the titles from your feed and
display them as links in your content. From here you can edit the bookmarks and add
 instructions or additional learning information.""")
        self.url              = ""
    urlInstruc      = lateTranslate('urlInstruc')
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for this iDevice:
        """ 
        if hasattr(self, 'rss') and hasattr(self.rss, 'images'):
            for this_image in self.rss.images: 
                if hasattr(this_image, '_imageResource') \
                    and this_resource == this_image._imageResource: 
                        return self.rss
        return None
    def getRichTextFields(self):
        """
        Like getResourcesField(), a general helper to allow nodes to search 
        through all of their fields without having to know the specifics of each
        iDevice type.  
        """
        fields_list = []
        if hasattr(self, 'rss'):
            fields_list.append(self.rss)
        return fields_list
    def loadRss(self, url):
        """
        Load the rss
        """
        content = ""
        try:
            rssDic = feedparser.parse(url)
            length = len(rssDic['entries'])
            if length > 0 :
                for i in range(0, length):
                    content += '<p><A href="%s">%s</A></P>' %(
                        rssDic['entries'][i].link, rssDic['entries'][i].title)          
        except IOError, error:
            log.warning(unicode(error))
            content += _(u"Unable to load RSS feed from %s <br/>Please check the spelling and connection and try again.") % url
        if content == "":
            content += _(u"Unable to load RSS feed from %s <br/>Please check the spelling and connection and try again.") % url
        self.rss.content = unicode(content)
        self.rss.content_w_resourcePaths = self.rss.content
        self.rss.content_wo_resourcePaths = self.rss.content
    def burstHTML(self, i):
        """
        takes a BeautifulSoup fragment (i) and bursts its contents to 
        import this idevice from a CommonCartridge export
        """
        title = i.find(name='span', attrs={'class' : 'iDeviceTitle' })
        if title is not None: 
            self.title = title.renderContents().decode('utf-8')
            self.emphasis=Idevice.SomeEmphasis
        rss = i.find(name='div', attrs={'id' : re.compile('^ta') })
        self.rss.content_wo_resourcePaths = rss.renderContents().decode('utf-8')
        self.rss.content_w_resourcePaths = \
                self.rss.MassageResourceDirsIntoContent( \
                    self.rss.content_wo_resourcePaths)
        self.rss.content = self.rss.content_w_resourcePaths
        url = i.find(name='div', attrs={'class' : 'rss_url' })
        if url is not None: 
            self.url = url.attrMap['value'].decode('utf-8')
