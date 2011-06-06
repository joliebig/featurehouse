"""
FreeTextIdevice: just has a block of text
"""
import logging
from exe.engine.idevice import Idevice
from exe.engine.field   import TextAreaField
log = logging.getLogger(__name__)
class FreeTextIdevice(Idevice):
    """
    FreeTextIdevice: just has a block of text
    """
    persistenceVersion = 7
    def __init__(self, content=""):
        Idevice.__init__(self, x_(u"Free Text"), 
                         x_(u"University of Auckland"), 
                         x_(u"""The majority of a learning resource will be 
establishing context, delivering instructions and providing general information.
This provides the framework within which the learning activities are built and 
delivered."""), "", "")
        self.emphasis = Idevice.NoEmphasis
        self.content  = TextAreaField(x_(u"Free Text"), 
                                      x_(u"""Use this field to enter text. This 
iDevice has no emphasis applied although limited formatting can be applied to 
text through the text editing buttons associated with the field."""),
                                      content)
        self.content.idevice = self
        if content:
            self.edit = False
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for this iDevice:
        """
        if hasattr(self, 'content') and hasattr(self.content, 'images'):
            for this_image in self.content.images:
                if hasattr(this_image, '_imageResource') \
                and this_resource == this_image._imageResource:
                    return self.content
        return None
    def getRichTextFields(self):
        """
        Like getResourcesField(), a general helper to allow nodes to search 
        through all of their fields without having to know the specifics of each
        iDevice type.  
        """
        fields_list = []
        if hasattr(self, 'content'):
            fields_list.append(self.content)
        return fields_list
    def burstHTML(self, i):
        """
        takes a BeautifulSoup fragment (i) and bursts its contents to 
        import this idevice from a CommonCartridge export
        """
        if i.attrMap['class']=="FreeTextIdevice":
            inner = i.find(name='div', 
                attrs={'class' : 'block' , 'style' : 'display:block' })
        else:
            inner = i
        self.content.content_wo_resourcePaths = \
                inner.renderContents().decode('utf-8')
        self.content.content_w_resourcePaths = \
                self.content.MassageResourceDirsIntoContent( \
                    self.content.content_wo_resourcePaths)
        self.content.content = self.content.content_w_resourcePaths
    def upgradeToVersion1(self):
        """
        Upgrades the node from version 0 (eXe version 0.4) to 1.
        Adds icon
        """
        self.icon = ""
    def upgradeToVersion2(self):
        """
        Upgrades the node from version 1 (not released) to 2
        Use new Field classes
        """
        self.content = TextAreaField("content", 
x_(u"This is a free text field general learning content can be entered."),
                                     self.content)
    def upgradeToVersion3(self):
        """
        Upgrades the node from 2 (v0.5) to 3 (v0.6).
        Old packages will loose their icons, but they will load.
        """
        log.debug(u"Upgrading iDevice")
        self.emphasis = Idevice.NoEmphasis
    def upgradeToVersion4(self):
        """
        Upgrades v0.6 to v0.7.
        """
        self.lastIdevice = False
    def upgradeToVersion5(self):
        """
        Upgrades v0.6 to v0.7.
        """
        self._upgradeIdeviceToVersion1()
    def upgradeToVersion6(self):
        """
        Upgrades to v0.12
        """
        self._upgradeIdeviceToVersion2()
    def upgradeToVersion7(self):
        """
        Attach the idevice to the TextAreaField for tinyMCE image embedding:
        """
        self.content.idevice = self
