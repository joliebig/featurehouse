"""
A ImageWithText Idevice is one built up from an image and free text.
"""
import logging
from exe.engine.idevice   import Idevice
from exe.engine.field     import TextAreaField, ImageField
from exe.engine.translate import lateTranslate
from exe.engine.freetextidevice   import FreeTextIdevice
from exe                       import globals as G
import os
log = logging.getLogger(__name__)
class ImageWithTextIdevice(Idevice):
    """
    A ImageWithText Idevice is one built up from an image and free text.
    """
    persistenceVersion = 8
    def __init__(self, defaultImage = None):
        Idevice.__init__(self, 
                         x_(u"Image with Text"), 
                         x_(u"University of Auckland"), 
                         x_(u"""<p>
The image with text iDevice can be used in a number of ways to support both
the emotional (affective) and learning task (cognitive) dimensions of eXe
content. 
</p><p>
<b>Integrating visuals with verbal summaries</b>
</p><p>
Cognitive psychologists indicate that presenting learners with a
representative image and corresponding verbal summary (that is presented
simultaneously) can reduce cognitive load and enhance learning retention.
This iDevice can be used to present an image (photograph, diagram or
graphic) with a brief verbal summary covering the main points relating to
the image. For example, if you were teaching the functions of a four-stroke
combustion engine, you could have a visual for each of the four positions of
the piston with a brief textual summary of the key aspects of each visual.
</p>"""), u"", u"")
        self.emphasis           = Idevice.NoEmphasis
        self.image              = ImageField(x_(u"Image"), u"")
        self.image.idevice      = self
        self.image.defaultImage = defaultImage
        self.text               = TextAreaField(x_(u"Text"),
                                                x_("""Enter the text you wish to 
                                                associate with the image."""))
        self.text.idevice       = self
        self.float              = u"left"
        self.caption            = u""
        self._captionInstruc    = x_(u"""Provide a caption for the image 
you have just inserted.""")
    captionInstruc = lateTranslate('captionInstruc')
    def upgradeToVersion1(self):
        """
        Called to upgrade from 0.5 release
        """
        self.float = u"left"
    def upgradeToVersion2(self):
        """
        Called to upgrade from 0.6 release
        """
        self.caption  = u""
        self.emphasis = Idevice.NoEmphasis
    def upgradeToVersion3(self):
        """
        Upgrades v0.6 to v0.7.
        """
        self.lastIdevice = False
    def upgradeToVersion4(self):
        """
        Upgrades to exe v0.10
        """
        self._upgradeIdeviceToVersion1()
    def upgradeToVersion5(self):
        """
        Upgrades to v0.12
        """
        log.debug("upgrade to version 5")
        self._upgradeIdeviceToVersion2()        
        self.image._upgradeFieldToVersion2()
    def upgradeToVersion6(self):
        """
        Called to upgrade from 0.13 release
        """
        self._captionInstruc  = x_(u"""Provide a caption for the image 
you have just inserted.""")
    def upgradeToVersion7(self):
        """
        Called to upgrade to version 0.24
        """
        self.image.isFeedback = False
    def upgradeToVersion8(self):
        """
        Converting ImageWithTextIdevice -> FreeTextIdevice,
        now that FreeText can hold embeddded images.
        BUT - due to the inconsistent loading of the objects via unpickling,
        since the resources aren't necessarily properly loaded and upgraded,
        NOR is the package necessarily, as it might not even have a list of
        resources yet, all of this conversion code must be done in an
        afterUpgradeHandler
        """ 
        G.application.afterUpgradeHandlers.append(self.convertToFreeText)
    def convertToFreeText(self):
        """
        Actually do the Converting of 
              ImageWithTextIdevice -> FreeTextIdevice,
        now that FreeText can hold embeddded images.
        """
        new_content = ""
        imageResource_exists = False
        if self.image.imageResource:
            self.image.imageResource.checksumCheck()
            if os.path.exists(self.image.imageResource.path) and \
            os.path.isfile(self.image.imageResource.path): 
                imageResource_exists = True
            else:
                log.warn("Couldn't find ImageWithText image when upgrading "\
                        + self.image.imageResource.storageName)
        if imageResource_exists:
            new_content += "<img src=\"resources/" \
                    + self.image.imageResource.storageName + "\" " 
            if self.image.height:
                new_content += "height=\"" + self.image.height + "\" " 
            if self.image.width:
                new_content += "width=\"" + self.image.width + "\" " 
            new_content += "/> \n"
        elif self.image.imageResource:
            new_content += "<BR>\n[WARNING: missing image: " \
                    + self.image.imageResource.storageName + "]\n"
        if self.caption != "": 
            new_content += "<BR>\n[" + self.caption + "]\n"
        if self.text.content != "": 
            new_content += "<P>\n" + self.text.content + "\n"
        replacementIdev = FreeTextIdevice(new_content)
        replacementIdev.content.content_wo_resourcePaths = \
                replacementIdev.content.MassageContentForRenderView( \
                    replacementIdev.content.content_w_resourcePaths)
        self.parentNode.addIdevice(replacementIdev)
        replacementIdev.content.setParentNode()
        if imageResource_exists:
            from exe.engine.galleryidevice  import GalleryImage 
            full_image_path = self.image.imageResource.path
            new_GalleryImage = GalleryImage(replacementIdev.content, \
                    self.caption,  full_image_path, mkThumbnail=False)
        while ( self.parentNode.idevices.index(replacementIdev) \
                > ( (self.parentNode.idevices.index(self) + 1))):
            replacementIdev.movePrev()
        self.delete()
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for this iDevice:
        """
        if hasattr(self, 'image') and hasattr(self.image, 'imageResource'):
            if this_resource == self.image.imageResource:
                return self.image
        if hasattr(self, 'text') and hasattr(self.text, 'images'):
            for this_image in self.text.images:
                if hasattr(this_image, '_imageResource') \
                and this_resource == this_image._imageResource:
                    return self.text
        return None
    def getRichTextFields(self):
        """
        Like getResourcesField(), a general helper to allow nodes to search 
        through all of their fields without having to know the specifics of each
        iDevice type.  
        """
        fields_list = []
        if hasattr(self, 'text'):
            fields_list.append(self.text)
        return fields_list
