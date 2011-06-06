"""
MathIdevice: just has a block of text
"""
import logging
from exe.engine.idevice import Idevice
from exe.engine.field   import MathField
log = logging.getLogger(__name__)
from exe.engine.path      import Path
from exe.engine.freetextidevice   import FreeTextIdevice
from exe                       import globals as G
import os
class MathIdevice(Idevice):
    """
    MathIdevice: just has a block of text
    """
    persistenceVersion = 1
    def __init__(self, instruc="", latex=""):
        Idevice.__init__(self, x_(u"Maths"), 
                         x_(u"University of Auckland"), 
                         x_("""The mathematical language LATEX has been 
                        used to enable your to insert mathematical formula 
                        into your content. It does this by translating 
                        LATEX into an image which is then displayed
                         within your eXe content. We would recommend that 
                        you use the Free Text iDevice to provide 
                        explanatory notes and learning instruction around 
                        this graphic."""),
                        "", 
                        "")
        self.emphasis = Idevice.NoEmphasis
        self.content  = MathField(x_(u"Maths"), 
                                      x_(u"""You can use the toolbar or enter latex manually into the textarea. """))
        self.content.idevice = self
    def getResourcesField(self, this_resource):
        """
        implement the specific resource finding mechanism for this iDevice:
        """
        if hasattr(self, 'content') and hasattr(self.content, 'gifResource'):
            if this_resource == self.content.gifResource:
                return self.content
        return None
    def getRichTextFields(self):
        """
        Like getResourcesField(), a general helper to allow nodes to search 
        through all of their fields without having to know the specifics of each
        iDevice type.  
        """
        return []
    def upgradeToVersion1(self):
        """
        Converting MathsIdevice -> FreeTextIdevice,
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
              MathsIdevice -> FreeTextIdevice,
        now that FreeText can hold embeddded images.
        """
        new_content = ""
        imageResource_exists = False
        if not self.content.gifResource is None:
            if os.path.exists(self.content.gifResource.path) and \
            os.path.isfile(self.content.gifResource.path): 
                imageResource_exists = True
            else:
                log.warn("Couldn't find Maths image when upgrading "\
                        + self.content.gifResource.storageName)
        if imageResource_exists:
            new_content += "<img src=\"resources/" \
                    + self.content.gifResource.storageName + "\" " 
            math_resource_url="resources/" \
                    + self.content.gifResource.storageName + ".tex\" " 
            new_content += "exe_math_latex=\"" + math_resource_url + "\" "
            new_content += "exe_math_size=\"" + repr(self.content.fontsize) \
                    + "\" "
            new_content += "/> \n"
        elif self.content.gifResource:
            new_content += "<BR>\n[WARNING: missing image: " \
                    + self.content.gifResource.storageName + "]\n"
        replacementIdev = FreeTextIdevice(new_content)
        replacementIdev.content.content_wo_resourcePaths = \
                replacementIdev.content.MassageContentForRenderView( \
                    replacementIdev.content.content_w_resourcePaths)
        self.parentNode.addIdevice(replacementIdev)
        replacementIdev.content.setParentNode()
        if imageResource_exists:
            from exe.engine.galleryidevice  import GalleryImage 
            full_image_path = self.content.gifResource.path
            new_GalleryImage = GalleryImage(replacementIdev.content, \
                    '',  full_image_path, mkThumbnail=False)
            webDir = Path(G.application.tempWebDir)
            source_tex_name = self.content.gifResource.storageName+".tex"
            math_path = webDir.joinpath(source_tex_name)
            math_filename_str = math_path.abspath().encode('utf-8')
            log.debug("convertToFreeText: writing LaTeX source into \'" \
                                        + math_filename_str + "\'.")
            math_file = open(math_filename_str, 'wb')
            math_file.write(self.content.latex)
            math_file.flush()
            math_file.close()
            new_GalleryLatex = GalleryImage(replacementIdev.content, \
                    '', math_filename_str, mkThumbnail=False) 
            new_GalleryLatexResource = new_GalleryLatex._imageResource
            mathsrc_resource_path = new_GalleryLatexResource._storageName
            mathsrc_resource_url = new_GalleryLatex.resourcesUrl \
                    + mathsrc_resource_path
            if (mathsrc_resource_url != math_resource_url): 
                log.warn('The math source was resource-ified differently ' \
                        + 'than expected, to: ' + mathsrc_resource_url \
                        + '; the source will need to be recreated.') 
            else: 
                log.debug('math source was resource-ified properly to: ' \
                        + mathsrc_resource_url)
        while ( self.parentNode.idevices.index(replacementIdev) \
                > ( (self.parentNode.idevices.index(self) + 1))):
            replacementIdev.movePrev()
        self.delete()
