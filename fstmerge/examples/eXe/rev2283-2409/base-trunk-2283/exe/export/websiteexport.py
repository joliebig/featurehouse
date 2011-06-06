"""
WebsiteExport will export a package as a website of HTML pages
"""
import logging
import re
import imp
from cgi                      import escape
from exe.webui.blockfactory   import g_blockFactory
from exe.engine.error         import Error
from exe.engine.path          import Path, TempDirPath
from exe.export.pages         import uniquifyNames
from exe.export.websitepage   import WebsitePage
from zipfile                  import ZipFile, ZIP_DEFLATED
log = logging.getLogger(__name__)
class WebsiteExport(object):
    """
    WebsiteExport will export a package as a website of HTML pages
    """
    def __init__(self, config, styleDir, filename):
        """
        'stylesDir' is the directory where we can copy the stylesheets from
        'outputDir' is the directory that will be [over]written
        with the website
        """
	self.config       = config
        self.imagesDir    = config.webDir/"images"
        self.scriptsDir   = config.webDir/"scripts"
        self.templatesDir = config.webDir/"templates"
        self.stylesDir    = Path(styleDir)
        self.filename     = Path(filename)
        self.pages        = []
    def exportZip(self, package):
	""" 
        Export web site
        Cleans up the previous packages pages and performs the export
        """
	
	outputDir = TempDirPath()
        self.copyFiles(package, outputDir)
        if (self.stylesDir/"websitepage.py").exists():
            global WebsitePage
            module = imp.load_source("websitepage", 
                                     self.stylesDir/"websitepage.py")
            WebsitePage = module.WebsitePage
        self.pages = [ WebsitePage("index", 1, package.root) ]
        self.generatePages(package.root, 1)
        uniquifyNames(self.pages)
        prevPage = None
        thisPage = self.pages[0]
        for nextPage in self.pages[1:]:
            thisPage.save(outputDir, prevPage, nextPage, self.pages)
            prevPage = thisPage
            thisPage = nextPage
        thisPage.save(outputDir, prevPage, None, self.pages)
	
        zipped = ZipFile(self.filename, "w")
        for scormFile in outputDir.files():
            zipped.write(scormFile,
                         scormFile.basename().encode('utf8'),
                         ZIP_DEFLATED)
        zipped.close()
        outputDir.rmtree()
	
    def export(self, package):
        """ 
        Export web site
        Cleans up the previous packages pages and performs the export
        """
        outputDir = self.filename
        if not outputDir.exists(): 
            outputDir.mkdir()
        self.copyFiles(package, outputDir)
        if (self.stylesDir/"websitepage.py").exists():
            global WebsitePage
            module = imp.load_source("websitepage", 
                                     self.stylesDir/"websitepage.py")
            WebsitePage = module.WebsitePage
        self.pages = [ WebsitePage("index", 1, package.root) ]
        self.generatePages(package.root, 1)
        uniquifyNames(self.pages)
        prevPage = None
        thisPage = self.pages[0]
        for nextPage in self.pages[1:]:
            thisPage.save(outputDir, prevPage, nextPage, self.pages)
            prevPage = thisPage
            thisPage = nextPage
        thisPage.save(outputDir, prevPage, None, self.pages)
    def copyFiles(self, package, outputDir):
        """
        Copy all the files used by the website.
        """
        styleFiles  = [self.stylesDir/'..'/'base.css']
        styleFiles += [self.stylesDir/'..'/'popup_bg.gif']
        styleFiles += self.stylesDir.files("*.css")
        styleFiles += self.stylesDir.files("*.jpg")
        styleFiles += self.stylesDir.files("*.gif")
        styleFiles += self.stylesDir.files("*.png")
        styleFiles += self.stylesDir.files("*.js")
        styleFiles += self.stylesDir.files("*.html")
        self.stylesDir.copylist(styleFiles, outputDir)
        package.resourceDir.copyfiles(outputDir)
        self.scriptsDir.copylist(('libot_drag.js', 'common.js'), 
                                  outputDir)
        self.templatesDir.copylist(('videoContainer.swf', 'magnifier.swf',
                                    'xspf_player.swf'),outputDir)
        (self.templatesDir/'fdl.html').copyfile(outputDir/'fdl.html')
    def generatePages(self, node, depth):
        """
        Recursively generate pages and store in pages member variable
        for retrieving later
        """           
        for child in node.children:
            pageName = child.titleShort.lower().replace(" ", "_")
            pageName = re.sub(r"\W", "", pageName)
            if not pageName:
                pageName = "__"
            self.pages.append(WebsitePage(pageName, depth, child))
            self.generatePages(child, depth + 1)
