import unittest, sys, zipfile
from exe.webui.block        import Block
from exe.engine.idevice     import Idevice
from exe.webui.blockfactory import g_blockFactory
from exe.webui.renderable   import Renderable
from exe.engine.node        import Node
from exe.engine.path        import Path, TempDirPath
from exe.export.scormexport import ScormExport
from utils                  import SuperTestCase, HTMLChecker
from nevow.context          import RequestContext
class TestBlock(SuperTestCase):
    """
    Tests that blocks can render stuff
    """
    ignoreErrorMsgs = [] # A list of regular expressions that match xmllint
    quickCheck = '--quick' in sys.argv
    def createPackage(self):
        """
        Creates a package using HTTP requests. Returns the html returned by the
        web server
        """
        def addIdevice(id_):
            request = self._request(action='AddIdevice', object=str(id_))
            ctx = RequestContext(request)
            return self.mainpage.authoringPage.render(request)
        ideviceCount = len(self.app.ideviceStore.getIdevices())
        for i in range(ideviceCount):
            allHtml = addIdevice(i)
        return allHtml
    def testAuthoringPage(self):
        """
        Generates a page of idevices and checks each ones xhtml individually
        """
        allHtml = self.createPackage()
        checker = HTMLChecker(self.ignoreErrorMsgs)
        mainOk = checker.check(allHtml, False, False)
        if self.quickCheck:
            if mainOk:
                return True
            else:
                self.fail('Main XHTML failed, check tmp.html and tmp.html.errors')
                return False
        Path('tmp.html').rename('tmpall.html')
        ln = len(self.package.currentNode.idevices)
        assert ln >= 1, 'Should be at least one idevice, only %s' % ln
        idevice = self.package.currentNode.idevices[0]
        ln = len(self.mainpage.authoringPage.blocks)
        assert ln >= 1, 'Should be at least one block, only %s' % ln
        chunks = zip(self.mainpage.authoringPage.blocks,
                     self.package.currentNode.idevices)
        for i, (block, idevice) in enumerate(chunks):
            assert block.idevice is idevice
            viewHTML = block.renderView('default')
            previewHTML = block.renderPreview('default')
            editHTML = block.renderEdit('default')
            if not checker.check(viewHTML, True, False):
                self.fail('Block "%s" generated bad view XHTML' % idevice.title)
                return False
            if not checker.check(previewHTML, True, True):
                self.fail('Block "%s" generated bad preview XHTML' %
                          idevice.title)
                return False
            if not checker.check(editHTML, True, True):
                self.fail('Block "%s" generated bad edit XHTML' % idevice.title)
                return False
        if not mainOk:
            Path('tmpall.html').rename('tmp.html')
            self.fail('Authoring Page generated bad XHTML, but all the blocks '
                      'were good')
    def _testSCORMExport(self):
        """
        Creates a nice package, then does a scorm export and tests the
        output
        """
        self.createPackage()
        stylesDir  = self.app.config.webDir/'style'/'default'
        filename = 'scormExport.zip'
        scormExport = ScormExport(self.app.config, stylesDir, filename)
        scormExport.export(self.package)
        zf = zipfile.ZipFile('scormExport.zip')
        names = zf.namelist()
        if 'index.html' in names:
            html = zf.read('index.html')
        else:
            self.fail('No "index.html" found in %s' % filename)
        if 'imsmanifest.xml' in names:
            xml = zf.read('imsmanifest.xml')
        else:
            self.fail('No "imsmanifest.xml" found in %s' % filename)
        checker = HTMLChecker(self.ignoreErrorMsgs)
        if not checker.check(html, False, False):
            self.fail('Scorm export generated bad XHTML')
if __name__ == "__main__":
    if '--quick' in sys.argv:
        sys.argv.remove('--quick')
    unittest.main()
