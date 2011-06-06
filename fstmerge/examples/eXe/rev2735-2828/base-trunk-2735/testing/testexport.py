"""
Tests website and scorm exports.
"""
import unittest
import utils
from exe.engine.package       import Package
from exe.engine.path          import Path, TempDirPath
from exe.export.websiteexport import WebsiteExport
from exe.export.scormexport   import ScormExport
from exe.export.imsexport     import IMSExport
from zipfile                  import ZipFile
from sets                     import Set
class TestWebsiteExport(unittest.TestCase):
    def testExport(self):
        outdir = TempDirPath()
        package = Package.load('testPackage.elp')
        exporter = WebsiteExport('../exe/webui/style/default', 
                                 outdir,
                                 '../exe/webui/images', 
                                 '../exe/webui/scripts',
                                 '../exe/webui/templates')
        exporter.export(package)
        assert outdir.isdir()
        assert (outdir / 'index.html').isfile()
        for filename in Path('../exe/webui/style/default').files():
            assert ((outdir / filename.basename()).exists(),
                    'Style file "%s" not copied' % (outdir / filename.basename()))
        pagenodes  = Set([p.node for p in exporter.pages])
        othernodes = Set(self._getNodes([], package.root))
        assert pagenodes == othernodes
        for page in exporter.pages:
            self._testPage(page, outdir)
    def _getNodes(self, nodes, node):
        nodes.append(node)
        for child in node.children:
            self._getNodes(nodes, child)
        return nodes 
    def _testPage(self, page, outdir):
        """
        Tests for more or less correct creation of webpages
        """
        node = page.node
        pageName = outdir/page.name+'.html'
        assert pageName.exists(), 'HTML file "%s" not created' % pageName
        html = open(pageName).read()
        assert (node.title in html,
                'Node title (%s) not found in "%s"' % (node.title, pageName))
class BaseTestScormExport(utils.SuperTestCase):
    def doTest(self, ExporterClass):
        """Exports a package with meta data"""
        package = Package.load('testPackage.elp')
        outFilename = Path('scormtest.zip')
        exporter = ExporterClass(self.app.config,
                                 '../exe/webui/style/default', 
                                 outFilename)
        exporter.export(package)
        assert outFilename.exists()
        zipped = ZipFile(outFilename)
        filenames = zipped.namelist()
        assert 'imsmanifest.xml' in filenames, filenames
        self._testManifest(zipped.read('imsmanifest.xml'))
        pagenodes  = Set([p.node for p in exporter.pages])
        othernodes = Set(self._getNodes([], package.root))
        assert pagenodes == othernodes
        for page in exporter.pages:
            self._testPage(page, zipped)
        zipped.close()
    def _testManifest(self, content):
        """Override this func to test different types of manifest files"""
    def _getNodes(self, nodes, node):
        nodes.append(node)
        for child in node.children:
            self._getNodes(nodes, child)
        return nodes 
    def _testPage(self, page, zipped):
        """Tests for more or less correct creation
        of webpages"""
        node     = page.node
        filename = page.name + '.html'
        assert (filename in zipped.namelist(), 
                'HTML file "%s" not created' % filename)
        html = zipped.read(filename)
        assert (node.title in html,
                'Node title (%s) not found in "%s"' % (node.title, filename))
class TestScormMetaExport(BaseTestScormExport):
    def testMeta(self):
        """Tests exporting of scorm packages with meta data"""
        self.doTest(ScormExport)
    def _setupConfigFile(self, configParser):
        """Set up our config file nicely"""
        BaseTestScormExport._setupConfigFile(self, configParser)
    def _testManifest(self, content):
        """Checks that there is meta data in 'content'"""
        assert '<metadata>' in content
        assert '</metadata>' in content
class TestScormNoMetaExport(BaseTestScormExport):
    def testMeta(self):
        """Tests exporting of scorm packages without meta data"""
        self.doTest(IMSExport)
    def _testManifest(self, content):
        """Checks that there is meta data in 'content'"""
        assert '<metadata>' in content
        assert '</metadata>' in content
if __name__ == "__main__":
    unittest.main()
