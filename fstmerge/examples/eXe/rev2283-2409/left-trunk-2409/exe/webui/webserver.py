"""
WebServer module
"""
import sys
from cStringIO import StringIO
sys.stderr, oldStdErr = StringIO(), sys.stderr
sys.stdout, oldStdOut = StringIO(), sys.stdout
try:
    from twisted.internet              import reactor
    from twisted.internet.error        import CannotListenError
    from nevow                         import appserver
    from twisted.web                   import static
finally:
    print sys.stderr
    sys.stderr = oldStdErr
    print sys.stdout
    sys.stdout = oldStdOut
from exe.webui.packageredirectpage import PackageRedirectPage
from exe.webui.editorpage          import EditorPage
from exe.webui.preferencespage     import PreferencesPage
from exe.webui.aboutpage           import AboutPage
import logging
log = logging.getLogger(__name__)
class WebServer:
    """
    Encapsulates some twisted components to serve
    all webpages, scripts and nevow functionality
    """
    def __init__(self, application):
        """
        Initialize
        """
        self.application = application
        self.config      = application.config
        self.root        = PackageRedirectPage(self)   
        self.editor      = EditorPage(self.root)
        self.preferences = PreferencesPage(self.root)
        self.about       = AboutPage(self.root)
    def run(self):
        """
        Start serving webpages from the local web server
        """
        log.debug("start web server running")
        webDir = self.config.webDir
        self.root.putChild("images",      static.File(webDir+"/images"))
        self.root.putChild("css",         static.File(webDir+"/css"))   
        self.root.putChild("scripts",     static.File(webDir+"/scripts"))
        self.root.putChild("style",       static.File(webDir+"/style"))
        self.root.putChild("docs",        static.File(webDir+"/docs"))
        xulDir = self.config.xulDir
        self.root.putChild("xulscripts",  static.File(xulDir+"/scripts"))
        self.root.putChild("xultemplates",  static.File(xulDir+"/templates"))
        self.root.putChild("templates",   static.File(webDir+"/templates"))
        self.root.putChild("editor",      self.editor)
        self.root.putChild("preferences", self.preferences)
        self.root.putChild("about",       self.about)
        verbose_port_search = 0
        port_test_done = 0
        found_port = 0
        test_port_num = self.config.port
        test_port_count = 0
        max_port_tests = 5000
        while not port_test_done:
            test_port_num = self.config.port + test_port_count
            try:
                if verbose_port_search:
                    print "trying to listenTCP on port# ", test_port_num
                reactor.listenTCP(test_port_num, appserver.NevowSite(self.root),
                                  interface="127.0.0.1")
                if verbose_port_search:
                    print "still here after listenTCP on port# ", test_port_num
                found_port = 1
                port_test_done = 1
            except CannotListenError, exc:
                if verbose_port_search:
                    print "caught exception after listenTCP on port# ", test_port_num
                last_exception = exc
                test_port_count += 1
                if test_port_count >= max_port_tests:
                    port_test_done = 1
        if found_port:
            self.config.port = test_port_num
            if verbose_port_search:
                print "found available eXe port# ", self.config.port
            reactor.run()
        else:
            print "Sorry, unable to find an available port in the range of: ", self.config.port, " - ", test_port_num
            print "last exception: ", unicode(last_exception)
            log.error("Can't listen on interface 127.0.0.1, ports %s-%s, last exception: %s" % 
                          (self.config.port, test_port_num, unicode(last_exception)))
