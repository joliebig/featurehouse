"""
WebServer module
"""
from twisted.internet              import reactor
from twisted.internet.error        import CannotListenError
from nevow                         import appserver
from twisted.web                   import static
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
        try:
            reactor.listenTCP(self.config.port, appserver.NevowSite(self.root),
                              interface="127.0.0.1")
        except CannotListenError, exc:
            log.error("Can't listen on interface 127.0.0.1, port %s: %s" % 
                      (self.config.port, unicode(exc)))
        else:
            reactor.run()
