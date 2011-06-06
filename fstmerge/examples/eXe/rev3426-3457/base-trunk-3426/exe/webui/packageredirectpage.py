"""
PackageRedirectPage is the first screen the user loads.  It doesn't show
anything it just redirects the user to a new package.
"""
import logging
from exe.webui.renderable     import RenderableResource
from exe.xului.mainpage       import MainPage
log = logging.getLogger(__name__)
class PackageRedirectPage(RenderableResource):
    """
    PackageRedirectPage is the first screen the user loads.  It doesn't show
    anything it just redirects the user to a new package or loads an existing 
    package.
    """
    name = '/'
    def __init__(self, webServer):
        """
        Initialize
        """
        RenderableResource.__init__(self, None, None, webServer)
        self.webServer = webServer
        self.stopping = None
    def getChild(self, name, request):
        """
        Get the child page for the name given.
        This is called if our ancestors can't find our child.
        This is probably because the url is in unicode
        """
        if name == '':
            return self
        else:
            result = self.children.get(unicode(name, 'utf8'))
            if result is not None:
                return result
            else:
                return RenderableResource.getChild(self, name, request)
    def bindNewPackage(self, package):
        """
        Binds 'package' to the appropriate url
        and creates a MainPage instance for it
        and a directory for the resource files
	    In the GTK version, this should actually
        redirect people to MainPage. Copy from
	    svn revision 1311 to re-enable gtk.
        """
        MainPage(self, package)
    def render_GET(self, request):
        """
        Create a new package and redirect the webrowser to the URL for it
        """
        log.debug("render_GET" + repr(request.args))
        package = self.packageStore.createPackage()
        self.bindNewPackage(package)
        log.info("Created a new package name="+ package.name)
        request.redirect(package.name.encode('utf8'))
        return ''
