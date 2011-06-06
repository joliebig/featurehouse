"""
Displays a page with an error message
"""
import logging
from exe.webui import common
from twisted.web.resource import Resource
log = logging.getLogger(__name__)
class ErrorPage(Resource):
    """
    Displays a page with an error message
    """
    def __init__(self, errMessage):
        """
        Initialize
        """
        Resource.__init__(self)
        self.errMessage = errMessage
    def getChild(self, name, request):
        """
        Get the child page for the name given
        """
        if name == '':
            return self
        else:
            return Resource.getChild(self, name, request)
    def render_GET(self, request):
        """
        Create a new package and redirect the webrowser to the URL for it
        """
        log.info("render_GET" + repr(request.args))
        html = "<html><body>" 
        html += "<b>" + self.errMessage + "</b>"
        html += common.footer()
        return html
