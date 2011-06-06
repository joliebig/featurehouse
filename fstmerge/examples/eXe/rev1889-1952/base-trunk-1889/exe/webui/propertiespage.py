"""
The PropertiesPage is for user to enter or edit package's properties
"""
import logging
from exe.webui.propertiespane import PropertiesPane
from exe.webui.renderable import RenderableResource
log = logging.getLogger(__name__)
class PropertiesPage(RenderableResource):
    """
    The PropertiesPage is for user to enter or edit package's properties
    """
    name = 'properties'
    def __init__(self, parent):
        """
        Initialize
        """
        RenderableResource.__init__(self, parent)
        self.propertiesPane = PropertiesPane(self)
    def render_GET(self, request):
        """
        Render the XHTML for the properties page
        """
        log.debug("render_GET"+ repr(request.args))
        log.info("creating the properties page")
        html = '\n'.join(
          ['''<?xml version="1.0" encoding="iso-8859-1"?>''',
           '''<!DOCTYPE html PUBLIC ''',
           '''"-//W3C//DTD XHTML 1.0 Strict//EN" ''',
           '''"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">''',
           '''<html xmlns="http://www.w3.org/1999/xhtml">''',
           '''<head>''',
           '''<style type="text/css">''',
           '''@import url(/css/exe.css);''',
           '''@import url(/style/base.css);''',
           '''@import url(/style/default/content.css);</style>''',
           '''<script type="text/javascript" '''
           '''src="/scripts/common.js"></script>''',
           '''<script type="text/javascript" '''
           '''src="/scripts/fckeditor.js"></script>''',
           '''<script type="text/javascript" '''
           '''src="/scripts/libot_drag.js"></script>''',
           '''<title>eXe : elearning XHTML editor</title>''',
           '''<meta http-equiv="content-type" content="text/html;  '''
           '''charset=UTF-8"></meta>''',
           '''</head>'''])
        html += "<div id=\"main\"> \n"
        html += "<h3>%s</h3>\n" % _(u"Project Properties")
        html += self.propertiesPane.render()
        html += "</div> \n"
        html += "</body></html>"
        return html.encode('utf8')
    def render_POST(self, request):
        """
        Handles the submission of the properties form,
        creating a page that redirects the brower's top document
        back to the original package, thereby updating all the tree elements
        """
        self.propertiesPane.process(request)
        log.info("after propertityPane process:"+ repr(request.args))
        return '\n'.join(
            ['<html>'
             ' <head/>',
             ' <body onload="top.location = top.location"/>',
             '</html>']).encode('utf8')
