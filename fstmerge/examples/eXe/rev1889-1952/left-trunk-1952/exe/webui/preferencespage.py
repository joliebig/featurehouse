"""
The PreferencesPage is responsible for managing eXe preferences
"""
import logging
from twisted.web.resource      import Resource
from exe.webui                 import common
from exe.webui.renderable      import RenderableResource
log = logging.getLogger(__name__)
class PreferencesPage(RenderableResource):
    """
    The PreferencesPage is responsible for managing eXe preferences
    """
    name = 'preferences'
    def __init__(self, parent):
        """
        Initialize
        """
        RenderableResource.__init__(self, parent)
        self.localeNames  = []
        for locale, translation in self.config.locales.items():
            localeName  = locale + ": " 
            localeName += translation.info().get('x-poedit-language', '')
            self.localeNames.append((localeName, locale))
    def getChild(self, name, request):
        """
        Try and find the child for the name given
        """
        if name == "":
            return self
        else:
            return Resource.getChild(self, name, request)
    def render_GET(self, request):
        """Render the preferences"""
        log.debug("render_GET")
        html  = common.docType()
        html += u"<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
        html += u"<head>\n"
        html += u"<style type=\"text/css\">\n"
        html += u"@import url(/css/exe.css);\n"
        html += u'@import url(/style/base.css);\n'
        html += u"@import url(/style/standardwhite/content.css);</style>\n"
        html += u"<title>"+_("eXe : elearning XHTML editor")+"</title>\n"
        html += u"<meta http-equiv=\"content-type\" content=\"text/html; "
        html += u" charset=UTF-8\"></meta>\n";
        html += u"</head>\n"
        html += u"<body>\n"
        html += u"<div id=\"main\"> \n"     
        html += u"<form method=\"post\" action=\"\" "
        html += u"id=\"contentForm\" >"  
        html += common.formField('select', _(u"Select Language"),
                                 'locale',
                                 options = self.localeNames,
                                 selection = self.config.locale)
        html += u"<div id=\"editorButtons\"> \n"     
        html += u"<br/>" 
        html += common.submitButton("ok", _("OK"))
        html += common.submitButton("cancel", _("Cancel"))
        html += u"</div>\n"
        html += u"</div>\n"
        html += u"<br/></form>\n"
        html += u"</body>\n"
        html += u"</html>\n"
        return html.encode('utf8')
    def render_POST(self, request):
        """
        Process the preferences selection
        """
        log.debug("render_POST " + repr(request.args))
        if "ok" in request.args:
            self.config.locale = request.args["locale"][0]
            self.config.locales[self.config.locale].install(unicode=True)
            self.config.configParser.set('user', 'locale', self.config.locale)
        html  = common.docType()
        html += u"<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
        html += u"<head></head>\n"
        html += u"<body onload=\"opener.location.reload(); "
        html += u"self.close();\"></body>\n"
        html += u"</html>\n"
        return html.encode('utf8')
