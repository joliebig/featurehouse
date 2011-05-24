__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:58 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from template import Template, Templates
class HostTemplate(Template):
	def __init__(self):
		Template.__init__(self, Template.TEMPLATE_HOST, "<HOST>")
	
class HostTemplates(Templates):
	
	def __init__(self):
		Templates.__init__(self)
		
		template = HostTemplate()
		template.setRegex("(?:::f{4,6}:)?(?P<%s>\S+)" % template.getName())
		self.templates.append(template)
