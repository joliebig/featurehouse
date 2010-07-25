__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:40 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from template import Template, Templates
class PrefixTemplate(Template):
	def __init__(self):
		Template.__init__(self, Template.TEMPLATE_PREFIX, "<PREFIX>")
	
class PrefixTemplates(Templates):
	
	def __init__(self):
		Templates.__init__(self)
		
		template = PrefixTemplate()
		template.setRegex("\S+ \S+\[\d+\]:")
		self.templates.append(template)
		
		template = PrefixTemplate()
		template.setRegex("\[\S+\] \S+: \S+:")
		self.templates.append(template)
		
		template = PrefixTemplate()
		template.setRegex("\S+ \S+:")
		self.templates.append(template)
