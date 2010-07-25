__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:38 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
class Template:
	
	TEMPLATE_HOST = "host"
	TEMPLATE_TIME = "time"
	TEMPLATE_PREFIX = "prefix"
	
	def __init__(self, name, tag):
		self.__name = name
		self.__tag = tag
		self.__regex = ""
		self.__description = ""
		
	def getName(self):
		return self.__name
	
	def getTag(self):
		return self.__tag
	def setDescription(self, description):
		self.__description = description
	def getDescription(self):
		return self.__description
	
	def setRegex(self, regex):
		self.__regex = regex
		
	def getRegex(self):
		return self.__regex
class Templates:
	
	def __init__(self):
		self.templates = list()
		
	def getTemplates(self):
		return self.templates
