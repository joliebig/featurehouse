__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:58 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import re, sre_constants, logging
from template import Template
from timetemplate import TimeTemplates
from prefixtemplate import PrefixTemplates
from hosttemplate import HostTemplates
logSys = logging.getLogger("fail2ban.filter.failregex")
class Regex:
	
	def __init__(self, regex):
		self.__originalRegex = regex
		self.__convertedRegex = None
		self.__compiledRegex = None
		self.__templates = dict()
		self.__hostRegex = None
		self.__dateRegex = None
		self.__prefixRegex = None
		
	def process(self):
		regex = self.__originalRegex
		for item in self.__templates.values():
			regex = regex.replace(item.getTag(), item.getRegex(), 1)
		try:
			self.__compiledRegex = re.compile(regex)
			self.__convertedRegex = regex
		except sre_constants.error:
			raise RegexException("Unable to compile regular expression '%s'" %
								 regex)
	def register(self, template):
		self.__templates[template.getName()] = template
	
	def getTemplate(self, tag):
		return self.__templates[tag]
	def match(self, line):
		return self.__compiledRegex.match(line)
	
	def getOriginalRegex(self):
		return self.__originalRegex
	
	def getConvertedRegex(self):
		return self.__convertedRegex
class FailRegex:
	
	HOST_TEMPLATES = HostTemplates()
	PREFIX_TEMPLATES = PrefixTemplates()
	TIME_TEMPLATES = TimeTemplates()
	
	def __init__(self, regex):
		self.__regex = Regex(regex)
		self.__match = None
		self.__found = False
	def __autoDetection(self, line):
		for host in self.HOST_TEMPLATES.getTemplates():
			self.__regex.register(host)
			for date in self.TIME_TEMPLATES.getTemplates():
				self.__regex.register(date)
				for prefix in self.PREFIX_TEMPLATES.getTemplates():
					self.__regex.register(prefix)
					self.__regex.process()
					match = self.__regex.match(line)
					if match:
						self.__found = True
						
						
						
						return match
		return None
	def search(self, line):
		if self.__found:
			self.__match = self.__regex.match(line)
		else:
			self.__match = self.__autoDetection(line)
	
	def hasMatched(self):
		if self.__match:
			return True
		else:
			return False
	
	def getOriginalRegex(self):
		return self.__regex.getOriginalRegex()
	
	def getHost(self):
		template = self.__regex.getTemplate(Template.TEMPLATE_HOST)
		host = self.__match.group(template.getName())
		if host == None:
			
			s = self.__match.string
			r = self.__match.re
			raise RegexException("No 'host' found in '%s' using '%s'" % (s, r))
		return host
	
	def getTime(self):
		template = self.__regex.getTemplate(Template.TEMPLATE_TIME)
		time = self.__match.group(template.getName())
		if time == None:
			
			s = self.__match.string
			r = self.__match.re
			raise RegexException("No 'time' found in '%s' using '%s'" % (s, r))
		try:
			return template.getTime(time)
		except Exception:
			return None
class RegexException(Exception):
	pass
