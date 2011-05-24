__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:36 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import re, sre_constants
class Regex:
	
	
	
	
	
	
	
	def __init__(self, regex):
		self._matchCache = None
		
		
		regex = regex.replace("<HOST>", "(?:::f{4,6}:)?(?P<host>\S+)")
		if regex.lstrip() == '':
			raise RegexException("Cannot add empty regex")
		try:
			self._regexObj = re.compile(regex)
			self._regex = regex
		except sre_constants.error:
			raise RegexException("Unable to compile regular expression '%s'" %
								 regex)
	
	
	
	
	
	
	
	def getRegex(self):
		return self._regex
	
	
	
	
	
	
	
	
	
	def search(self, value):
		self._matchCache = self._regexObj.search(value)
	
	
	
	
	
	
	def hasMatched(self):
		if self._matchCache:
			return True
		else:
			return False
class RegexException(Exception):
	pass
class FailRegex(Regex):
	
	
	
	
	
	
	
	def __init__(self, regex):
		
		Regex.__init__(self, regex)
		
		if "host" not in self._regexObj.groupindex:
			raise RegexException("No 'host' group in '%s'" % self._regex)
	
	
	
	
	
	
	
	def getHost(self):
		host = self._matchCache.group("host")
		if host == None:
			
			s = self._matchCache.string
			r = self._matchCache.re
			raise RegexException("No 'host' found in '%s' using '%s'" % (s, r))
		return host
