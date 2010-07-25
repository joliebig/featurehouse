__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:43 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from regex import Regex, RegexException
class FailRegex(Regex):
	
	
	
	
	
	
	
	def __init__(self, value):
		
		regex = value.replace("<HOST>", "(?:::f{4,6}:)?(?P<host>\S+)")
		
		Regex.__init__(self, regex)
		
		if "host" not in self._regexObj.groupindex:
			raise RegexException("No 'host' group in '%s'" % self._regex)
	
	
	
	
	
	
	
	def getHost(self):
		host = self._matchCache.group("host")
		if host == None:
			raise RegexException("Unexpected error. Please check your regex")
		return host
