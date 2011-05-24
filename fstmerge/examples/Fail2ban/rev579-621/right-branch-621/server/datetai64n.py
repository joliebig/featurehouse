__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:38 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import time
from datetemplate import DateTemplate
class DateTai64n(DateTemplate):
	
	def __init__(self):
		DateTemplate.__init__(self)
		
		self.setRegex("@[0-9a-f]{24}")
	
	def getDate(self, line):
		date = None
		dateMatch = self.matchDate(line)
		if dateMatch:
			
			value = dateMatch.group()
			seconds_since_epoch = value[2:17]
			date = list(time.gmtime(int(seconds_since_epoch, 16)))
		return date
