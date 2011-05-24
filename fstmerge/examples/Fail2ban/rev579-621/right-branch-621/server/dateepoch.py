__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:36 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import time
from datetemplate import DateTemplate
class DateEpoch(DateTemplate):
	
	def __init__(self):
		DateTemplate.__init__(self)
		
		self.setRegex("^\d{10}(\.\d{6})?")
	
	def getDate(self, line):
		date = None
		dateMatch = self.matchDate(line)
		if dateMatch:
			
			date = list(time.localtime(float(dateMatch.group())))
		return date
