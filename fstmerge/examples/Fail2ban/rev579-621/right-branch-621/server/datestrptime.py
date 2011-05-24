__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:37 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from mytime import MyTime
import time
from datetemplate import DateTemplate
class DateStrptime(DateTemplate):
	
	TABLE = dict()
	TABLE["Jan"] = []
	TABLE["Feb"] = [u"Fév"]
	TABLE["Mar"] = [u"Mär"]
	TABLE["Apr"] = ["Avr"]
	TABLE["May"] = ["Mai"]
	TABLE["Jun"] = []
	TABLE["Jul"] = []
	TABLE["Aug"] = ["Aou"]
	TABLE["Sep"] = []
	TABLE["Oct"] = ["Okt"]
	TABLE["Nov"] = []
	TABLE["Dec"] = [u"Déc", "Dez"]
	
	def __init__(self):
		DateTemplate.__init__(self)
	
	@staticmethod
	def convertLocale(date):
		for t in DateStrptime.TABLE:
			for m in DateStrptime.TABLE[t]:
				if date.find(m) >= 0:
					return date.replace(m, t)
		return date
	
	def getDate(self, line):
		date = None
		dateMatch = self.matchDate(line)
		if dateMatch:
			try:
				
				date = list(time.strptime(dateMatch.group(), self.getPattern()))
			except ValueError:
				
				conv = self.convertLocale(dateMatch.group())
				date = list(time.strptime(conv, self.getPattern()))
			if date[0] < 2000:
				
				date[0] = MyTime.gmtime()[0]
				
				
				
				if time.mktime(date) > MyTime.time():
					date[0] -= 1
		return date
