__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:29 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import re, time
from mytime import MyTime
import iso8601
class DateTemplate:
	
	def __init__(self):
		self.__name = ""
		self.__regex = ""
		self.__cRegex = None
		self.__hits = 0
	
	def setName(self, name):
		self.__name = name
		
	def getName(self):
		return self.__name
	
	def setRegex(self, regex):
		self.__regex = regex.strip()
		self.__cRegex = re.compile(regex)
		
	def getRegex(self):
		return self.__regex
	
	def getHits(self):
		return self.__hits
	
	def matchDate(self, line):
		dateMatch = self.__cRegex.search(line)
		if not dateMatch == None:
			self.__hits += 1
		return dateMatch
	
	def getDate(self, line):
		raise Exception("matchDate() is abstract")
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
		self.__pattern = ""
	
	def setPattern(self, pattern):
		self.__pattern = pattern.strip()
		
	def getPattern(self):
		return self.__pattern
	
	
	def convertLocale(date):
		for t in DateStrptime.TABLE:
			for m in DateStrptime.TABLE[t]:
				if date.find(m) >= 0:
					return date.replace(m, t)
		return date
	convertLocale = staticmethod(convertLocale)
	
	def getDate(self, line):
		date = None
		dateMatch = self.matchDate(line)
		if dateMatch:
			try:
				
				date = list(time.strptime(dateMatch.group(), self.getPattern()))
			except ValueError:
				
				conv = self.convertLocale(dateMatch.group())
				try:
					date = list(time.strptime(conv, self.getPattern()))
				except ValueError, e:
					
					
					conv += " %s" % MyTime.gmtime()[0]
					pattern = "%s %%Y" % self.getPattern()
					date = list(time.strptime(conv, pattern))
			if date[0] < 2000:
				
				date[0] = MyTime.gmtime()[0]
				
				
				
				if time.mktime(date) > MyTime.time():
					date[0] -= 1
				elif date[1] == 1 and date[2] == 1:
					
					
					date[1] = MyTime.gmtime()[1]
					date[2] = MyTime.gmtime()[2]
		return date
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
class DateISO8601(DateTemplate):
	def __init__(self):
		DateTemplate.__init__(self)
		date_re = "[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}" \
		".[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?" \
		"(Z|(([-+])([0-9]{2}):([0-9]{2})))?"
		self.setRegex(date_re)
	
	def getDate(self, line):
		date = None
		dateMatch = self.matchDate(line)
		if dateMatch:
			
			value = dateMatch.group()
			date = list(iso8601.parse_date(value).timetuple())
		return date
