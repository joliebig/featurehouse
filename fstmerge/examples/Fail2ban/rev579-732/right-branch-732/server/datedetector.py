__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:51 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import time, logging
from datetemplate import DateStrptime, DateTai64n, DateEpoch, DateISO8601
from threading import Lock
logSys = logging.getLogger("fail2ban.filter.datedetector")
class DateDetector:
	
	def __init__(self):
		self.__lock = Lock()
		self.__templates = list()
	
	def addDefaultTemplate(self):
		self.__lock.acquire()
		try:
			
			template = DateStrptime()
			template.setName("MONTH Day Hour:Minute:Second")
			template.setRegex("\S{3}\s{1,2}\d{1,2} \d{2}:\d{2}:\d{2}")
			template.setPattern("%b %d %H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("WEEKDAY MONTH Day Hour:Minute:Second Year")
			template.setRegex("\S{3} \S{3}\s{1,2}\d{1,2} \d{2}:\d{2}:\d{2} \d{4}")
			template.setPattern("%a %b %d %H:%M:%S %Y")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("WEEKDAY MONTH Day Hour:Minute:Second")
			template.setRegex("\S{3} \S{3}\s{1,2}\d{1,2} \d{2}:\d{2}:\d{2}")
			template.setPattern("%a %b %d %H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("Year/Month/Day Hour:Minute:Second")
			template.setRegex("\d{4}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}")
			template.setPattern("%Y/%m/%d %H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("Day/Month/Year Hour:Minute:Second")
			template.setRegex("\d{2}/\d{2}/\d{4} \d{2}:\d{2}:\d{2}")
			template.setPattern("%d/%m/%Y %H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("Day/MONTH/Year:Hour:Minute:Second")
			template.setRegex("\d{2}/\S{3}/\d{4}:\d{2}:\d{2}:\d{2}")
			template.setPattern("%d/%b/%Y:%H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("Month/Day/Year:Hour:Minute:Second")
			template.setRegex("\d{2}/\d{2}/\d{4}:\d{2}:\d{2}:\d{2}")
			template.setPattern("%m/%d/%Y:%H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("Year-Month-Day Hour:Minute:Second")
			template.setRegex("\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}")
			template.setPattern("%Y-%m-%d %H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("Day-MONTH-Year Hour:Minute:Second[.Millisecond]")
			template.setRegex("\d{2}-\S{3}-\d{4} \d{2}:\d{2}:\d{2}")
			template.setPattern("%d-%b-%Y %H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("Day-Month-Year Hour:Minute:Second")
			template.setRegex("\d{2}-\d{2}-\d{4} \d{2}:\d{2}:\d{2}")
			template.setPattern("%d-%m-%Y %H:%M:%S")
			self.__templates.append(template)
			
			template = DateTai64n()
			template.setName("TAI64N")
			self.__templates.append(template)
			
			template = DateEpoch()
			template.setName("Epoch")
			self.__templates.append(template)
			
			template = DateISO8601()
			template.setName("ISO 8601")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("Hour:Minute:Second")
			template.setRegex("^\d{2}:\d{2}:\d{2}")
			template.setPattern("%H:%M:%S")
			self.__templates.append(template)
			
			template = DateStrptime()
			template.setName("<Month/Day/Year@Hour:Minute:Second>")
			template.setRegex("^<\d{2}/\d{2}/\d{2}@\d{2}:\d{2}:\d{2}>")
			template.setPattern("<%m/%d/%y@%H:%M:%S>")
			self.__templates.append(template)
		finally:
			self.__lock.release()
	
	def getTemplates(self):
		return self.__templates
	
	def matchTime(self, line):
		self.__lock.acquire()
		try:
			for template in self.__templates:
				match = template.matchDate(line)
				if not match == None:
					return match
			return None
		finally:
			self.__lock.release()
	def getTime(self, line):
		self.__lock.acquire()
		try:
			for template in self.__templates:
				try:
					date = template.getDate(line)
					if date == None:
						continue
					return date
				except ValueError:
					pass
			return None
		finally:
			self.__lock.release()
	def getUnixTime(self, line):
		date = self.getTime(line)
		if date == None:
			return None
		else:
			return time.mktime(date)
	
	
	
	
	def sortTemplate(self):
		self.__lock.acquire()
		try:
			logSys.debug("Sorting the template list")
			self.__templates.sort(lambda x, y: cmp(x.getHits(), y.getHits()))
			self.__templates.reverse()
		finally:
			self.__lock.release()
