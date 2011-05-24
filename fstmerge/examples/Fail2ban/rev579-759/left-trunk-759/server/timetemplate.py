__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:38 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import time, logging
from template import Template, Templates
from mytime import MyTime
import iso8601
logSys = logging.getLogger("fail2ban.timetemplate")
class TimeTemplate(Template):
	
	def __init__(self):
		Template.__init__(self, Template.TEMPLATE_TIME, "<TIME>")
	
	def setRegex(self, regex):
		Template.setRegex(self, "(?P<%s>%s)" % (self.getName(), regex))
	
	def getTime(self, line):
		raise Exception("getTime() is abstract")
class TimeISO8601(TimeTemplate):
	def __init__(self):
		TimeTemplate.__init__(self)
		date_re = "[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}" \
		".[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?" \
		"(Z|(([-+])([0-9]{2}):([0-9]{2})))?"
		self.setRegex(date_re)
	
	def getTime(self, line):
		
		return list(iso8601.parse_date(line).utctimetuple())
class TimeEpoch(TimeTemplate):
	
	def __init__(self):
		TimeTemplate.__init__(self)
		
		self.setRegex("\d{10}(\.\d{6})?")
	
	def getTime(self, line):
		
		return list(time.localtime(float(line)))
class TimeStrptime(TimeTemplate):
	
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
		TimeTemplate.__init__(self)
		self.__pattern = ""
	
	def setPattern(self, pattern):
		self.__pattern = pattern.strip()
		
	def getPattern(self):
		return self.__pattern
	
	
	def convertLocale(date):
		for t in TimeStrptime.TABLE:
			for m in TimeStrptime.TABLE[t]:
				if date.find(m) >= 0:
					return date.replace(m, t)
		return date
	convertLocale = staticmethod(convertLocale)
	
	def getTime(self, line):
		try:
			
			date = list(time.strptime(line, self.getPattern()))
		except ValueError:
			
			conv = self.convertLocale(line)
			try:
				date = list(time.strptime(conv, self.getPattern()))
			except ValueError:
				
				
				conv += " %s" % MyTime.gmtime()[0]
				pattern = "%s %%Y" % self.getPattern()
				date = list(time.strptime(conv, pattern))
		if date[0] < 2000:
			
			date[0] = MyTime.gmtime()[0]
			
			
			
			if time.mktime(date) > MyTime.time():
				date[0] -= 1
		return date
class TimeTai64n(TimeTemplate):
	
	def __init__(self):
		TimeTemplate.__init__(self)
		
		self.setRegex("@[0-9a-f]{24}")
	
	def getTime(self, line):
		
		seconds_since_epoch = line[2:17]
		return list(time.gmtime(int(seconds_since_epoch, 16)))
class TimeTemplates(Templates):
	
	def __init__(self):
		Templates.__init__(self)
		
		template = TimeStrptime()
		template.setDescription("Month Day Hour:Minute:Second")
		template.setRegex("\S{3}\s{1,2}\d{1,2} \d{2}:\d{2}:\d{2}")
		template.setPattern("%b %d %H:%M:%S")
		self.templates.append(template)
		
		template = TimeStrptime()
		template.setDescription("Weekday Month Day Hour:Minute:Second Year")
		template.setRegex("\S{3} \S{3}\s{1,2}\d{1,2} \d{2}:\d{2}:\d{2} \d{4}")
		template.setPattern("%a %b %d %H:%M:%S %Y")
		self.templates.append(template)
		
		template = TimeStrptime()
		template.setDescription("Weekday Month Day Hour:Minute:Second")
		template.setRegex("\S{3} \S{3}\s{1,2}\d{1,2} \d{2}:\d{2}:\d{2}")
		template.setPattern("%a %b %d %H:%M:%S")
		self.templates.append(template)
		
		template = TimeStrptime()
		template.setDescription("Year/Month/Day Hour:Minute:Second")
		template.setRegex("\d{4}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}")
		template.setPattern("%Y/%m/%d %H:%M:%S")
		self.templates.append(template)
		
		template = TimeStrptime()
		template.setDescription("Day/Month/Year Hour:Minute:Second")
		template.setRegex("\d{2}/\d{2}/\d{4} \d{2}:\d{2}:\d{2}")
		template.setPattern("%d/%m/%Y %H:%M:%S")
		self.templates.append(template)
		
		template = TimeStrptime()
		template.setDescription("Day/Month/Year:Hour:Minute:Second")
		template.setRegex("\d{2}/\S{3}/\d{4}:\d{2}:\d{2}:\d{2}")
		template.setPattern("%d/%b/%Y:%H:%M:%S")
		self.templates.append(template)
		
		template = TimeStrptime()
		template.setDescription("Year-Month-Day Hour:Minute:Second")
		template.setRegex("\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}")
		template.setPattern("%Y-%m-%d %H:%M:%S")
		self.templates.append(template)
		
		template = TimeStrptime()
		template.setDescription("Day-Month-Year Hour:Minute:Second[.Millisecond]")
		template.setRegex("\d{2}-\S{3}-\d{4} \d{2}:\d{2}:\d{2}")
		template.setPattern("%d-%b-%Y %H:%M:%S")
		self.templates.append(template)
		
		template = TimeTai64n()
		template.setDescription("TAI64N")
		self.templates.append(template)
		
		template = TimeEpoch()
		template.setDescription("Epoch")
		self.templates.append(template)
		
		template = TimeISO8601()
		template.setDescription("ISO 8601")
		self.templates.append(template)
