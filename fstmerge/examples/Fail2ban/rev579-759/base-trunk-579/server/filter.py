__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:48 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from failmanager import FailManager
from failticket import FailTicket
from jailthread import JailThread
from datedetector import DateDetector
from mytime import MyTime
from regex import Regex, RegexException
from failregex import FailRegex
import logging, re
logSys = logging.getLogger("fail2ban.filter")
class Filter(JailThread):
	
	
	
	
	
	
	def __init__(self, jail):
		JailThread.__init__(self)
		
		self.jail = jail
		
		self.failManager = FailManager()
		
		self.__crtHandler = None
		self.__crtFilename = None
		
		self.__logPath = []
		
		self.__failRegex = list()
		
		self.__ignoreRegex = list()
		
		self.__findTime = 6000
		
		self.__ignoreIpList = []
		
		self.__lastPos = dict()
		
		self.__lastDate = dict()
		
		self.dateDetector = DateDetector()
		self.dateDetector.addDefaultTemplate()
		logSys.info("Created Filter")
	
	
	
	
	def addLogPath(self, path):
		self.getLogPath().append(path)
		
		self.__lastDate[path] = 0
		self.__lastPos[path] = 0
	
	
	
	
	
	
	def delLogPath(self, path):
		self.getLogPath().remove(path)
		del self.__lastDate[path]
		del self.__lastPos[path]
	
	
	
	
		
	def getLogPath(self):
		return self.__logPath
	
	
	
	
	
	
	
	def containsLogPath(self, path):
		try:
			self.getLogPath().index(path)
			return True
		except ValueError:
			return False
	
	
	
	
	
	
	def setTimeRegex(self, value):
		self.dateDetector.setDefaultRegex(value)
		logSys.info("Set default regex = %s" % value)
	
	
	
	
	
		
	def getTimeRegex(self):
		return self.dateDetector.getDefaultRegex()
	
	
	
	
	
	
	def setTimePattern(self, value):
		self.dateDetector.setDefaultPattern(value)
		logSys.info("Set default pattern = %s" % value)
	
	
	
	
	
	
	def getTimePattern(self):
		return self.dateDetector.getDefaultPattern()
	
	
	
	
	
	
	
	
	def addFailRegex(self, value):
		try:
			regex = FailRegex(value)
			self.__failRegex.append(regex)
		except RegexException, e:
			logSys.error(e)
	
	def delFailRegex(self, index):
		try:
			del self.__failRegex[index]
		except IndexError:
			logSys.error("Cannot remove regular expression. Index %d is not "
						 "valid" % index)
	
	
	
	
	
	
	def getFailRegex(self):
		failRegex = list()
		for regex in self.__failRegex:
			failRegex.append(regex.getRegex())
		return failRegex
	
	
	
	
	
	
	
	
	def addIgnoreRegex(self, value):
		try:
			regex = Regex(value)
			self.__ignoreRegex.append(regex)
		except RegexException, e:
			logSys.error(e)
	
	def delIgnoreRegex(self, index):
		try:
			del self.__ignoreRegex[index]
		except IndexError:
			logSys.error("Cannot remove regular expression. Index %d is not "
						 "valid" % index)
	
	
	
	
	
	
	def getIgnoreRegex(self):
		ignoreRegex = list()
		for regex in self.__ignoreRegex:
			ignoreRegex.append(regex.getRegex())
		return ignoreRegex
	
	
	
	
	
	
	
	
	def setFindTime(self, value):
		self.__findTime = value
		self.failManager.setMaxTime(value)
		logSys.info("Set findtime = %s" % value)
	
	
	
	
	
	
	def getFindTime(self):
		return self.__findTime
	
	
	
	
	
	
	def setMaxRetry(self, value):
		self.failManager.setMaxRetry(value)
		logSys.info("Set maxRetry = %s" % value)
	
	
	
	
	
	
	def getMaxRetry(self):
		return self.failManager.getMaxRetry()
	
	
	
	
	
	
	
	def run(self):
		raise Exception("run() is abstract")
	
	
	
	
	
	
	
	
	def addIgnoreIP(self, ip):
		logSys.debug("Add " + ip + " to ignore list")
		self.__ignoreIpList.append(ip)
		
	def delIgnoreIP(self, ip):
		logSys.debug("Remove " + ip + " from ignore list")
		self.__ignoreIpList.remove(ip)
		
	def getIgnoreIP(self):
		return self.__ignoreIpList
	
	
	
	
	
	
	
	
	
	def inIgnoreIPList(self, ip):
		for i in self.__ignoreIpList:
			
			if i == "":
				return False
			s = i.split('/', 1)
			
			if len(s) == 1:
				s.insert(1, '32')
			s[1] = long(s[1])
			try:
				a = DNSUtils.cidr(s[0], s[1])
				b = DNSUtils.cidr(ip, s[1])
			except Exception:
				
				ips = DNSUtils.dnsToIp(i)
				if ip in ips:
					return True
				else:
					return False
			if a == b:
				return True
		return False
	
	
	
	
	def __openLogFile(self, filename):
		""" Opens the log file specified on init.
		"""
		try:
			self.__crtFilename = filename
			self.__crtHandler = open(filename)
			logSys.debug("Opened " + filename)
			return True
		except OSError:
			logSys.error("Unable to open " + filename)
		except IOError:
			logSys.error("Unable to read " + filename +
						 ". Please check permissions")
		return False
	
	
	
	
	def __closeLogFile(self):
		self.__crtFilename = None
		self.__crtHandler.close()
	
	
	
	
	
	
	
	def __setFilePos(self):
		line = self.__crtHandler.readline()
		lastDate = self.__lastDate[self.__crtFilename]
		lineDate = self.dateDetector.getUnixTime(line)
		if lastDate < lineDate:
			logSys.debug("Date " + `lastDate` + " is smaller than " + `lineDate`)
			logSys.debug("Log rotation detected for " + self.__crtFilename)
			self.__lastPos[self.__crtFilename] = 0
		lastPos = self.__lastPos[self.__crtFilename]
		logSys.debug("Setting file position to " + `lastPos` + " for " +
					 self.__crtFilename)
		self.__crtHandler.seek(lastPos)
	
	
	
	def __getFilePos(self):
		return self.__crtHandler.tell()
	
	
	
	
	
	
	
	def getFailures(self, filename):
		
		if not self.__openLogFile(filename):
			logSys.error("Unable to get failures in " + filename)
			return False
		self.__setFilePos()
		lastLine = None
		for line in self.__crtHandler:
			if not self._isActive():
				
				break
			try:
				
				line = line.decode('utf-8')
			except UnicodeDecodeError:
				pass
			if not self.dateDetector.matchTime(line):
				
				continue
			lastLine = line
			for element in self.findFailure(line):
				ip = element[0]
				unixTime = element[1]
				if unixTime < MyTime.time()-self.__findTime:
					break
				if self.inIgnoreIPList(ip):
					logSys.debug("Ignore "+ip)
					continue
				logSys.debug("Found "+ip)
				self.failManager.addFailure(FailTicket(ip, unixTime))
		self.__lastPos[filename] = self.__getFilePos()
		if lastLine:
			self.__lastDate[filename] = self.dateDetector.getUnixTime(lastLine)
		self.__closeLogFile()
		return True
	
	
	
	
	
	
	def findFailure(self, line):
		failList = list()
		
		for ignoreRegex in self.__ignoreRegex:
			ignoreRegex.search(line)
			if ignoreRegex.hasMatched():
				
				logSys.debug("Ignoring this line")
				return failList
		
		for failRegex in self.__failRegex:
			failRegex.search(line)
			if failRegex.hasMatched():
				
				date = self.dateDetector.getUnixTime(line)
				if date == None:
					logSys.debug("Found a match but no valid date/time found "
								 + "for " + line + ". Please contact the "
								 + "author in order to get support for this "
								 + "format")
				else:
					try:
						host = failRegex.getHost()
						ipMatch = DNSUtils.textToIp(host)
						if ipMatch:
							for ip in ipMatch:
								failList.append([ip, date])
							
							break
					except RegexException, e:
						logSys.error(e)
		return failList
	
	
	
	
	
	
	
	
	def status(self):
		ret = [("Currently failed", self.failManager.size()), 
			   ("Total failed", self.failManager.getFailTotal())]
		return ret
import socket, struct
class DNSUtils:
	
	DNS_CRE = re.compile("(?:(?:\w|-)+\.){2,}\w+")
	IP_CRE = re.compile("(?:\d{1,3}\.){3}\d{1,3}")
	
	@staticmethod
	def dnsToIp(dns):
		""" Convert a DNS into an IP address using the Python socket module.
			Thanks to Kevin Drapel.
		"""
		try:
			return socket.gethostbyname_ex(dns)[2]
		except socket.gaierror:
			logSys.warn("Unable to find a corresponding IP address for %s"
						% dns)
			return list()
	
	@staticmethod
	def textToDns(text):
		""" Search for possible DNS in an arbitrary text.
			Thanks to Tom Pike.
		"""
		match = DNSUtils.DNS_CRE.match(text)
		if match:
			return match
		else:
			return None
	
	@staticmethod
	def searchIP(text):
		""" Search if an IP address if directly available and return
			it.
		"""
		match = DNSUtils.IP_CRE.match(text)
		if match:
			return match
		else:
			return None
	
	@staticmethod
	def isValidIP(string):
		""" Return true if str is a valid IP
		"""
		s = string.split('/', 1)
		try:
			socket.inet_aton(s[0])
			return True
		except socket.error:
			return False
	
	@staticmethod
	def textToIp(text):
		""" Return the IP of DNS found in a given text.
		"""
		ipList = list()
		
		plainIP = DNSUtils.searchIP(text)
		if not plainIP == None:
			plainIPStr = plainIP.group(0)
			if DNSUtils.isValidIP(plainIPStr):
				ipList.append(plainIPStr)
		if not ipList:
			
			dns = DNSUtils.textToDns(text)
			if not dns == None:
				ip = DNSUtils.dnsToIp(dns.group(0))
				for e in ip:
					ipList.append(e)
		return ipList
	
	@staticmethod
	def cidr(i, n):
		""" Convert an IP address string with a CIDR mask into a 32-bit
			integer.
		"""
		
		MASK = 0xFFFFFFFFL
		return ~(MASK >> n) & MASK & DNSUtils.addr2bin(i)
	
	@staticmethod
	def addr2bin(string):
		""" Convert a string IPv4 address into an unsigned integer.
		"""
		return struct.unpack("!L", socket.inet_aton(string))[0]
	
	@staticmethod
	def bin2addr(addr):
		""" Convert a numeric IPv4 address into string n.n.n.n form.
		"""
		return socket.inet_ntoa(struct.pack("!L", addr))
