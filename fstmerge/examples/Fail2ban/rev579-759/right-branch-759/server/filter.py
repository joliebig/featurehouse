__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:29 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from failmanager import FailManager
from ticket import FailTicket
from jailthread import JailThread
from datedetector import DateDetector
from mytime import MyTime
from failregex import FailRegex, Regex, RegexException
import logging, re, os, fcntl, time
logSys = logging.getLogger("fail2ban.filter")
class Filter(JailThread):
	
	
	
	
	
	
	def __init__(self, jail):
		JailThread.__init__(self)
		
		self.jail = jail
		
		self.failManager = FailManager()
		
		self.__failRegex = list()
		
		self.__ignoreRegex = list()
		
		self.__findTime = 6000
		
		self.__ignoreIpList = []
		
		self.dateDetector = DateDetector()
		self.dateDetector.addDefaultTemplate()
		logSys.debug("Created Filter")
	
	
	
	
	
	
	
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
	
	
	
	
	
	
	
	def addBannedIP(self, ip):
		unixTime = time.time()
		self.failManager.addFailure(FailTicket(ip, unixTime))
		return ip
	
	
	
	
	
	
	
	
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
				continue
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
					continue
			if a == b:
				return True
		return False
	
	def processLine(self, line):
		try:
			
			l = line.decode('utf-8')
		except UnicodeDecodeError:
			l = line
		timeMatch = self.dateDetector.matchTime(l)
		if timeMatch:
			
			timeLine = timeMatch.group()
			
			
			
			logLine  = l[:timeMatch.start()] + l[timeMatch.end():]
		else:
			timeLine = l
			logLine = l
		return self.findFailure(timeLine, logLine)
	def processLineAndAdd(self, line):
		for element in self.processLine(line):
			ip = element[0]
			unixTime = element[1]
			if unixTime < MyTime.time() - self.getFindTime():
				break
			if self.inIgnoreIPList(ip):
				logSys.debug("Ignore %s" % ip)
				continue
			logSys.debug("Found %s" % ip)
			self.failManager.addFailure(FailTicket(ip, unixTime))
	
	
	
	
	
	
	def ignoreLine(self, line):
		for ignoreRegex in self.__ignoreRegex:
			ignoreRegex.search(line)
			if ignoreRegex.hasMatched():
				return True
		return False
	
	
	
	
	
	
	def findFailure(self, timeLine, logLine):
		failList = list()
		
		if self.ignoreLine(logLine):
			
			return failList
		
		for failRegex in self.__failRegex:
			failRegex.search(logLine)
			if failRegex.hasMatched():
				
				date = self.dateDetector.getUnixTime(timeLine)
				if date == None:
					logSys.debug("Found a match for '" + logLine +"' but no "
								 + "valid date/time found for '"
								 + timeLine + "'. Please contact the "
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
class FileFilter(Filter):
	
	def __init__(self, jail):
		Filter.__init__(self, jail)
		
		self.__logPath = []
	
	
	
	
	
	def addLogPath(self, path, tail = False):
		container = FileContainer(path, tail)
		self.__logPath.append(container)
	
	
	
	
	
	
	def delLogPath(self, path):
		for log in self.__logPath:
			if log.getFileName() == path:
				self.__logPath.remove(log)
				return
	
	
	
	
		
	def getLogPath(self):
		return self.__logPath
	
	
	
	
	
	
	
	def containsLogPath(self, path):
		for log in self.__logPath:
			if log.getFileName() == path:
				return True
		return False
	
	def getFileContainer(self, path):
		for log in self.__logPath:
			if log.getFileName() == path:
				return log
		return None
	
	
	
	
	
	
	
	
	def getFailures(self, filename):
		container = self.getFileContainer(filename)
		if container == None:
			logSys.error("Unable to get failures in " + filename)
			return False
		
		try:
			container.open()
		except Exception, e:
			logSys.error("Unable to open %s" % filename)
			logSys.exception(e)
			return False
		
		line = container.readline()
		while not line == "":
			if not self._isActive():
				
				break
			self.processLineAndAdd(line)
			
			line = container.readline()
		container.close()
		return True
	
	def status(self):
		ret = Filter.status(self)
		path = [m.getFileName() for m in self.getLogPath()]
		ret.append(("File list", path))
		return ret
import md5
class FileContainer:
	
	def __init__(self, filename, tail = False):
		self.__filename = filename
		self.__tail = tail
		self.__handler = None
		
		handler = open(filename)
		stats = os.fstat(handler.fileno())
		self.__ino = stats.st_ino
		try:
			firstLine = handler.readline()
			
			self.__hash = md5.new(firstLine).digest()
			
			if tail:
				handler.seek(0, 2)
				self.__pos = handler.tell()
			else:
				self.__pos = 0
		finally:
			handler.close()
	
	def getFileName(self):
		return self.__filename
	
	def open(self):
		self.__handler = open(self.__filename)
		
		fd = self.__handler.fileno()
		fcntl.fcntl(fd, fcntl.F_SETFD, fd | fcntl.FD_CLOEXEC)
		firstLine = self.__handler.readline()
		
		myHash = md5.new(firstLine).digest()
		stats = os.fstat(self.__handler.fileno())
		
		if self.__hash != myHash or self.__ino != stats.st_ino:
			logSys.info("Log rotation detected for %s" % self.__filename)
			self.__hash = myHash
			self.__ino = stats.st_ino
			self.__pos = 0
		
		self.__handler.seek(self.__pos)
	
	def readline(self):
		if self.__handler == None:
			return ""
		return self.__handler.readline()
	
	def close(self):
		if not self.__handler == None:
			
			self.__pos = self.__handler.tell()
			
			self.__handler.close()
			self.__handler = None
import socket, struct
class DNSUtils:
	
	IP_CRE = re.compile("^(?:\d{1,3}\.){3}\d{1,3}$")
	
	
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
	dnsToIp = staticmethod(dnsToIp)
	
	
	def searchIP(text):
		""" Search if an IP address if directly available and return
			it.
		"""
		match = DNSUtils.IP_CRE.match(text)
		if match:
			return match
		else:
			return None
	searchIP = staticmethod(searchIP)
	
	
	def isValidIP(string):
		""" Return true if str is a valid IP
		"""
		s = string.split('/', 1)
		try:
			socket.inet_aton(s[0])
			return True
		except socket.error:
			return False
	isValidIP = staticmethod(isValidIP)
	
	
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
			
			ip = DNSUtils.dnsToIp(text)
			for e in ip:
				ipList.append(e)
		return ipList
	textToIp = staticmethod(textToIp)
	
	
	def cidr(i, n):
		""" Convert an IP address string with a CIDR mask into a 32-bit
			integer.
		"""
		
		MASK = 0xFFFFFFFFL
		return ~(MASK >> n) & MASK & DNSUtils.addr2bin(i)
	cidr = staticmethod(cidr)
	
	
	def addr2bin(string):
		""" Convert a string IPv4 address into an unsigned integer.
		"""
		return struct.unpack("!L", socket.inet_aton(string))[0]
	addr2bin = staticmethod(addr2bin)
	
	
	def bin2addr(addr):
		""" Convert a numeric IPv4 address into string n.n.n.n form.
		"""
		return socket.inet_ntoa(struct.pack("!L", addr))
	bin2addr = staticmethod(bin2addr)
