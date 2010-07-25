__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:44 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from failmanager import FailManagerEmpty
from filter import Filter
from mytime import MyTime
import time, logging, os
logSys = logging.getLogger("fail2ban.filter")
class FilterPoll(Filter):
	
	
	
	
	
	
	def __init__(self, jail):
		Filter.__init__(self, jail)
		self.__modified = False
		
		self.__lastModTime = dict()
		self.__file404Cnt = dict()
		logSys.info("Created FilterPoll")
	
	
	
	
	def addLogPath(self, path):
		if self.containsLogPath(path):
			logSys.error(path + " already exists")
		else:
			self.__lastModTime[path] = 0
			self.__file404Cnt[path] = 0
			Filter.addLogPath(self, path)
			logSys.info("Added logfile = %s" % path)	
	
	
	
	
	
	
	def delLogPath(self, path):
		if not self.containsLogPath(path):
			logSys.error(path + " is not monitored")
		else:
			del self.__lastModTime[path]
			del self.__file404Cnt[path]
			Filter.delLogPath(self, path)
			logSys.info("Removed logfile = %s" % path)
	
	
	
	
	
	
	
	def run(self):
		self.setActive(True)
		while self._isActive():
			if not self.getIdle():
				
				for f in self.getLogPath():
					if self.isModified(f):
						self.getFailures(f)
						self.__modified = True
				if self.__modified:
					try:
						ticket = self.failManager.toBan()
						self.jail.putFailTicket(ticket)
					except FailManagerEmpty:
						self.failManager.cleanup(MyTime.time())
					self.dateDetector.sortTemplate()
					self.__modified = False
				time.sleep(self.getSleepTime())
			else:
				time.sleep(self.getSleepTime())
		logSys.debug(self.jail.getName() + ": filter terminated")
		return True
	
	
	
	
	
	
	def isModified(self, filename):
		try:
			logStats = os.stat(filename)
			self.__file404Cnt[filename] = 0
			if self.__lastModTime[filename] == logStats.st_mtime:
				return False
			else:
				logSys.debug(filename + " has been modified")
				self.__lastModTime[filename] = logStats.st_mtime
				return True
		except OSError:
			logSys.error("Unable to get stat on " + filename)
			self.__file404Cnt[filename] = self.__file404Cnt[filename] + 1
			if self.__file404Cnt[filename] > 2:
				logSys.warn("Too much read error. Set the jail idle")
				self.jail.setIdle(True)
				self.__file404Cnt[filename] = 0
			return False
