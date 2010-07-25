__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:30 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from failmanager import FailManagerEmpty
from filter import FileFilter
from mytime import MyTime
import time, logging, gamin
logSys = logging.getLogger("fail2ban.filter")
class FilterGamin(FileFilter):
	
	
	
	
	
	
	def __init__(self, jail):
		FileFilter.__init__(self, jail)
		self.__modified = False
		
		self.monitor = gamin.WatchMonitor()
		logSys.debug("Created FilterGamin")
	def callback(self, path, event):
		logSys.debug("Got event: " + `event` + " for " + path)
		if event in (gamin.GAMCreated, gamin.GAMChanged, gamin.GAMExists):
			logSys.debug("File changed: " + path)
			self.getFailures(path)
			self.__modified = True
	
	
	
	
	def addLogPath(self, path, tail = False):
		if self.containsLogPath(path):
			logSys.error(path + " already exists")
		else:
			self.monitor.watch_file(path, self.callback)
			FileFilter.addLogPath(self, path, tail)
			logSys.info("Added logfile = %s" % path)			
	
	
	
	
	
	
	def delLogPath(self, path):
		if not self.containsLogPath(path):
			logSys.error(path + " is not monitored")
		else:
			self.monitor.stop_watch(path)
			FileFilter.delLogPath(self, path)
			logSys.info("Removed logfile = %s" % path)
		
	
	
	
	
	
	
	def run(self):
		self.setActive(True)
		while self._isActive():
			if not self.getIdle():
				
				
				if self.monitor.event_pending():
					self.monitor.handle_events()
				if self.__modified:
					try:
						while True:
							ticket = self.failManager.toBan()
							self.jail.putFailTicket(ticket)
					except FailManagerEmpty:
						self.failManager.cleanup(MyTime.time())
					self.dateDetector.sortTemplate()
					self.__modified = False
				time.sleep(self.getSleepTime())
			else:
				time.sleep(self.getSleepTime())
		
		self.__cleanup()
		logSys.debug(self.jail.getName() + ": filter terminated")
		return True
	
	
	def __cleanup(self):
		for path in self.getLogPath():
			self.monitor.stop_watch(path.getFileName())
		del self.monitor
