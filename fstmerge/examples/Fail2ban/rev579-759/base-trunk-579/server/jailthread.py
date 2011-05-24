__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:48 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from threading import Thread
import logging
logSys = logging.getLogger("fail2ban.server")
class JailThread(Thread):
	
	
	
	
	
	
	
	def __init__(self):
		Thread.__init__(self)
		
		self.__isRunning = False
		
		self.__isIdle = False
		
		self.__sleepTime = 1
	
	
	
	
	
	
	
	
	def setSleepTime(self, value):
		self.__sleepTime = value
		logSys.info("Set sleeptime = " + value)
	
	
	
	
	
	
	def getSleepTime(self):
		return self.__sleepTime
	
	
	
	
	
	
	
	def setIdle(self, value):
		self.__isIdle = value
	
	
	
	
	
	
	def getIdle(self):
		return self.__isIdle
	
	
	
	
	
	
	def stop(self):
		self.__isRunning = False
	
	
	
	
	
	
	def setActive(self, value):
		self.__isRunning = value
	
	
	
	
	
	
	
	def _isActive(self):
		return self.__isRunning
	
	
	
	
	
	
	
	def status(self):
		pass
