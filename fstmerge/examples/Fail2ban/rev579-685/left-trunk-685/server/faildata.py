__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:44 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging
logSys = logging.getLogger("fail2ban")
class FailData:
	
	def __init__(self):
		self.__retry = 0
		self.__lastTime = 0
	
	def setRetry(self, value):
		self.__retry = value
	
	def getRetry(self):
		return self.__retry
	
	def inc(self):
		self.__retry += 1
	
	def setLastTime(self, value):
		if value > self.__lastTime:
			self.__lastTime = value
	
	def getLastTime(self):
		return self.__lastTime
	
