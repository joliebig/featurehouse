__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:48 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging
logSys = logging.getLogger("fail2ban")
class Ticket:
	
	def __init__(self, ip, time):
		self.__ip = ip
		self.__time = time
		self.__attempt = 0
	
	def setIP(self, value):
		self.__ip = value
	
	def getIP(self):
		return self.__ip
	
	def setTime(self, value):
		self.__time = value
	
	def getTime(self):
		return self.__time
	
	def setAttempt(self, value):
		self.__attempt = value
	
	def getAttempt(self):
		return self.__attempt
	
