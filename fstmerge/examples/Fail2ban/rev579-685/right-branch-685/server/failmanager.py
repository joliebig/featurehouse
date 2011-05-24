__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:36 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from faildata import FailData
from ticket import FailTicket
from threading import Lock
import logging
logSys = logging.getLogger("fail2ban.filter")
class FailManager:
	
	def __init__(self):
		self.__lock = Lock()
		self.__failList = dict()
		self.__maxRetry = 3
		self.__maxTime = 600
		self.__failTotal = 0
	
	def setFailTotal(self, value):
		try:
			self.__lock.acquire()
			self.__failTotal = value
		finally:
			self.__lock.release()
		
	def getFailTotal(self):
		try:
			self.__lock.acquire()
			return self.__failTotal
		finally:
			self.__lock.release()
	
	def setMaxRetry(self, value):
		try:
			self.__lock.acquire()
			self.__maxRetry = value
		finally:
			self.__lock.release()
	
	def getMaxRetry(self):
		try:
			self.__lock.acquire()
			return self.__maxRetry
		finally:
			self.__lock.release()
	
	def setMaxTime(self, value):
		try:
			self.__lock.acquire()
			self.__maxTime = value
		finally:
			self.__lock.release()
	
	def getMaxTime(self):
		try:
			self.__lock.acquire()
			return self.__maxTime
		finally:
			self.__lock.release()
	def addFailure(self, ticket):
		try:
			self.__lock.acquire()
			ip = ticket.getIP()
			unixTime = ticket.getTime()
			if self.__failList.has_key(ip):
				fData = self.__failList[ip]
				fData.inc()
				fData.setLastTime(unixTime)
			else:
				fData = FailData()
				fData.inc()
				fData.setLastTime(unixTime)
				self.__failList[ip] = fData
			self.__failTotal += 1
		finally:
			self.__lock.release()
	
	def size(self):
		try:
			self.__lock.acquire()
			return len(self.__failList)
		finally:
			self.__lock.release()
	
	def cleanup(self, time):
		try:
			self.__lock.acquire()
			tmp = self.__failList.copy()
			for item in tmp:
				if tmp[item].getLastTime() < time - self.__maxTime:
					self.__delFailure(item)
		finally:
			self.__lock.release()
	
	def __delFailure(self, ip):
		if self.__failList.has_key(ip):
			del self.__failList[ip]
	
	def toBan(self):
		try:
			self.__lock.acquire()
			for ip in self.__failList:
				data = self.__failList[ip]
				if data.getRetry() >= self.__maxRetry:
					self.__delFailure(ip)
					
					failTicket = FailTicket(ip, data.getLastTime())
					failTicket.setAttempt(data.getRetry())
					return failTicket
			raise FailManagerEmpty
		finally:
			self.__lock.release()
class FailManagerEmpty(Exception):
	pass
