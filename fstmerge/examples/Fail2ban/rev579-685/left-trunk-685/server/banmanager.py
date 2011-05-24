__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:43 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from banticket import BanTicket
from threading import Lock
from mytime import MyTime
import logging
logSys = logging.getLogger("fail2ban.action")
class BanManager:
	
	
	
	
	
	
	def __init__(self):
		
		self.__lock = Lock()
		
		self.__banList = list()
		
		self.__banTime = 600
		
		self.__banTotal = 0
	
	
	
	
	
	
	
	def setBanTime(self, value):
		try:
			self.__lock.acquire()
			self.__banTime = int(value)
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	def getBanTime(self):
		try:
			self.__lock.acquire()
			return self.__banTime
		finally:
			self.__lock.release()
	
	
	
	
	
	
	def setBanTotal(self, value):
		try:
			self.__lock.acquire()
			self.__banTotal = value
		finally:
			self.__lock.release()
	
	
	
	
	
	
	def getBanTotal(self):
		try:
			self.__lock.acquire()
			return self.__banTotal
		finally:
			self.__lock.release()
	
	
	
	
	
	def getBanList(self):
		try:
			self.__lock.acquire()
			return [m.getIP() for m in self.__banList]
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	
	
	def createBanTicket(ticket):
		ip = ticket.getIP()
		
		lastTime = MyTime.time()
		banTicket = BanTicket(ip, lastTime)
		banTicket.setAttempt(ticket.getAttempt())
		return banTicket
	createBanTicket = staticmethod(createBanTicket)
	
	
	
	
	
	
	
	
	def addBanTicket(self, ticket):
		try:
			self.__lock.acquire()
			if not self.__inBanList(ticket):
				self.__banList.append(ticket)
				self.__banTotal += 1
				return True
			return False
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	def size(self):
		try:
			self.__lock.acquire()
			return len(self.__banList)
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	
	
	def __inBanList(self, ticket):
		for i in self.__banList:
			if ticket.getIP() == i.getIP():
				return True
		return False
	
	
	
	
	
	
	
	
	def unBanList(self, time):
		try:
			self.__lock.acquire()
			
			if self.__banTime < 0:
				return list()
			
			unBanList = [ticket for ticket in self.__banList
						 if ticket.getTime() < time - self.__banTime]
			
			
			self.__banList = [ticket for ticket in self.__banList
							  if ticket not in unBanList]
						
			return unBanList
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	def flushBanList(self):
		try:
			self.__lock.acquire()
			uBList = self.__banList
			self.__banList = list()
			return uBList
		finally:
			self.__lock.release()
