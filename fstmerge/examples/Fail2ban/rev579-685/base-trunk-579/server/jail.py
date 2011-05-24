__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:42 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import Queue, logging
from actions import Actions
logSys = logging.getLogger("fail2ban.jail")
class Jail:
	
	def __init__(self, name, backend = "auto"):
		self.__name = name
		self.__queue = Queue.Queue()
		self.__filter = None
		if backend == "polling":
			self.__initPoller()
		else:
			try:
				self.__initGamin()
			except ImportError:
				self.__initPoller()
		self.__action = Actions(self)
	
	def __initPoller(self):
		logSys.info("Using poller")
		from filterpoll import FilterPoll
		self.__filter = FilterPoll(self)
	
	def __initGamin(self):
		
		import gamin
		logSys.info("Using Gamin")
		from filtergamin import FilterGamin
		self.__filter = FilterGamin(self)
	
	def setName(self, name):
		self.__name = name
	
	def getName(self):
		return self.__name
	
	def getFilter(self):
		return self.__filter
	
	def getAction(self):
		return self.__action
	
	def putFailTicket(self, ticket):
		self.__queue.put(ticket)
	
	def getFailTicket(self):
		try:
			return self.__queue.get(False)
		except Queue.Empty:
			return False
	
	def start(self):
		self.__filter.start()
		self.__action.start()
	
	def stop(self):
		self.__filter.stop()
		self.__action.stop()
		self.__filter.join()
		self.__action.join()
	
	def isAlive(self):
		isAlive0 = self.__filter.isAlive()
		isAlive1 = self.__action.isAlive()
		return isAlive0 or isAlive1
	
	def setIdle(self, value):
		self.__filter.setIdle(value)
		self.__action.setIdle(value)
	
	def getIdle(self):
		return self.__filter.getIdle() or self.__action.getIdle()
	
	def getStatus(self):
		fStatus = self.__filter.status()
		aStatus = self.__action.status()
		ret = [("filter", fStatus), 
			   ("action", aStatus)]
		return ret
