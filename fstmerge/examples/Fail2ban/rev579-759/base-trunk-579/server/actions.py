__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:47 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from banmanager import BanManager
from jailthread import JailThread
from action import Action
from mytime import MyTime
import time, logging
logSys = logging.getLogger("fail2ban.actions")
class Actions(JailThread):
	
	
	
	
	
	
	
	def __init__(self, jail):
		JailThread.__init__(self)
		
		self.jail = jail
		self.__actions = list()
		
		self.__banManager = BanManager()
	
	
	
	
	
	
	def addAction(self, name):
		action = Action(name)
		self.__actions.append(action)
	
	
	
	
	
	
	def delAction(self, name):
		for action in self.__actions:
			if action.getName() == name:
				self.__actions.remove(action)
				break
	
	
	
	
	
	
	
	
	
	def getAction(self, name):
		for action in self.__actions:
			if action.getName() == name:
				return action
		raise KeyError
	
	
	
	
	
	
	def getLastAction(self):
		action = self.__actions.pop()
		self.__actions.append(action)
		return action
	
	
	
	
	
	
	def setBanTime(self, value):
		self.__banManager.setBanTime(value)
		logSys.info("Set banTime = %s" % value)
	
	
	
	
	
	
	def getBanTime(self):
		return self.__banManager.getBanTime()
	
	
	
	
	
	
	
	
	def run(self):
		self.setActive(True)
		for action in self.__actions:
			action.execActionStart()
		while self._isActive():
			if not self.getIdle():
				
				ret = self.__checkBan()
				if not ret:
					self.__checkUnBan()
					time.sleep(self.getSleepTime())
			else:
				time.sleep(self.getSleepTime())
		self.__flushBan()
		for action in self.__actions:
			action.execActionStop()
		logSys.debug(self.jail.getName() + ": action terminated")
		return True
	
	
	
	
	
	
	
	def __checkBan(self):
		ticket = self.jail.getFailTicket()
		if ticket != False:
			aInfo = dict()
			bTicket = BanManager.createBanTicket(ticket)
			aInfo["ip"] = bTicket.getIP()
			aInfo["failures"] = bTicket.getAttempt()
			aInfo["time"] = bTicket.getTime()
			if self.__banManager.addBanTicket(bTicket):
				logSys.warn("[%s] Ban %s" % (self.jail.getName(), aInfo["ip"]))
				for action in self.__actions:
					action.execActionBan(aInfo)
				return True
			else:
				logSys.warn("[%s] %s already banned" % (self.jail.getName(), 
														aInfo["ip"]))
		return False
	
	
	
	
	
	
	def __checkUnBan(self):
		for ticket in self.__banManager.unBanList(MyTime.time()):
			self.__unBan(ticket)
	
	
	
	
	
	
	def __flushBan(self):
		logSys.debug("Flush ban list")
		for ticket in self.__banManager.flushBanList():
			self.__unBan(ticket)
	
	
	
	
	
	
	
	def __unBan(self, ticket):
		aInfo = dict()
		aInfo["ip"] = ticket.getIP()
		aInfo["failures"] = ticket.getAttempt()
		aInfo["time"] = ticket.getTime()
		logSys.warn("[%s] Unban %s" % (self.jail.getName(), aInfo["ip"]))
		for action in self.__actions:
			action.execActionUnban(aInfo)
			
	
	
	
	
	
	
	
	
	def status(self):
		ret = [("Currently banned", self.__banManager.size()), 
			   ("Total banned", self.__banManager.getBanTotal()),
			   ("IP list", self.__banManager.getBanList())]
		return ret
