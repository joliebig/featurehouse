__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:44 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging, os
logSys = logging.getLogger("fail2ban.actions.action")
class Action:
	
	def __init__(self, name):
		self.__name = name
		self.__cInfo = dict()
		
		self.__actionStart = ''
		
		self.__actionBan = ''
		
		self.__actionUnban = ''
		
		self.__actionCheck = ''
		
		self.__actionStop = ''
		logSys.debug("Created Action")
	
	
	
	
	
	
	def setName(self, name):
		self.__name = name
	
	
	
	
	
	
	def getName(self):
		return self.__name
	
	
	
	
	
	
	
	
	
	
	
	def setCInfo(self, key, value):
		self.__cInfo[key] = value
	
	
	
	
	
	
	def getCInfo(self, key):
		return self.__cInfo[key]
	
	
	
	
	
	
	def delCInfo(self, key):
		del self.__cInfo[key]
	
	
	
	
	
		
	def setActionStart(self, value):
		self.__actionStart = value
		logSys.info("Set actionStart = %s" % value)
	
	
	
	
	
	
	def getActionStart(self):
		return self.__actionStart
	
	
	
	
	
	
	
	
	
	def execActionStart(self):
		startCmd = Action.replaceTag(self.__actionStart, self.__cInfo)
		return Action.executeCmd(startCmd)
	
	
	
	
	
	
	def setActionBan(self, value):
		self.__actionBan = value
		logSys.info("Set actionBan = %s" % value)
	
	
	
	
	
	
	def getActionBan(self):
		return self.__actionBan
	
	
	
	
	
	
	def execActionBan(self, aInfo):
		return self.__processCmd(self.__actionBan, aInfo)
	
	
	
	
	
	
	def setActionUnban(self, value):
		self.__actionUnban = value
		logSys.info("Set actionUnban = %s" % value)
	
	
	
	
	
	
	def getActionUnban(self):
		return self.__actionUnban
	
	
	
	
	
	
	def execActionUnban(self, aInfo):
		return self.__processCmd(self.__actionUnban, aInfo)
	
	
	
	
	
	
	def setActionCheck(self, value):
		self.__actionCheck = value
		logSys.info("Set actionCheck = %s" % value)
	
	
	
	
	
	
	def getActionCheck(self):
		return self.__actionCheck
	
	
	
	
	
	
	def setActionStop(self, value):
		self.__actionStop = value
		logSys.info("Set actionStop = %s" % value)
	
	
	
	
	
	
	def getActionStop(self):
		return self.__actionStop
	
	
	
	
	
	
	
	
	
	def execActionStop(self):
		stopCmd = Action.replaceTag(self.__actionStop, self.__cInfo)
		return Action.executeCmd(stopCmd)
	
	
	
	
	
	
	
	
	
	def replaceTag(query, aInfo):
		""" Replace tags in query
		"""
		string = query
		for tag in aInfo:
			string = string.replace('<' + tag + '>', str(aInfo[tag]))
		
		string = string.replace("<br>", '\n')
		return string
	replaceTag = staticmethod(replaceTag)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	def __processCmd(self, cmd, aInfo = None):
		""" Executes an OS command.
		"""
		if cmd == "":
			logSys.debug("Nothing to do")
			return True
		
		checkCmd = Action.replaceTag(self.__actionCheck, self.__cInfo)
		if not Action.executeCmd(checkCmd):
			logSys.error("Invariant check failed. Trying to restore a sane" +
						 " environment")
			stopCmd = Action.replaceTag(self.__actionStop, self.__cInfo)
			Action.executeCmd(stopCmd)
			startCmd = Action.replaceTag(self.__actionStart, self.__cInfo)
			Action.executeCmd(startCmd)
			if not Action.executeCmd(checkCmd):
				logSys.fatal("Unable to restore environment")
				return False
		
		if not aInfo == None:
			realCmd = Action.replaceTag(cmd, aInfo)
		else:
			realCmd = cmd
		
		
		realCmd = Action.replaceTag(realCmd, self.__cInfo)
		
		return Action.executeCmd(realCmd)
	
	
	
	
	
	
	
	
	
	
	
	
	def executeCmd(realCmd):
		logSys.debug(realCmd)
		try:
			
			
			retcode = os.system(realCmd)
			if retcode == 0:
				logSys.debug("%s returned successfully" % realCmd)
				return True
			else:
				logSys.error("%s returned %x" % (realCmd, retcode))
		except OSError, e:
			logSys.error("%s failed with %s" % (realCmd, e))
		return False
	executeCmd = staticmethod(executeCmd)
