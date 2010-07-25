__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:46 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging
from configreader import ConfigReader
from fail2banreader import Fail2banReader
from jailsreader import JailsReader
logSys = logging.getLogger("fail2ban.client.config")
class Configurator:
	
	def __init__(self):
		self.__settings = dict()
		self.__streams = dict()
		self.__fail2ban = Fail2banReader()
		self.__jails = JailsReader()
	
	
	def setBaseDir(folderName):
		ConfigReader.setBaseDir(folderName)
	setBaseDir = staticmethod(setBaseDir)
	
	
	def getBaseDir():
		return ConfigReader.getBaseDir()
	getBaseDir = staticmethod(getBaseDir)
	
	def readEarly(self):
		self.__fail2ban.read()
	
	def readAll(self):
		self.readEarly()
		self.__jails.read()
	
	def getEarlyOptions(self):
		return self.__fail2ban.getEarlyOptions()
	def getOptions(self, jail = None):
		self.__fail2ban.getOptions()
		return self.__jails.getOptions(jail)
		
	def convertToProtocol(self):
		self.__streams["general"] = self.__fail2ban.convert()
		self.__streams["jails"] = self.__jails.convert()
	
	def getConfigStream(self):
		cmds = list()
		for opt in self.__streams["general"]:
			cmds.append(opt)
		for opt in self.__streams["jails"]:
			cmds.append(opt)
		return cmds
	
