__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:45 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging
from configreader import ConfigReader
from jailreader import JailReader
logSys = logging.getLogger("fail2ban.client.config")
class JailsReader(ConfigReader):
	
	def __init__(self):
		ConfigReader.__init__(self)
		self.__jails = list()
	
	def read(self):
		ConfigReader.read(self, "jail")
	
	def getOptions(self, section = None):
		opts = []
		self.__opts = ConfigReader.getOptions(self, "Definition", opts)
		if section:
			
			jail = JailReader(section)
			jail.read()
			ret = jail.getOptions()
			if ret:
				if jail.isEnabled():
					
					self.__jails.append(jail)
			else:
				logSys.error("Errors in jail '%s'. Skipping..." % section)
				return False
		else:
			
			for sec in self.sections():
				jail = JailReader(sec)
				jail.read()
				ret = jail.getOptions()
				if ret:
					if jail.isEnabled():
						
						self.__jails.append(jail)
				else:
					logSys.error("Errors in jail '" + sec + "'. Skipping...")
					return False
		return True
	
	def convert(self):
		stream = list()
		for opt in self.__opts:
			if opt == "":
				stream.append([])
		
		for jail in self.__jails:
			stream.extend(jail.convert())
		
		for jail in self.__jails:
			stream.append(["start", jail.getName()])
		
		return stream
		
