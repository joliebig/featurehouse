__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:47:01 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging
from configreader import ConfigReader
logSys = logging.getLogger("fail2ban.client.config")
class FilterReader(ConfigReader):
	
	def __init__(self, fileName, name):
		ConfigReader.__init__(self)
		self.__file = fileName
		self.__name = name
	
	def setFile(self, fileName):
		self.__file = fileName
	
	def getFile(self):
		return self.__file
	
	def setName(self, name):
		self.__name = name
	
	def getName(self):
		return self.__name
	
	def read(self):
		return ConfigReader.read(self, "filter.d/" + self.__file)
	
	def getOptions(self, pOpts):
		opts = [["string", "timeregex", None],
				["string", "timepattern", None],
				["string", "ignoreregex", ""],
				["string", "failregex", ""]]
		self.__opts = ConfigReader.getOptions(self, "Definition", opts, pOpts)
	
	def convert(self):
		stream = list()
		for opt in self.__opts:
			if opt == "timeregex":
				stream.append(["set", self.__name, "timeregex", self.__opts[opt]])
			elif opt == "timepattern":
				stream.append(["set", self.__name, "timepattern", self.__opts[opt]])
			elif opt == "failregex":
				for regex in self.__opts[opt].split('\n'):
					
					if regex != '':
						stream.append(["set", self.__name, "addfailregex", regex])
			elif opt == "ignoreregex":
				for regex in self.__opts[opt].split('\n'):
					
					if regex != '':
						stream.append(["set", self.__name, "addignoreregex", regex])		
		return stream
		
