__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:50 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging
from configreader import ConfigReader
logSys = logging.getLogger("fail2ban.client.config")
class Fail2banReader(ConfigReader):
	
	def __init__(self):
		ConfigReader.__init__(self)
	
	def read(self):
		ConfigReader.read(self, "fail2ban")
	
	def getEarlyOptions(self):
		opts = [["string", "socket", "/tmp/fail2ban.sock"]]
		return ConfigReader.getOptions(self, "Definition", opts)
	
	def getOptions(self):
		opts = [["int", "loglevel", 1],
				["string", "logtarget", "STDERR"]]
		self.__opts = ConfigReader.getOptions(self, "Definition", opts)
	
	def convert(self):
		stream = list()
		for opt in self.__opts:
			if opt == "loglevel":
				stream.append(["set", "loglevel", self.__opts[opt]])
			elif opt == "logtarget":
				stream.append(["set", "logtarget", self.__opts[opt]])
		return stream
	
