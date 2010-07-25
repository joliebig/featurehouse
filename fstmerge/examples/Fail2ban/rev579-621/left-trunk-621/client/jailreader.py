__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:50 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging, re, glob
from configreader import ConfigReader
from filterreader import FilterReader
from actionreader import ActionReader
logSys = logging.getLogger("fail2ban.client.config")
class JailReader(ConfigReader):
	
	actionCRE = re.compile("^((?:\w|-|_|\.)+)(?:\[(.*)\])?$")
	
	def __init__(self, name):
		ConfigReader.__init__(self)
		self.__name = name
		self.__filter = None
		self.__actions = list()
	
	def setName(self, value):
		self.__name = value
	
	def getName(self):
		return self.__name
	
	def read(self):
		ConfigReader.read(self, "jail")
	
	def isEnabled(self):
		return self.__opts["enabled"]
	
	def getOptions(self):
		opts = [["bool", "enabled", "false"],
				["string", "logpath", "/var/log/messages"],
				["string", "backend", "auto"],
				["int", "maxretry", 3],
				["int", "findtime", 600],
				["int", "bantime", 600],
				["string", "failregex", None],
				["string", "ignoreregex", None],
				["string", "ignoreip", None],
				["string", "filter", ""],
				["string", "action", ""]]
		self.__opts = ConfigReader.getOptions(self, self.__name, opts)
		
		if self.isEnabled():
			
			self.__filter = FilterReader(self.__opts["filter"], self.__name)
			ret = self.__filter.read()
			if ret:
				self.__filter.getOptions(self.__opts)
			else:
				logSys.error("Unable to read the filter")
				return False
			
			
			for act in self.__opts["action"].split('\n'):
				try:
					splitAct = JailReader.splitAction(act)
					action = ActionReader(splitAct, self.__name)
					ret = action.read()
					if ret:
						action.getOptions(self.__opts)
						self.__actions.append(action)
					else:
						raise AttributeError("Unable to read action")
				except AttributeError, e:
					logSys.error("Error in action definition " + act)
					logSys.debug(e)
					return False
		return True
	
	def convert(self):
		stream = []
		for opt in self.__opts:
			if opt == "logpath":
				for path in self.__opts[opt].split("\n"):
					pathList = glob.glob(path)
					if len(pathList) == 0:
						logSys.error("No file found for " + path)
					for p in pathList:
						stream.append(["set", self.__name, "addlogpath", p])
			elif opt == "backend":
				backend = self.__opts[opt]
			elif opt == "maxretry":
				stream.append(["set", self.__name, "maxretry", self.__opts[opt]])
			elif opt == "ignoreip":
				for ip in self.__opts[opt].split():
					
					if ip != '':
						stream.append(["set", self.__name, "addignoreip", ip])
			elif opt == "findtime":
				stream.append(["set", self.__name, "findtime", self.__opts[opt]])
			elif opt == "bantime":
				stream.append(["set", self.__name, "bantime", self.__opts[opt]])
			elif opt == "failregex":
				stream.append(["set", self.__name, "failregex", self.__opts[opt]])
			elif opt == "ignoreregex":
				stream.append(["set", self.__name, "ignoreregex", self.__opts[opt]])
		stream.extend(self.__filter.convert())
		for action in self.__actions:
			stream.extend(action.convert())
		stream.insert(0, ["add", self.__name, backend])
		return stream
	
	
	def splitAction(action):
		m = JailReader.actionCRE.match(action)
		d = dict()
		if not m.group(2) == None:
			
			actions = ""
			escapeChar = None
			allowComma = False
			for c in m.group(2):
				if c in ('"', "'") and not allowComma:
					
					escapeChar = c
					allowComma = True
				elif c == escapeChar:
					
					escapeChar = None
					allowComma = False
				else:
					if c == ',' and allowComma:
						actions += "<COMMA>"
					else:
						actions += c
			
			
			actionsSplit = actions.split(',')
			
			actionsSplit = [n.replace("<COMMA>", ',') for n in actionsSplit]
			
			for param in actionsSplit:
				p = param.split('=')
				try:
					d[p[0].strip()] = p[1].strip()
				except IndexError:
					logSys.error("Invalid argument %s in '%s'" % (p, m.group(2)))
		return [m.group(1), d]
	splitAction = staticmethod(splitAction)
