__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:45 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging, os
from configparserinc import SafeConfigParserWithIncludes
from ConfigParser import NoOptionError, NoSectionError
logSys = logging.getLogger("fail2ban.client.config")
class ConfigReader(SafeConfigParserWithIncludes):
	
	BASE_DIRECTORY = "/etc/fail2ban/"
	
	def __init__(self):
		SafeConfigParserWithIncludes.__init__(self)
		self.__opts = None
	
	
	def setBaseDir(folderName):
		path = folderName.rstrip('/')
		ConfigReader.BASE_DIRECTORY = path + '/'
	setBaseDir = staticmethod(setBaseDir)
		
	
	def getBaseDir():
		return ConfigReader.BASE_DIRECTORY
	getBaseDir = staticmethod(getBaseDir)
	
	def read(self, filename):
		basename = ConfigReader.BASE_DIRECTORY + filename
		logSys.debug("Reading " + basename)
		bConf = basename + ".conf"
		bLocal = basename + ".local"
		if os.path.exists(bConf) or os.path.exists(bLocal):
			SafeConfigParserWithIncludes.read(self, [bConf, bLocal])
			return True
		else:
			logSys.error(bConf + " and " + bLocal + " do not exist")
			return False
	
	
	
	
	
	
	
	
	
	
	
	def getOptions(self, sec, options, pOptions = None):
		values = dict()
		for option in options:
			try:
				if option[0] == "bool":
					v = self.getboolean(sec, option[1])
				elif option[0] == "int":
					v = self.getint(sec, option[1])
				else:
					v = self.get(sec, option[1])
				if not pOptions == None and option[1] in pOptions:
					continue
				values[option[1]] = v
			except NoSectionError, e:
				
				logSys.error(e)
				values[option[1]] = option[2]
			except NoOptionError:
				if not option[2] == None:
					logSys.warn("'%s' not defined in '%s'. Using default value"
								% (option[1], sec))
					values[option[1]] = option[2]
			except ValueError:
				logSys.warn("Wrong value for '" + option[1] + "' in '" + sec +
							"'. Using default one: '" + `option[2]` + "'")
				values[option[1]] = option[2]
		return values
