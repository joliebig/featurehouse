__author__ = 'Yaroslav Halhenko'
__revision__ = '$Revision: 1.1 $'
__date__ = '$Date: 2010-07-25 12:47:02 $'
__copyright__ = 'Copyright (c) 2007 Yaroslav Halchenko'
__license__ = 'GPL'
import logging, os
from ConfigParser import SafeConfigParser
logSys = logging.getLogger("fail2ban.client.config")
class SafeConfigParserWithIncludes(SafeConfigParser):
	"""
	Class adds functionality to SafeConfigParser to handle included
	other configuration files (or may be urls, whatever in the future)
	File should have section [includes] and only 2 options implemented
	are 'files_before' and 'files_after' where files are listed 1 per
	line.
	Example:
[INCLUDES]
before = 1.conf
			   3.conf
after = 1.conf
	It is a simple implementation, so just basic care is taken about
	recursion. Includes preserve right order, ie new files are
	inserted to the list of read configs before original, and their
	includes correspondingly so the list should follow the leaves of
	the tree.
	I wasn't sure what would be the right way to implement generic (aka c++
    template) so we could base at any *configparser class... so I will
    leave it for the future
	"""
	SECTION_NAME = "INCLUDES"
	
	def getIncludes(resource, seen = []):
		"""
		Given 1 config resource returns list of included files
		(recursively) with the original one as well
		Simple loops are taken care about
		"""
		
		
		SCPWI = SafeConfigParserWithIncludes
		
		parser = SafeConfigParser()
		parser.read(resource)
		
		resourceDir = os.path.dirname(resource)
		newFiles = [ ('before', []), ('after', []) ]
		if SCPWI.SECTION_NAME in parser.sections():
			for option_name, option_list in newFiles:
				if option_name in parser.options(SCPWI.SECTION_NAME):
					newResources = parser.get(SCPWI.SECTION_NAME, option_name)
					for newResource in newResources.split('\n'):
						if os.path.isabs(newResource):
							r = newResource
						else:
							r = "%s/%s" % (resourceDir, newResource)
						if r in seen:
							continue
						s = seen + [resource]
						option_list += SCPWI.getIncludes(r, s)
		
		return newFiles[0][1] + [resource] + newFiles[1][1]
		
	getIncludes = staticmethod(getIncludes)
	def read(self, filenames):
		fileNamesFull = []
		if not isinstance(filenames, list):
			filenames = [ filenames ]
		for filename in filenames:
			fileNamesFull += SafeConfigParserWithIncludes.getIncludes(filename)
		logSys.debug("Reading files: %s" % fileNamesFull)
		return SafeConfigParser.read(self, fileNamesFull)
