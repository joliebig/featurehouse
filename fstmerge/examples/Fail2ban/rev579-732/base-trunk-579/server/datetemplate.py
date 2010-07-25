__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:34 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import re
class DateTemplate:
	
	def __init__(self):
		self.__name = ""
		self.__regex = ""
		self.__cRegex = None
		self.__pattern = ""
		self.__hits = 0
	
	def setName(self, name):
		self.__name = name
		
	def getName(self):
		return self.__name
	
	def setRegex(self, regex):
		self.__regex = regex.strip()
		self.__cRegex = re.compile(regex)
		
	def getRegex(self):
		return self.__regex
	
	def setPattern(self, pattern):
		self.__pattern = pattern.strip()
		
	def getPattern(self):
		return self.__pattern
	
	def isValid(self):
		return self.__regex != "" and self.__pattern != ""
	
	def incHits(self):
		self.__hits = self.__hits + 1
	
	def getHits(self):
		return self.__hits
	
	def matchDate(self, line):
		dateMatch = self.__cRegex.search(line)
		return dateMatch
	
	def getDate(self, line):
		raise Exception("matchDate() is abstract")
