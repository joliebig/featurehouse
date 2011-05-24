__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:54 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import unittest
from server.datedetector import DateDetector
from server.datetemplate import DateTemplate
class DateDetectorTest(unittest.TestCase):
	def setUp(self):
		"""Call before every test case."""
		self.__datedetector = DateDetector()
		self.__datedetector.addDefaultTemplate()
	def tearDown(self):
		"""Call after every test case."""
	
	def testGetEpochTime(self):
		log = "1138049999 [sshd] error: PAM: Authentication failure"
		date = [2006, 1, 23, 21, 59, 59, 0, 23, 0]
		dateUnix = 1138049999.0
		
		self.assertEqual(self.__datedetector.getTime(log), date)
		self.assertEqual(self.__datedetector.getUnixTime(log), dateUnix)
	
	def testGetTime(self):
		log = "Jan 23 21:59:59 [sshd] error: PAM: Authentication failure"
		date = [2005, 1, 23, 21, 59, 59, 1, 23, -1]
		dateUnix = 1106513999.0
	
		self.assertEqual(self.__datedetector.getTime(log), date)
		self.assertEqual(self.__datedetector.getUnixTime(log), dateUnix)
	
