__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:57 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import unittest
from server.filterpoll import FilterPoll
from server.filter import FileFilter
from server.failmanager import FailManager
from server.failmanager import FailManagerEmpty
class IgnoreIP(unittest.TestCase):
	def setUp(self):
		"""Call before every test case."""
		self.__filter = FileFilter(None)
	def tearDown(self):
		"""Call after every test case."""
	def testIgnoreIPOK(self):
		ipList = "127.0.0.1", "192.168.0.1", "255.255.255.255", "99.99.99.99"
		for ip in ipList:
			self.__filter.addIgnoreIP(ip)
			self.assertTrue(self.__filter.inIgnoreIPList(ip))
		
		self.__filter.addIgnoreIP("www.epfl.ch")
		self.assertTrue(self.__filter.inIgnoreIPList("128.178.50.12"))
	
	def testIgnoreIPNOK(self):
		ipList = "", "999.999.999.999", "abcdef", "192.168.0."
		for ip in ipList:
			self.__filter.addIgnoreIP(ip)
			self.assertFalse(self.__filter.inIgnoreIPList(ip))
		
		self.__filter.addIgnoreIP("www.epfl.ch")
		self.assertFalse(self.__filter.inIgnoreIPList("127.177.50.10"))
class LogFile(unittest.TestCase):
	FILENAME = "testcases/files/testcase01.log"
	def setUp(self):
		"""Call before every test case."""
		self.__filter = FilterPoll(None)
		self.__filter.addLogPath(LogFile.FILENAME)
	def tearDown(self):
		"""Call after every test case."""
		
	
	
	
	def testIsModified(self):
		self.assertTrue(self.__filter.isModified(LogFile.FILENAME))
class GetFailures(unittest.TestCase):
	FILENAME_01 = "testcases/files/testcase01.log"
	FILENAME_02 = "testcases/files/testcase02.log"
	FILENAME_03 = "testcases/files/testcase03.log"
	FILENAME_04 = "testcases/files/testcase04.log"
	def setUp(self):
		"""Call before every test case."""
		self.__filter = FileFilter(None)
		self.__filter.setActive(True)
		
		
		
	def tearDown(self):
		"""Call after every test case."""
		
	def testGetFailures01(self):
		output = ('193.168.0.128', 3, 1124013599.0)
		
		self.__filter.addLogPath(GetFailures.FILENAME_01)
		self.__filter.addFailRegex("(?:(?:Authentication failure|Failed [-/\w+]+) for(?: [iI](?:llegal|nvalid) user)?|[Ii](?:llegal|nvalid) user|ROOT LOGIN REFUSED) .*(?: from|FROM) <HOST>")
		self.__filter.getFailures(GetFailures.FILENAME_01)
		
		ticket = self.__filter.failManager.toBan()
		attempts = ticket.getAttempt()
		date = ticket.getTime()
		ip = ticket.getIP()
		found = (ip, attempts, date)
		
		self.assertEqual(found, output)
	
	def testGetFailures02(self):
		output = ('141.3.81.106', 4, 1124013539.0)
		self.__filter.addLogPath(GetFailures.FILENAME_02)
		self.__filter.addFailRegex("Failed .* from <HOST>")
		
		self.__filter.getFailures(GetFailures.FILENAME_02)
		
		ticket = self.__filter.failManager.toBan()
		attempts = ticket.getAttempt()
		date = ticket.getTime()
		ip = ticket.getIP()
		found = (ip, attempts, date)
		
		self.assertEqual(found, output)
	def testGetFailures03(self):
		output = ('203.162.223.135', 6, 1124013544.0)
		self.__filter.addLogPath(GetFailures.FILENAME_03)
		self.__filter.addFailRegex("error,relay=<HOST>,.*550 User unknown")
		
		self.__filter.getFailures(GetFailures.FILENAME_03)
		
		ticket = self.__filter.failManager.toBan()
		
		attempts = ticket.getAttempt()
		date = ticket.getTime()
		ip = ticket.getIP()
		found = (ip, attempts, date)
		
		self.assertEqual(found, output)	
	def testGetFailures04(self):
		output = [('212.41.96.186', 4, 1124013600.0),
				  ('212.41.96.185', 4, 1124013598.0)]
		self.__filter.addLogPath(GetFailures.FILENAME_04)
		self.__filter.addFailRegex("Invalid user .* <HOST>")
		
		self.__filter.getFailures(GetFailures.FILENAME_04)
		try:
			for i in range(2):
				ticket = self.__filter.failManager.toBan()		
				attempts = ticket.getAttempt()
				date = ticket.getTime()
				ip = ticket.getIP()
				found = (ip, attempts, date)
				self.assertEqual(found, output[i])
		except FailManagerEmpty:
			pass
		
	def testGetFailuresMultiRegex(self):
		output = ('141.3.81.106', 8, 1124013541.0)
		self.__filter.addLogPath(GetFailures.FILENAME_02)
		self.__filter.addFailRegex("Failed .* from <HOST>")
		self.__filter.addFailRegex("Accepted .* from <HOST>")
		
		self.__filter.getFailures(GetFailures.FILENAME_02)
		
		ticket = self.__filter.failManager.toBan()
		attempts = ticket.getAttempt()
		date = ticket.getTime()
		ip = ticket.getIP()
		found = (ip, attempts, date)
		
		self.assertEqual(found, output)
	
	def testGetFailuresIgnoreRegex(self):
		output = ('141.3.81.106', 8, 1124013541.0)
		self.__filter.addLogPath(GetFailures.FILENAME_02)
		self.__filter.addFailRegex("Failed .* from <HOST>")
		self.__filter.addFailRegex("Accepted .* from <HOST>")
		self.__filter.addIgnoreRegex("for roehl")
		
		self.__filter.getFailures(GetFailures.FILENAME_02)
		
		self.assertRaises(FailManagerEmpty, self.__filter.failManager.toBan)
