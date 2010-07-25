__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:47:04 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import unittest, socket, time, pickle
from server.banmanager import BanManager
from server.banticket import BanTicket
class AddFailure(unittest.TestCase):
	def setUp(self):
		"""Call before every test case."""
		self.__ticket = BanTicket('193.168.0.128', 1167605999.0)
		self.__banManager = BanManager()
		self.assertTrue(self.__banManager.addBanTicket(self.__ticket))
	def tearDown(self):
		"""Call after every test case."""
	
	def testAdd(self):
		self.assertEqual(self.__banManager.size(), 1)
	
	def testAddDuplicate(self):
		self.assertFalse(self.__banManager.addBanTicket(self.__ticket))
		self.assertEqual(self.__banManager.size(), 1)
		
	def _testInListOK(self):
		ticket = BanTicket('193.168.0.128', 1167605999.0)
		self.assertTrue(self.__banManager.inBanList(ticket))
	
	def _testInListNOK(self):
		ticket = BanTicket('111.111.1.111', 1167605999.0)
		self.assertFalse(self.__banManager.inBanList(ticket))
		
