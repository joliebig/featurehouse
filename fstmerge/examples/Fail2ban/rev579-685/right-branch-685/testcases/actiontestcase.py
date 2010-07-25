__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:47 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import unittest, time
from server.action import Action
class ExecuteAction(unittest.TestCase):
	def setUp(self):
		"""Call before every test case."""
		self.__action = Action("Test")
	def tearDown(self):
		"""Call after every test case."""
		self.__action.execActionStop()
	
	def testExecuteActionBan(self):
		self.__action.setActionStart("touch /tmp/fail2ban.test")
		self.__action.setActionStop("rm -f /tmp/fail2ban.test")
		self.__action.setActionBan("echo -n")
		self.__action.setActionCheck("[ -e /tmp/fail2ban.test ]")
		
		self.assertTrue(self.__action.execActionBan(None))
		
