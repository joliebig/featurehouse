__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:54 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import unittest, socket, time
from server.server import Server
class StartStop(unittest.TestCase):
	def setUp(self):
		"""Call before every test case."""
		self.__server = Server()
		self.__server.setLogLevel(0)
		self.__server.start(False)
	def tearDown(self):
		"""Call after every test case."""
		self.__server.quit()
	
	def testStartStopJail(self):
		name = "TestCase"
		self.__server.addJail(name)
		self.__server.startJail(name)
		time.sleep(1)
		self.__server.stopJail(name)
class Transmitter(unittest.TestCase):
	
	def setUp(self):
		"""Call before every test case."""
		self.__server = Server()
		self.__server.setLogLevel(0)
		self.__server.start(False)
	def tearDown(self):
		"""Call after every test case."""
		self.__server.quit()
	
	def testSetActionOK(self):
		name = "TestCase"
		cmdList = [["add", name],
				   ["set", name, "actionstart", "Action Start"],
				   ["set", name, "actionstop", "Action Stop"],
				   ["set", name, "actioncheck", "Action Check"],
				   ["set", name, "actionban", "Action Ban"],
				   ["set", name, "actionunban", "Action Unban"],
				   ["quit"]]
		
		outList = [(0, name),
				   (0, 'Action Start'),
				   (0, 'Action Stop'),
				   (0, 'Action Check'),
				   (0, 'Action Ban'),
				   (0, 'Action Unban'),
				   (0, None)]
		
		cnt = 0
		for cmd in cmdList:
			self.assertEqual(self.__server.transm.proceed(cmd), outList[cnt])
			cnt += 1
	
	def testSetActionNOK(self):
		name = "TestCase"
		cmdList = [["addd", name],
				   ["set", name, "test"],
				   ["prout prout", "Stop"],
				   ["fail2ban", "sucks"],
				   ["set"],
				   ["_/&%", "@*+%&"],
				   [" quit"]]
		
		outList = [1,
				   1,
				   1,
				   1,
				   1,
				   1,
				   1]
		
		cnt = 0
		for cmd in cmdList:
			msg = self.__server.transm.proceed(cmd)
			self.assertEqual(msg[0], outList[cnt])
			cnt += 1
	
	def testJail(self):
		name = "TestCase"
		cmdList = [["add", name],
				   ["set", name, "logpath", "testcases/files/testcase01.log"],
				   ["set", name, "timeregex", "\S{3}\s{1,2}\d{1,2} \d{2}:\d{2}:\d{2}"],
				   ["set", name, "timepattern", "%b %d %H:%M:%S"],
				   ["set", name, "failregex", "Authentication failure"],
				   ["start", name],
				   ["stop", name],
				   ["quit"]]
				  
		for cmd in cmdList:
			self.__server.transm.proceed(cmd)
			if cmd == ["start", name]:
				time.sleep(2)
				jail = self.__server.jails[name]
				self.assertEqual(jail.getFilter().failManager.size(), 0)
				self.assertEqual(jail.getAction().banManager.size(), 2)
		
