__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:45 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import unittest
from client.jailreader import JailReader
class JailReaderTest(unittest.TestCase):
	def setUp(self):
		"""Call before every test case."""
	def tearDown(self):
		"""Call after every test case."""
	def testSplitAction(self):
		action = "mail-whois[name=SSH]"
		expected = ['mail-whois', {'name': 'SSH'}]
		result = JailReader.splitAction(action)
		self.assertEquals(expected, result)
		
