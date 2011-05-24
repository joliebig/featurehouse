__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:43 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import time
class MyTime:
	
	myTime = None
	
	
	
	
	
	
	
	
	
	def setTime(t):
		MyTime.myTime = t
	setTime = staticmethod(setTime)
	
	
	
	
	
	
	
	def time():
		if MyTime.myTime == None:
			return time.time()
		else:
			return MyTime.myTime
	time = staticmethod(time)
	
	
	
	
	
	
	
	def gmtime():
		if MyTime.myTime == None:
			return time.gmtime()
		else:
			return time.gmtime(MyTime.myTime)
	gmtime = staticmethod(gmtime)
	
