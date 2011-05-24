__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:41 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
import logging
from ticket import Ticket
logSys = logging.getLogger("fail2ban")
class FailTicket(Ticket):
	
	def __init__(self, ip, time):
		Ticket.__init__(self, ip, time)
	
