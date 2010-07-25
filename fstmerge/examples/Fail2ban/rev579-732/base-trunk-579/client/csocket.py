__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:46 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from pickle import dumps, loads, HIGHEST_PROTOCOL
import socket
class CSocket:
	
	END_STRING = "<F2B_END_COMMAND>"
	
	def __init__(self, sock = "/tmp/fail2ban.sock"):
		
		
		self.__csock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
		
		self.__csock.connect(sock)
	
	def send(self, msg):
		
		obj = dumps([str(m) for m in msg], HIGHEST_PROTOCOL)
		self.__csock.send(obj + CSocket.END_STRING)
		ret = self.receive(self.__csock)
		self.__csock.close()
		return ret
	
	@staticmethod
	def receive(sock):
		msg = ''
		while msg.rfind(CSocket.END_STRING) == -1:
			chunk = sock.recv(6)
			if chunk == '':
				raise RuntimeError, "socket connection broken"
			msg = msg + chunk
		return loads(msg)
