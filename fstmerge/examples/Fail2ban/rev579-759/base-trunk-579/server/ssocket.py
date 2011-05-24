__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:48 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from threading import Thread
from pickle import dumps, loads, HIGHEST_PROTOCOL
import socket, logging, os, os.path
logSys = logging.getLogger("fail2ban.comm")
class SSocket(Thread):
	
	END_STRING = "<F2B_END_COMMAND>"
	
	def __init__(self, transmitter):
		Thread.__init__(self)
		self.__transmit = transmitter
		self.__isRunning = False
		self.__socket = "/tmp/fail2ban.sock"
		self.__ssock = None
		logSys.debug("Created SSocket")
	
	def initialize(self, sock = "/tmp/fail2ban.sock", force = False):
		self.__socket = sock
		
		if os.path.exists(sock):
			logSys.error("Fail2ban seems to be already running")
			if force:
				logSys.warn("Forcing execution of the server")
				os.remove(sock)
			else:
				raise SSocketErrorException("Server already running")
		
		
		self.__ssock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
		
		
		
		
		
		self.__ssock.settimeout(1)
		
		
		self.__ssock.bind(sock)
		
		self.__ssock.listen(1)
	
	def run(self):
		self.__isRunning = True
		while self.__isRunning:
			try:
				(csock, address) = self.__ssock.accept()
				thread = SocketWorker(csock, self.__transmit)
				thread.start()
			except socket.timeout:
				
				pass
			except socket.error:
				
				pass
		self.__ssock.close()
		
		if os.path.exists(self.__socket):
			logSys.debug("Removed socket file " + self.__socket)
			os.remove(self.__socket)
		logSys.debug("Socket shutdown")
		return True
	
	
	
	
	
	
	
	def stop(self):
		self.__isRunning = False
class SocketWorker(Thread):
	
	def __init__(self, csock, transmitter):
		Thread.__init__(self)
		self.__csock = csock
		self.__transmit = transmitter
		
	def run(self):
		logSys.debug("Starting new thread to handle the request")
		msg = self.__receive(self.__csock)
		msg = self.__transmit.proceed(msg)
		self.__send(self.__csock, msg)
		self.__csock.close()
		logSys.debug("Connection closed")
	
	@staticmethod
	def __send(sock, msg):
		obj = dumps(msg, HIGHEST_PROTOCOL)
		sock.send(obj + SSocket.END_STRING)
	
	@staticmethod
	def __receive(sock):
		msg = ''
		while msg.rfind(SSocket.END_STRING) == -1:
			chunk = sock.recv(128)
			if chunk == '':
				raise RuntimeError, "socket connection broken"
			msg = msg + chunk
		return loads(msg)
class SSocketErrorException(Exception):
	pass
