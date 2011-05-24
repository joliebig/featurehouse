__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:46 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from pickle import dumps, loads, HIGHEST_PROTOCOL
import asyncore, asynchat, socket, os, logging, sys
logSys = logging.getLogger("fail2ban.server.communication")
class RequestHandler(asynchat.async_chat):
	
	END_STRING = "<F2B_END_COMMAND>"
	def __init__(self, conn, transmitter):
		asynchat.async_chat.__init__(self, conn)
		self.__transmitter = transmitter
		self.__buffer = []
		
		self.set_terminator(RequestHandler.END_STRING)
		self.found_terminator = self.handle_request_line
	def collect_incoming_data(self, data):
		logSys.debug("Received raw data: " + str(data))
		self.__buffer.append(data)
	
	
	
	
	def handle_request_line(self):
		
		message = loads("".join(self.__buffer))
		
		message = self.__transmitter.proceed(message)
		
		message = dumps(message, HIGHEST_PROTOCOL)
		
		self.send(message + RequestHandler.END_STRING)
		
		self.close_when_done()
		
	def handle_error(self):
		logSys.error("Unexpected communication error")
		self.close()
		
class AsyncServer(asyncore.dispatcher):
	def __init__(self, transmitter):
		asyncore.dispatcher.__init__(self)
		self.__transmitter = transmitter
		self.__sock = "/tmp/fail2ban.sock"
		self.__init = False
	
	
	def writable(self):
		return False
	def handle_accept(self):
		try:
			conn, addr = self.accept()
		except socket.error:
			logSys.warning("Socket error")
			return
		except TypeError:
			logSys.warning("Type error")
			return
		
		
		RequestHandler(conn, self.__transmitter)
	
	
	
	
	
	
	
	def start(self, sock, force):
		self.__sock = sock
		
		if os.path.exists(sock):
			logSys.error("Fail2ban seems to be already running")
			if force:
				logSys.warn("Forcing execution of the server")
				os.remove(sock)
			else:
				raise AsyncServerException("Server already running")
		
		self.create_socket(socket.AF_UNIX, socket.SOCK_STREAM)
		self.set_reuse_addr()
		self.bind(sock)
		self.listen(1)
		
		self.__init = True
		
		asyncore.loop(timeout = 2)
	
	
	
	
	def stop(self):
		if self.__init:
			
			self.close()
		
		if os.path.exists(self.__sock):
			logSys.debug("Removed socket file " + self.__sock)
			os.remove(self.__sock)
		logSys.debug("Socket shutdown")
class AsyncServerException(Exception):
	pass
