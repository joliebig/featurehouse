__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:51 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from threading import Lock, RLock
from jails import Jails
from transmitter import Transmitter
from asyncserver import AsyncServer
from asyncserver import AsyncServerException
from common import version
import logging, logging.handlers, sys, os, signal
logSys = logging.getLogger("fail2ban.server")
class Server:
	
	PID_FILE = "/var/run/fail2ban/fail2ban.pid"
	def __init__(self, daemon = False):
		self.__loggingLock = Lock()
		self.__lock = RLock()
		self.__jails = Jails()
		self.__daemon = daemon
		self.__transm = Transmitter(self)
		self.__asyncServer = AsyncServer(self.__transm)
		self.__logLevel = None
		self.__logTarget = None
		
		self.setLogLevel(3)
		self.setLogTarget("STDOUT")
	
	def __sigTERMhandler(self, signum, frame):
		logSys.debug("Caught signal %d. Exiting" % signum)
		self.quit()
	
	def start(self, sock, force = False):
		logSys.info("Starting Fail2ban v" + version.version)
		
		
		signal.signal(signal.SIGTERM, self.__sigTERMhandler)
		signal.signal(signal.SIGINT, self.__sigTERMhandler)
		
		
		os.umask(0077)
		if self.__daemon:
			logSys.info("Starting in daemon mode")
			ret = self.__createDaemon()
			if ret:
				logSys.info("Daemon started")
			else:
				logSys.error("Could not create daemon")
				raise ServerInitializationError("Could not create daemon")
		
		
		try:
			logSys.debug("Creating PID file %s" % Server.PID_FILE)
			pidFile = open(Server.PID_FILE, 'w')
			pidFile.write("%s\n" % os.getpid())
			pidFile.close()
		except IOError, e:
			logSys.error("Unable to create PID file: %s" % e)
		
		
		logSys.debug("Starting communication")
		try:
			self.__asyncServer.start(sock, force)
		except AsyncServerException, e:
			logSys.error("Could not start server: %s", e)
		
		try:
			logSys.debug("Remove PID file %s" % Server.PID_FILE)
			os.remove(Server.PID_FILE)
		except OSError, e:
			logSys.error("Unable to remove PID file: %s" % e)
		logSys.info("Exiting Fail2ban")
	
	def quit(self):
		self.stopAllJail()
		
		self.__asyncServer.stop()
		
		try:
			self.__loggingLock.acquire()
			logging.shutdown()
		finally:
			self.__loggingLock.release()
	
	def addJail(self, name, backend):
		self.__jails.add(name, backend)
		
	def delJail(self, name):
		self.__jails.remove(name)
	
	def startJail(self, name):
		try:
			self.__lock.acquire()
			if not self.isAlive(name):
				self.__jails.get(name).start()
		finally:
			self.__lock.release()
	
	def stopJail(self, name):
		try:
			self.__lock.acquire()
			if self.isAlive(name):
				self.__jails.get(name).stop()
				self.delJail(name)
		finally:
			self.__lock.release()
	
	def stopAllJail(self):
		try:
			self.__lock.acquire()
			for jail in self.__jails.getAll():
				self.stopJail(jail)
		finally:
			self.__lock.release()
	
	def isAlive(self, name):
		return self.__jails.get(name).isAlive()
	
	def setIdleJail(self, name, value):
		self.__jails.get(name).setIdle(value)
		return True
	def getIdleJail(self, name):
		return self.__jails.get(name).getIdle()
	
	
	def addIgnoreIP(self, name, ip):
		self.__jails.getFilter(name).addIgnoreIP(ip)
	
	def delIgnoreIP(self, name, ip):
		self.__jails.getFilter(name).delIgnoreIP(ip)
	
	def getIgnoreIP(self, name):
		return self.__jails.getFilter(name).getIgnoreIP()
	
	def addLogPath(self, name, fileName):
		self.__jails.getFilter(name).addLogPath(fileName)
	
	def delLogPath(self, name, fileName):
		self.__jails.getFilter(name).delLogPath(fileName)
	
	def getLogPath(self, name):
		return [m.getFileName()
				for m in self.__jails.getFilter(name).getLogPath()]
	
	def setFindTime(self, name, value):
		self.__jails.getFilter(name).setFindTime(value)
	
	def getFindTime(self, name):
		return self.__jails.getFilter(name).getFindTime()
	def addFailRegex(self, name, value):
		self.__jails.getFilter(name).addFailRegex(value)
	
	def delFailRegex(self, name, index):
		self.__jails.getFilter(name).delFailRegex(index)
	
	def getFailRegex(self, name):
		return self.__jails.getFilter(name).getFailRegex()
	
	def addIgnoreRegex(self, name, value):
		self.__jails.getFilter(name).addIgnoreRegex(value)
	
	def delIgnoreRegex(self, name, index):
		self.__jails.getFilter(name).delIgnoreRegex(index)
	
	def getIgnoreRegex(self, name):
		return self.__jails.getFilter(name).getIgnoreRegex()
	
	def setMaxRetry(self, name, value):
		self.__jails.getFilter(name).setMaxRetry(value)
	
	def getMaxRetry(self, name):
		return self.__jails.getFilter(name).getMaxRetry()
	
	
	def addAction(self, name, value):
		self.__jails.getAction(name).addAction(value)
	
	def getLastAction(self, name):
		return self.__jails.getAction(name).getLastAction()
	
	def delAction(self, name, value):
		self.__jails.getAction(name).delAction(value)
	
	def setCInfo(self, name, action, key, value):
		self.__jails.getAction(name).getAction(action).setCInfo(key, value)
	
	def getCInfo(self, name, action, key):
		return self.__jails.getAction(name).getAction(action).getCInfo(key)
	
	def delCInfo(self, name, action, key):
		self.__jails.getAction(name).getAction(action).delCInfo(key)
	
	def setBanTime(self, name, value):
		self.__jails.getAction(name).setBanTime(value)
	
	def getBanTime(self, name):
		return self.__jails.getAction(name).getBanTime()
	
	def setActionStart(self, name, action, value):
		self.__jails.getAction(name).getAction(action).setActionStart(value)
	
	def getActionStart(self, name, action):
		return self.__jails.getAction(name).getAction(action).getActionStart()
		
	def setActionStop(self, name, action, value):
		self.__jails.getAction(name).getAction(action).setActionStop(value)
	
	def getActionStop(self, name, action):
		return self.__jails.getAction(name).getAction(action).getActionStop()
	
	def setActionCheck(self, name, action, value):
		self.__jails.getAction(name).getAction(action).setActionCheck(value)
	
	def getActionCheck(self, name, action):
		return self.__jails.getAction(name).getAction(action).getActionCheck()
	
	def setActionBan(self, name, action, value):
		self.__jails.getAction(name).getAction(action).setActionBan(value)
	
	def getActionBan(self, name, action):
		return self.__jails.getAction(name).getAction(action).getActionBan()
	
	def setActionUnban(self, name, action, value):
		self.__jails.getAction(name).getAction(action).setActionUnban(value)
	
	def getActionUnban(self, name, action):
		return self.__jails.getAction(name).getAction(action).getActionUnban()
		
	
	def status(self):
		try:
			self.__lock.acquire()
			jailList = ''
			for jail in self.__jails.getAll():
				jailList += jail + ', '
			length = len(jailList)
			if not length == 0:
				jailList = jailList[:length-2]
			ret = [("Number of jail", self.__jails.size()), 
				   ("Jail list", jailList)]
			return ret
		finally:
			self.__lock.release()
	
	def statusJail(self, name):
		return self.__jails.get(name).getStatus()
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	def setLogLevel(self, value):
		try:
			self.__loggingLock.acquire()
			self.__logLevel = value
			logLevel = logging.DEBUG
			if value == 0:
				logLevel = logging.FATAL
			elif value == 1:
				logLevel = logging.ERROR
			elif value == 2:
				logLevel = logging.WARNING
			elif value == 3:
				logLevel = logging.INFO
			logging.getLogger("fail2ban").setLevel(logLevel)
		finally:
			self.__loggingLock.release()
	
	
	
	
	
	
	
	def getLogLevel(self):
		try:
			self.__loggingLock.acquire()
			return self.__logLevel
		finally:
			self.__loggingLock.release()
	
	
	
	
	
	
	
	def setLogTarget(self, target):
		try:
			self.__loggingLock.acquire()
			
			formatter = logging.Formatter("%(asctime)s %(name)-16s: %(levelname)-6s %(message)s")
			if target == "SYSLOG":
				
				formatter = logging.Formatter("%(name)-16s: %(levelname)-6s %(message)s")
				facility = logging.handlers.SysLogHandler.LOG_DAEMON
				hdlr = logging.handlers.SysLogHandler("/dev/log", 
													  facility = facility)
			elif target == "STDOUT":
				hdlr = logging.StreamHandler(sys.stdout)
			elif target == "STDERR":
				hdlr = logging.StreamHandler(sys.stderr)
			else:
				
				try:
					open(target, "a").close()
					hdlr = logging.FileHandler(target)
				except IOError:
					logSys.error("Unable to log to " + target)
					logSys.info("Logging to previous target " + self.__logTarget)
					return False
			
			for handler in logging.getLogger("fail2ban").handlers:
				
				logging.getLogger("fail2ban").removeHandler(handler)
				handler.close()
			
			hdlr.setFormatter(formatter)
			logging.getLogger("fail2ban").addHandler(hdlr)
			
			if not self.__logTarget == None:
				logSys.info("Changed logging target to %s for Fail2ban v%s" %
						(target, version.version))
			
			self.__logTarget = target
			return True
		finally:
			self.__loggingLock.release()
	
	def getLogTarget(self):
		try:
			self.__loggingLock.acquire()
			return self.__logTarget
		finally:
			self.__loggingLock.release()
	
	def __createDaemon(self):
		""" Detach a process from the controlling terminal and run it in the
			background as a daemon.
		
			http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/278731
		"""
	
		try:
			
			
			
			
			
			
			pid = os.fork()
		except OSError, e:
			return((e.errno, e.strerror))	 
		
		if pid == 0:	   
	
			
			
			
			
			
			
			
			os.setsid()
		
			
			
			signal.signal(signal.SIGHUP, signal.SIG_IGN)
		
			try:
				
				
				
				
				
				pid = os.fork()		
			except OSError, e:
				return((e.errno, e.strerror))  
		
			if (pid == 0):	  
				
				
				os.chdir("/")
			else:
				os._exit(0)	  
		else:
			os._exit(0)		 
		
		
		
		
		try:
			maxfd = os.sysconf("SC_OPEN_MAX")
		except (AttributeError, ValueError):
			maxfd = 256	   
	
		for fd in range(0, maxfd):
			try:
				os.close(fd)
			except OSError:   
				pass
	
		
		os.open("/dev/null", os.O_RDONLY)	
		os.open("/dev/null", os.O_RDWR)		
		os.open("/dev/null", os.O_RDWR)		
		return True
class ServerInitializationError(Exception):
	pass
