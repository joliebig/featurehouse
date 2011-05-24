__author__ = "Cyril Jaquier"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2010-07-25 12:46:39 $"
__copyright__ = "Copyright (c) 2004 Cyril Jaquier"
__license__ = "GPL"
from jail import Jail
from threading import Lock
class Jails:
	
	
	
	
	def __init__(self):
		self.__lock = Lock()
		self.__jails = dict()
	
	
	
	
	
	
	
	
	
	def add(self, name, backend):
		try:
			self.__lock.acquire()
			if self.__jails.has_key(name):
				raise DuplicateJailException(name)
			else:
				self.__jails[name] = Jail(name, backend)
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	
	def remove(self, name):
		try:
			self.__lock.acquire()
			if self.__jails.has_key(name):
				del self.__jails[name]
			else:
				raise UnknownJailException(name)
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	
	def get(self, name):
		try:
			self.__lock.acquire()
			if self.__jails.has_key(name):
				jail = self.__jails[name]
				return jail
			else:
				raise UnknownJailException(name)
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	
	def getAction(self, name):
		try:
			self.__lock.acquire()
			if self.__jails.has_key(name):
				action = self.__jails[name].getAction()
				return action
			else:
				raise UnknownJailException(name)
		finally:
			self.__lock.release()
	
	
	
	
	
	
	
	
	def getFilter(self, name):
		try:
			self.__lock.acquire()
			if self.__jails.has_key(name):
				action = self.__jails[name].getFilter()
				return action
			else:
				raise UnknownJailException(name)
		finally:
			self.__lock.release()
	
	
	
	
	
	
	def getAll(self):
		try:
			self.__lock.acquire()
			return self.__jails.copy()
		finally:
			self.__lock.release()
	
	
	
	
	
	
	def size(self):
		try:
			self.__lock.acquire()
			return len(self.__jails)
		finally:
			self.__lock.release()
class DuplicateJailException(Exception):
	pass
class UnknownJailException(Exception):
	pass
