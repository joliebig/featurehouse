import os

class Test:
	@dbus.service.method
	def getVersion(self):
		try:
			pass
		except IOError:
			raise
		except KeyError:
			raise