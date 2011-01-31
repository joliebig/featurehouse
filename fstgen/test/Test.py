import os

@staticmethod
@entrymethod(cool=down)
def test(cool):
    print(cool)

class Test:
	@dbus.service.method
	def getVersion(self):
		try:
			pass
		except IOError:
			raise
		except KeyError:
			raise
