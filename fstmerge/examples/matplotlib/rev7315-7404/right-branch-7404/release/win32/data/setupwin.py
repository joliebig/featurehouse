from distutils import cygwinccompiler
try:
	cygwinccompiler.get_msvcr
	cygwinccompiler.get_msvcr = lambda: []
except AttributeError:
	pass
execfile('setup.py')
