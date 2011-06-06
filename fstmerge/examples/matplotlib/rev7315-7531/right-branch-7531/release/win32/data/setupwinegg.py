from distutils import cygwinccompiler
try:
	cygwinccompiler.get_msvcr
	cygwinccompiler.get_msvcr = lambda: []
except AttributeError:
	pass
from setuptools import setup
execfile('setup.py',
         {'additional_params' :
         {'namespace_packages' : ['mpl_toolkits']}})
