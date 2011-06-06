"""
   TODO-NOTES:
   Command-line executable that runs the tests.
      -- nice report on test pass/fail status
      -- hooks to add coverage checking and reporting
   Utilities
      -- image comparison tools (non-PIL dependant)
"""
import os
import sys
import os.path
originalStdout = sys.stdout
originalStderr = sys.stderr
cwd = os.path.abspath( os.getcwd() )
root = os.path.dirname( os.path.abspath( sys.argv[0] ) )
sys.path = [ root ] + sys.path
args = [ arg for arg in sys.argv ]
if root in cwd:
   working = cwd
else:
   working = root
if '--all' in args:
   working = root
os.chdir( working )
import nose
from mplTest import MplNosePlugin, path_utils
if '--clean' in args:
   for filename in path_utils.walk( working ):
      ext = path_utils.extension( filename )
      if ext == '.cover':
         print "Cleaning coverage file: %s" % (filename)
         path_utils.rm( filename )
      elif ext == '.pyc':
         print "Cleaning bytecode file: %s" % (filename)
         path_utils.rm( filename )
      elif path_utils.name( filename ) == 'saved-results':
         print "Cleaning directory:     %s" % (filename)
         path_utils.rmdir( filename )
   sys.exit( 0 )
for arg in args:
   if arg.startswith( '--make-test=' ):
      testname = arg[ 12: ]
      if (testname[0] == '"' and testname[-1] == '"') or \
         (testname[0] == "'" and testname[-1] == "'"):
         testname = testname[1:-1]
      filename = os.path.join( cwd, 'Test' + testname + '.py' )
      templName = os.path.join( root, 'mplTest', "TestTEMPLATE.py" )
      fin = open( templName, "r" )
      fout = open( filename, "w" )
      lines = fin.readlines()
      for line in lines:
         newline = line.replace( 'UNITTEST', testname )
         fout.write( newline )
      fin.close()
      fout.close()
      print "Generated '%s'" % (filename)
      sys.exit( 0 )
nose.run( argv = args,
          plugins = [ MplNosePlugin() ] )
sys.stdout = originalStdout
sys.stderr = originalStderr
