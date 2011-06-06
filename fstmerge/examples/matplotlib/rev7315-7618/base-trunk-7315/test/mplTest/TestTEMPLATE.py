"""The UNITTEST unit-test class implementation."""
from mplTest import *
import matplotlib
matplotlib.use( "Agg", warn = False )
import pylab
import numpy as npy
class TestUNITTEST( MplTestCase ):
   """UNITTEST unit test class."""
   tags = [
          ]
   def setUp( self ):
      """Setup any data needed for the unit test."""
      pass
   def tearDown( self ):
      """Clean-up any generated files here."""
      pass
   def test_case_001( self ):
      """TODO: A very brief description of the test case."""
      fname = self.outFile( "test_case_001a" )
      fout = open( fname, 'w' )
      fout.write( "A UNITTEST.test_case_001 output file.\n" )
      fout.close()
      fname = self.outFile( "test_case_001b" )
      fout = open( fname, 'w' )
      fout.write( "Another UNITTEST.test_case_001 output file.\n" )
      fout.close()
      pass
