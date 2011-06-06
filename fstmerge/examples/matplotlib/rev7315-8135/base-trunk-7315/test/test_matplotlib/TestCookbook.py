"""The Cookbook unit-test class implementation."""
from mplTest import *
import matplotlib
matplotlib.use( "Agg", warn = False )
import numpy as npy
import matplotlib.cbook as cbook
class TestCookbook( MplTestCase ):
   """Cookbook unit test class."""
   tags = [
          ]
   def setUp( self ):
      """Setup any data needed for the unit test."""
      pass
   def tearDown( self ):
      """Clean-up any generated files here."""
      pass
   def test_is_string_like( self ):
      """Test the 'is_string_like cookbook' function."""
      y = npy.arange( 10 )
      self.failUnless( cbook.is_string_like( y ) == False )
      y.shape = 10, 1
      self.failUnless( cbook.is_string_like( y ) == False )
      y.shape = 1, 10
      self.failUnless( cbook.is_string_like( y ) == False )
      self.failUnless( cbook.is_string_like( "hello world" ) )
      self.failUnless( cbook.is_string_like(10) == False )
