import os
import gobject
import gettext
ERROR = ''
try:
    import win32api
    import win32gui
except:
    ERROR = 'cannot import needed modules'
import CurrentSong
from emesenecommon import *
class Winamp( CurrentSong.CurrentSong ):
    def __init__( self ):
        CurrentSong.CurrentSong.__init__( self )
        self.winamp = None
        self.isRunning()
        self.currentSong = ''
    def isPlaying( self ):
        if self.winamp and win32api.SendMessage(self.winamp, 0x400, 0, 104) != 1:
            return False
        return True
    def isRunning( self ):
        try:
            self.winamp = win32gui.FindWindow('Winamp v1.x', None)
            return True
        except: 
            return False
    def getCurrentSong( self ):
        return self.currentSong
    def check( self ):
        if not self.isRunning(): return False
        if self.isPlaying():
            newCurrentSong = 'Winamp\\0Music\\01\\0{0}\\0%s\\0\\0' % \
                win32gui.GetWindowText( self.winamp ).replace( ' - Winamp' , '' )
            print newCurrentSong
        else:
            newCurrentSong = ''
        if self.currentSong != newCurrentSong:
            self.currentSong = newCurrentSong
            return True
        return False
    def getStatus( self ):
        '''
        check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )
        '''
        if os.name != 'nt':
            return ( False, 'This plugin only works on windows systems' )
        if ERROR != '':
            return ( False, ERROR )
        return ( True, 'Ok' )
