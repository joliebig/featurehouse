VERSION = '0.3'
import os
import CurrentSong
error = False
dcopext = None
pydcop = None
try:
    import dcopext
except:
    try:
        import pydcop
    except:
        error = True
class Amarok( CurrentSong.CurrentSong ):
    def __init__( self ):
        CurrentSong.CurrentSong.__init__( self )
        self.amarok = None
        if dcopext:
            self.client = dcopext.DCOPClient()
            self.client.attach()
        self.check()
    def _isServerRunning( self ):
        try:
            assert os.system( "ps aux|grep dcopserver|grep -v grep" ) == 0
        except:
            return False
        else:
            return True
    def isRunning( self ):
        if pydcop:
            return bool(self._isServerRunning() and "amarok" in pydcop.apps())
        elif dcopext:
            return bool(self.client.isApplicationRegistered('amarok'))
    def isPlaying( self ):
        if self.amarok:
            if pydcop and self.amarok.player.isPlaying():
                return True
            elif dcopext and self.amarok.player.isPlaying()[1]:
                return True
        return False
    def getStatus( self ):
        '''check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )'''
        global error
        if error:
            return ( False, _( "Can't import dcopext or python-dcop!" ) )
        if pydcop and not self._isServerRunning():
            return ( False, _( "dcop not running" ) )
        if os.name != 'posix':
            return ( False, _( 'This plugin only works in posix systems' ) )
        if pydcop and not self.is_on_path( 'dcop' ):
            return ( False, _( 'Dcop not found' ) )
        return ( True, 'Ok' )
    def check( self ):
        global error
        if error:
            return False
        if self.amarok == None and self.isRunning():
            if dcopext:
                self.amarok = dcopext.DCOPApp("amarok", self.client)
            elif pydcop:
                self.amarok = pydcop.DCOPApplication( "amarok" )
        if self.isPlaying():
            if dcopext:
                artist = self.amarok.player.artist()[1]
                title = self.amarok.player.title()[1]
                album = self.amarok.player.album()[1]
            elif pydcop:
                artist = self.amarok.player.artist()
                title = self.amarok.player.title()
                album = self.amarok.player.album()
            if self.artist != artist or self.title != title:
                self.artist = artist
                self.title = title
                self.album = album
                return True
        return False
