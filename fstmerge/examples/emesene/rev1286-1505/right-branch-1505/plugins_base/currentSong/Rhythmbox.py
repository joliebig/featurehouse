VERSION = '0.2'
OBJECT_NAME = 'org.gnome.Rhythmbox'
PLAYER_NAME = 'org.gnome.Rhythmbox.Player'
PLAYER_PATH = '/org/gnome/Rhythmbox/Player'
SHELL_NAME = 'org.gnome.Rhythmbox.Shell'
SHELL_PATH = '/org/gnome/Rhythmbox/Shell'
import CurrentSong
class Rhythmbox( CurrentSong.DbusBase ):
    '''Rhythmbox interface'''
    def __init__( self ):
        CurrentSong.DbusBase.__init__( self, OBJECT_NAME, self.setInterface )
        try: self.iface
        except: self.iface = None
        try: self.rbshell
        except: self.rbshell = None
        self.uri = ''
    def setInterface( self ):
        rbobj = self.bus.get_object( OBJECT_NAME, PLAYER_PATH )
        self.iface = self.module.Interface( rbobj, PLAYER_NAME )
        rbshellobj = self.bus.get_object( OBJECT_NAME, SHELL_PATH )
        self.rbshell = self.module.Interface( rbshellobj, SHELL_NAME )
    def setCurrentSongData( self, uri ):
        if not self.rbshell: return
        data = self.rbshell.getSongProperties( uri )
        self.title = data['title']
        self.artist = data['artist']
        self.album = data['album']
        self.uri = uri
    def check( self ):
        if not self.iface or not self.isNameActive(OBJECT_NAME):
            return
        uri = self.iface.getPlayingUri()
        if uri != self.uri:
            self.setCurrentSongData( uri )
            return True
        return False
    def isPlaying( self ):
        if not self.iface:
            return False
        else:
            return bool( self.iface.getPlaying() )
