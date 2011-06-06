VERSION = '0.02'
IFACE_NAME = 'org.gnome.Banshee'
IFACE_PATH = '/org/gnome/Banshee/Player'
import CurrentSong
class Banshee( CurrentSong.DbusBase ):
    def __init__( self ):
        CurrentSong.DbusBase.__init__( self, IFACE_NAME, self.setInterface )
        try: self.iface
        except: self.iface = None
    def setInterface( self ):
        self.iface = self.bus.get_object( IFACE_NAME, IFACE_PATH )
    def setCurrentSongData( self ):
        if self.iface:
            self.title = self.iface.GetPlayingTitle()
            self.artist = self.iface.GetPlayingArtist()
            self.album = self.iface.GetPlayingAlbum()
    def isPlaying( self ):
        if not self.iface: return False
        if not self.iface.GetPlayingTitle():
            print "nao passou teste"
            return False 
        if self.iface.GetPlayingTitle() != None:
            print "passou teste"
            return True
        return False
    def check( self ):
        if not self.iface or not self.isNameActive(IFACE_NAME):
            return
        if self.iface.GetPlayingTitle() != self.title:
            self.setCurrentSongData()
            return True
        return False
