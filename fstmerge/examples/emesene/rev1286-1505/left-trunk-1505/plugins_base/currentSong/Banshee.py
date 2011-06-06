VERSION = '1.0'
IFACE_NAME = 'org.bansheeproject.Banshee'
IFACE_PATH = '/org/bansheeproject/Banshee/PlayerEngine'
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
            info = self.iface.GetCurrentTrack()
            self.title = info["name"]
            self.artist = info["artist"]
            self.album = info["album"]
    def isPlaying( self ):
        if not self.isNameActive(IFACE_NAME):
            return False
        if not self.iface:
            return False
        if self.iface.GetCurrentState() == "playing":
            return True
        else:
            return False
    def check( self ):
        if not self.isNameActive(IFACE_NAME):
            return False
        if not self.iface:
            return False
        if self.iface.GetCurrentState() == "playing":
            if self.iface.GetCurrentTrack()["name"] != self.title:
                self.setCurrentSongData()
                return True
        return False
