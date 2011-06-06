VERSION = '0.2'
IFACE_NAME = 'org.gnome.Listen'
IFACE_PATH = '/org/gnome/listen'
import CurrentSong
class Listen( CurrentSong.DbusBase ):
    '''Communicate with GNOME's Listen player'''
    def __init__(self):
        CurrentSong.DbusBase.__init__( self, IFACE_NAME, self.setInterface )
        try: self.iface
        except: self.iface = None
    def setInterface( self ):
        self.iface = self.bus.get_object( IFACE_NAME, IFACE_PATH )
    def check(self):
        message = self.iface.current_playing()
        if message != "":
            buf = message.split(" - (")
            title = buf[0]
            secbuf = buf[1].split(" - ")
            album = secbuf[0]
            artist = secbuf[1][0:-1]
            if title != self.title or \
               album != self.album or \
               artist != self.artist:
                self.title = title
                self.album = album
                self.artist = artist
                return True
	
        if not self.iface or not self.isNameActive(IFACE_NAME) or message == "":
            if self.artist != "" or self.title != "" or self.album != "":
	        self.artist = ""
	        self.title = ""
	        self.album = ""
	        return True
    def isPlaying(self):
        if not self.isRunning(): return False
        if not self.iface: return False
        try:
            self.iface.current_playing()
        except:
            return False
        return True
    def isRunning(self):
        return self.isNameActive(IFACE_NAME)
