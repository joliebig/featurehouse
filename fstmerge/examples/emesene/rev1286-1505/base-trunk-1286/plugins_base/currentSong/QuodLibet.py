IFACE_NAME = 'net.sacredchao.QuodLibet'
IFACE_PATH = '/net/sacredchao/QuodLibet'
import os
import CurrentSong
class QuodLibet( CurrentSong.DbusBase ):
    def __init__( self ):
        CurrentSong.DbusBase.__init__( self, IFACE_NAME, self.setInterface )
        try: self.iface
        except: self.iface = None
    def setInterface( self ):
        self.iface = self.bus.get_object( IFACE_NAME, IFACE_PATH )
    def isPlaying( self ):
        return self.isNameActive( IFACE_NAME )
    def check( self ):
        if not self.iface:
            return False
        artist = ''
        title = ''
        album = ''
        filename = ''
        if self.isPlaying():
            data = dict(self.iface.CurrentSong())
            if 'title' in data: title = data['title']
            if 'album' in data: album = data['album']
            if 'artist' in data: artist = data['artist']
            if '~filename' in data:
                filename = data['~filename']
                filename = filename[filename.rfind('/') +1:-1]
        if self.artist != artist or \
           self.title != title or \
           self.filename != filename:
            self.artist = artist
            self.title = title
            self.album = album
            self.filename = filename
            return True
        return False
    def getStatus( self ):
        '''check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )'''
        if os.name != 'posix':
            return (False, _('This plugin only works in posix systems'))
        if not self.iface:
            return (False, _('You don\'t have dbus. '
                'Try to install dbus and/or use a dbus enabled '
                'version of quodlibet.'))
        return ( True, 'Ok' )
