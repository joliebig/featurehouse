import os
import gettext
class CurrentSong(object):
    customConfig = {}
    def __init__( self ):
        self.playing = ''
        self.artist = ''
        self.title = ''
        self.album = ''
        self.filename = ''
        style = '%title - %artist (%album)'
        self.setStyle( style )
        self.dictCommand = {
            'show': (self.cmd_Show,'Show playing song',True)
        }
        self._log = []
        self.status = 'unknown'
    def log(self, type, message):
        print "%s: %s" % (type, message)
        self._log.append((type, message))
    def getSongDict(self):
        songinfo = {}
        songinfo['artist'] = self.artist 
        songinfo['title'] = self.title
        songinfo['album'] = self.album
        return songinfo
    def getCurrentSong( self ):
        '''return the formated current song'''
        return self.parseStyle()
    def parseStyle( self ):
        '''return a parsed style according to the value of the variables'''
        if self.title == '' and self.artist == '' and self.album == '':
            return ''
        else:
            return self.style.replace('%artist', self.artist)\
                .replace('%title', self.title).replace('%album', self.album)
    def check( self ):
        '''check if there was a change in the song'''
        return False
    def isPlaying( self ):
        '''check if the player is playing'''
        return False
    def isRunning( self ):
        '''check if the player is running'''
        return False
    def getStatus( self ):
        '''check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )'''
        return ( True, 'OK' )
    def setStyle( self, string ):
        '''set the style'''
        self.style = '\\0Music\\01\\0' + string.replace('%title', '{0}').replace('%artist', '{1}').replace('%album', '{2}') + '\\0%title\\0%artist\\0%album\\0\\0'
    def is_on_path(self, fname):
        for p in os.environ['PATH'].split(os.pathsep):
            if os.path.isfile(os.path.join(p, fname)):
                return True
    def start( self ):
        pass
    def stop( self ):
        pass
    def getCoverPath( self ):
        return None
    def updateConfig( self ):
        pass
    def cmd_Show( self , *args):
        if self.artist == '' and self.album == '' and self.title == '':
            return ( False , 'Not Playing' )
        cm =  self.getCurrentSong()
        cm = cm[cm.find( '\\0Music\\01\\0')+12:]
        cmargs = cm.split('\\0')
        cm = cmargs[0]
        for args in range(1, len(cmargs)):
            cm = cm.replace( '{%s}' %str(args-1), cmargs[args])
        if cm == '':
            return ( False , 'Not Playing' )
        return ( True, cm )
ROOT_NAME = 'org.freedesktop.DBus'
ROOT_PATH = '/org/freedesktop/DBus'
DBUS = False
class DbusBase( CurrentSong ):
    def __init__( self, name = '', callback = None ):
        CurrentSong.__init__( self )
        global DBUS
        try:
            import dbus
            dbus_version = getattr(dbus, 'version', (0,0,0))
            if dbus_version >= (0,41,0) and dbus_version < (0,80,0):
                dbus.SessionBus()
                import dbus.glib
            elif dbus_version >= (0,80,0):
                from dbus.mainloop.glib import DBusGMainLoop
                DBusGMainLoop(set_as_default=True)
                dbus.SessionBus()
            else:
                self.log('error', 'python-dbus is too old!')
                raise
        except Exception, e:
            self.log('error', 'cant start dbus')
            DBUS = False
        else:
            DBUS = True
        if not DBUS:
            return
        self.module = dbus
        self.bus = dbus.SessionBus()
        self.root = self.bus.get_object( ROOT_NAME, ROOT_PATH )
        self.isNocWaiting = False
        if name and callback:
            self.reset( name, callback )
    def reset( self, name, callback ):
        self.log( 'info', 'reset player: ' + str(name) )
        self.status = 'not running'
        if self.isNameActive( name ):
            self.log( 'info', 'player running: ' + str(name) )
            self.status = 'running'
            callback()
        else:
            self.log( 'info', 'not running, listening NameOwnerChanged' )
            self.status = 'not running'
            def noc(changedName, *args):
                if str(changedName) == name and self.isNameActive(name):
                    self.log( 'info', 'player running: ' + str(name) )
                    self.status = 'running'
                    callback()
                    self.isNocWaiting = False
            self.isNocWaiting = True
            self.bus.add_signal_receiver( noc, 'NameOwnerChanged', \
                                          ROOT_NAME, None, ROOT_PATH )
    def setCurrentSongData( self ):
        self.artist = ''
        self.title = ''
        self.album = ''
        self.filename = ''
    def getStatus( self ):
        '''don't override this'''
        global DBUS
        if os.name != 'posix':
            return ( False, _( 'This plugin only works in posix systems' ) )
        if not DBUS:
            return ( False, _( 'D-Bus cannot be initialized' ) )
        return ( True, 'Ok' )
    def isPlaying( self ):
        return False
    def check( self ):
        return False
    def isNameActive( self, name ):
        '''a helper for your class, so don't override it'''
        return bool( self.root.NameHasOwner(name) )
