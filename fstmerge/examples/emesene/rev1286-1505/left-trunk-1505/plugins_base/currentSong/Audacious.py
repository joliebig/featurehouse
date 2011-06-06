VERSION = '0.2'
IFACE_NAME = 'org.atheme.audacious'
IFACE_PATH = '/org/atheme/audacious'
import os
import commands
import CurrentSong
class Audacious( CurrentSong.DbusBase ):
    def __init__( self ):
        CurrentSong.DbusBase.__init__( self, IFACE_NAME, self.setInterface )
        try:
            self.iface
        except:
            self.iface = None # We don't run a recent Audacious with dbus support
            self.playingNow = '' # We'll use that variable to hold the filename as returned by Audacious
    def setInterface( self ):
        self.iface = self.bus.get_object( IFACE_NAME, IFACE_PATH )
    def isPlaying( self ):
        if self.iface:
            return self.iface.Status() == "playing" #Note: self.iface.Playing() returns True even if it's paused
        else:
            return commands.getoutput("audtool playback-status") == "playing"
    def getStatus( self ):
        '''check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )'''
        if os.name != 'posix':
            return ( False, _( 'This plugin only works in posix systems' ) ) #no posix here
        if not self.is_on_path( 'audtool' ) and not self.iface:
            return ( False, _( 'audtool is not on the path and you don\'t have dbus enabled.' ) ) #no audtool here
        return ( True, 'Ok' ) #ok, run baby!
    def getCurrentSong( self ):
        if not self.iface:
            if self.isPlaying():
                song = commands.getoutput( "audtool current-song" )
                fsong = '\\0Music\\01\\0' + song + '\\0\\0'
                return fsong
            else:
                return ''
        else: #Use the parseStyle from the super class
            return self.parseStyle()
    def check( self ):
        if self.iface:
            if self.isPlaying():
                songPosition = self.iface.Position()
                artist = self.iface.SongTuple(songPosition, "artist") 
	        title = self.iface.SongTuple(songPosition, "title") 
	        album = self.iface.SongTuple(songPosition, "album") 
	        if self.artist != artist or self.title != title or self.album != album:
	            self.artist = artist
	            self.title = title
	            self.album = album
	            self.filename = self.iface.SongTitle(songPosition)
	            return True
	    else: # Just stopped, paused, ... set the data as empty strings
	        if self.artist != "" or self.title != "" or self.album != "":
	            self.artist = ""
	            self.title = ""
	            self.album = ""
	            self.filename = ""
	            return True
        elif self.isPlaying():
            currentSong = self.getCurrentSong()
            if self.playingNow != currentSong:
                self.playingNow = currentSong
                return True
        elif self.playingNow != "": # Just stopped, paused ... set to an empty string
            self.playingNow = ""
            return True
        return False
