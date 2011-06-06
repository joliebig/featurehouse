VERSION = '0.1'
import CurrentSong
import sys
import gettext
import os
import socket
import urllib, urllib2
def _extract_fields(reply, fields):
    """Return a dict associating a value to each requested field from a MPD
    reply.
    Parameters:
        reply - MPD reply (string)
        fields - sequence of fields (lowercase) we'll associate a value to.
    Here's an example of an MPD reply:
        Artist: foo
        Title: bar
        Album: other
        OK
    """
    ret = {}
    for field in fields:
        ret[field] = ""
    for line in reply.splitlines():
        if ":" in line:
            field, value = line.split(":", 1)
            field = field.strip().lower()
            if field in fields:
                ret[field] = value.strip()
    return ret
RETRYNUM = 1000
class Mpd( CurrentSong.CurrentSong ):
    customConfig = {
        'host': '127.0.0.1',
        'port': '6600',
	'password': '',
    }
    def __init__(self):
        CurrentSong.CurrentSong.__init__(self)
        self._socket = None
        self._retNum = 0
        self.song_info = {
            'artist': '',
            'title': '',
            'album': '',
        }
        dictCommandMPD = {
            'play': ( self.cmd_Play, 'Start playing',False ),
            'stop': ( self.cmd_Stop, 'Stop playing',False ),
            'pause': (self.cmd_Pause, 'Pause player',False ),
            'next': (self.cmd_Next, 'Jump to next song',False),
            'prev': (self.cmd_Prev, 'Jump to previous song',False),
            'getvol': (self.cmd_GetVolume, 'get volume',False),
            'setvol': (self.cmd_SetVolume, 'set volume <int vol>. The range of volume is 0-100',False)
        }
        self.dictCommand.update(dictCommandMPD)
    def updateConfig(self):
        if self.connected():
            self._socket.close()
            self._socket = None
        self._connect()
        for item in self.song_info.keys():
            self.song_info[item] = ""
    def tryReconnect():
        if self._retNum > RETRYNUM:
            self._retNum = 0
            self._connect()
        else:
            self._retNum += 1
    def _getSocket(self):
        if self._socket == None:
            self.tryReconnect()
        return self._socket
    def _connect(self):
        if self.connected():
            return
        self.log('info', 'trying to connect')
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            self._socket.connect((self.customConfig.get('host', 
                '127.0.0.1'), int(self.customConfig.get('port', 6600))))
            if not self._socket.recv(128).startswith("OK"):
                raise
            if self.customConfig.get('password', ''):
                self.log('info', 'sending password')
                self._socket.send('password ' +  self.customConfig['password'] +'\n')
                if not self._socket.recv(128).startswith("OK"):
                    self.log('error', 'wrong password')
                    raise socket.error
        except socket.error, msg:
            self.log('error', 'can\'t connect to %s:%s' % \
                (self.customConfig.get('host', '127.0.0.1'), 
                    self.customConfig.get('port', 6600)))
            self.status = 'not running'
            self._socket.close()
            self._socket = None
        else:
            self.log('info', 'connected sucessfully')
            self.status = 'connected'
        return 
    def connected(self):
        if self._socket is None:
            return False
        return True
    def start(self):
        self._connect()
        return True
    def check(self):
        if self.checkSongChanged():
            self.artist = self.getArtist()
            self.title = self.getTitle()
            self.album = self.getAlbum()
            return True
        return False
    def stop(self):
        '''Close connection'''
        if self.connected():
            self._socket.close()
        self._socket = None
    def setHost(self, host):
        self.host = host
    def setPort(self, port):
        self.port = port
    def setPassword(self, password):
        self.password = password
    def getArtist(self):
        return self.song_info["artist"]
    def getAlbum(self):
        return self.song_info["album"]
    def getTitle(self):
        return self.song_info["title"]
    def sendCmd(self, cmd):
        try:
            sock = self._getSocket()
            sock.send("%s\n" % cmd)
            ret = sock.recv(1024)
            if ret.startswith("ACK"):
                return None
            return ret
        except:
            return None
    def isPlaying(self):
        status = self.sendCmd("status")
        if status is not None:
            if _extract_fields(status, ["state"])["state"] == "play":
                return True
        return False
    def checkSongChanged(self):
        if not self.isPlaying():
            if "".join(self.song_info.values()) != "":
                for item in self.song_info.keys():
                    self.song_info[item] = ""      
                return True
        else: 
            song_rpl = self.sendCmd("currentsong")
            if song_rpl is not None:
                song_info = _extract_fields(song_rpl, self.song_info.keys())
                if self.song_info != song_info:
                    self.song_info = song_info
                    return True
        return False
    def cmd_Pause(self,args):
        ret = self.sendCmd('pause')
        if ret != None and ret.startswith('OK'):
            return ( True, 'Player Paused' )
        return ( False, 'Error Pausing Player' )
    def cmd_Play(self,args):
        ret = self.sendCmd('play')
        if ret != None and ret.startswith('OK'):
            return ( True, 'Player start playing' )
        return ( False, 'Error on playing' )
    def cmd_Stop(self,args):
        ret = self.sendCmd('stop')
        if ret != None and ret.startswith('OK'):
            return ( True, 'Player Stopped' )
        return ( False, 'Error on Stopping Player' )
    def cmd_Next(self,args):
        ret = self.sendCmd('next')
        if ret != None and ret.startswith('OK'):
            return ( True, 'Next Track' )
        return ( False, 'Error jumping to next track' )
    def cmd_Prev(self,args):
        ret = self.sendCmd('prev')
        if ret != None and ret.startswith('OK'):
            return ( True, 'Previous Track' )
        return ( False, 'Error jumping to previos track' )
    def cmd_GetVolume(self,args):
        ret = self.sendCmd('status')
        if ret != None:
            try:
                volume = _extract_fields(ret,['volume'])['volume']
                return ( True, 'Volume: ' + str(volume))
            except:
                pass
        return ( False, 'Error Getting Volume' )
    def cmd_SetVolume(self,args):
        try:
            vol = int(args)
            if vol < 0 or vol > 100:
                raise
        except:
            return ( False, 'setvol needs an integer value,range 0-100' )
        ret = self.sendCmd('setvol ' + str(vol))
        if ret != None and ret.startswith('OK'):
            return ( True, 'Volume setted to ' + str(vol) )
        return ( False, 'Error Setting Volume' )
