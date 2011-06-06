VERSION = '0.3'
import os
import struct
import socket
if os.name == 'posix':
    import pwd
import CurrentSong
ERROR = ''
CMD_GET_VERSION = 0         #
CMD_PLAYLIST_ADD = 1        #
CMD_PLAY = 2                #
CMD_PAUSE = 3               #
CMD_STOP = 4                #
CMD_IS_PLAYING = 5          #
CMD_IS_PAUSED = 6           #
CMD_GET_PLAYLIST_POS = 7    #
CMD_SET_PLAYLIST_POS = 8    #
CMD_GET_PLAYLIST_LENGTH = 9 #
CMD_PLAYLIST_CLEAR = 10     #
CMD_GET_OUTPUT_TIME = 11    #
CMD_JUMP_TO_TIME = 12       #
CMD_GET_VOLUME = 13         #
CMD_SET_VOLUME = 14         #
CMD_GET_SKIN = 15           #
CMD_SET_SKIN = 16
CMD_GET_PLAYLIST_FILE = 17  #
CMD_GET_PLAYLIST_TITLE = 18 #
CMD_GET_PLAYLIST_TIME = 19  #
CMD_GET_INFO = 20           #
CMD_GET_EQ_DATA = 21 # these two aren't implemented in
CMD_SET_EQ_DATA = 22 # xmms 1.0.1, and that's all I have
CMD_PL_WIN_TOGGLE = 23      #
CMD_EQ_WIN_TOGGLE = 24      #
CMD_SHOW_PREFS_BOX = 25     #
CMD_TOGGLE_AOT = 26         #
CMD_SHOW_ABOUT_BOX = 27     # doesn't seem to work :-(
CMD_EJECT = 28              #
CMD_PLAYLIST_PREV = 29      #
CMD_PLAYLIST_NEXT = 30      #
CMD_PING = 31
CMD_GET_BALANCE = 32        #
CMD_TOGGLE_REPEAT = 33      #
CMD_TOGGLE_SHUFFLE = 34     #
CMD_MAIN_WIN_TOGGLE = 35    #
CMD_PLAYLIST_ADD_URL_STRING = 36
CMD_IS_EQ_WIN = 37          #
CMD_IS_PL_WIN = 38          #
CMD_IS_MAIN_WIN = 39        #
class ClientPacketHeader:
    def __init__(self):
        self.version,self.cmd,self.length = 0,0,0
    def __repr__(self):
        return "<< %s : version: %s cmd: %s length: %s >>"\
            %(self.__class__.__name__,self.version,self.cmd,self.length)
    def encode(self):
        return struct.pack("=hhl",self.version,self.cmd,self.length)
class XmmsSocket:
    def __init__(self,session=0):
        self.status = True
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        if os.path.exists( "/tmp/xmms_%s.%d"%(pwd.getpwuid(os.geteuid())[0],session) ):
            sockFile = "/tmp/xmms_%s.%d"%(pwd.getpwuid(os.geteuid())[0],session)
        else:
            self.status = False
            return
        if sockFile == None: return
        try:
            self.sock.connect( sockFile )
        except:
            self.status = False
    def send(self,cmd,data=""):
        if type(data) == type(1):
            data = struct.pack("=l",data)
        packet = struct.pack("=hhl",1,cmd,len(data))+data
        self.sock.send(packet)
    def sendf(self,cmd,fmt,args):
        data = apply(struct.pack,(fmt,)+args)
        self.send(cmd,data)
    def read_header(self, sock):
        head = ClientPacketHeader()
        head.version, head.cmd, head.length = \
            struct.unpack("=hhl",sock.recv(struct.calcsize( "=hhl" )))
        return head
    def get_reply(self):
        self.reply_header = self.read_header(self.sock)
        return self.sock.recv(self.reply_header.length)\
            .decode('utf-8', 'replace').encode('utf-8')
    def get_replyf(self,fmt):
        return struct.unpack(fmt,self.get_reply())
    def read_ack(self):
        self.get_reply()
    def isXmmsOpen(self):
        return self.status
class XmmsCommands:
    def __init__(self):
        pass
    def send_requestf(self, cmd, fmt, data=""):
        xmmsSocket = XmmsSocket()
        if xmmsSocket.isXmmsOpen():
            xmmsSocket.send(cmd, data)
            return xmmsSocket.get_replyf(fmt)
        return False
    def send_command(self, cmd, data=""):
        xmmsSocket = XmmsSocket()
        if xmmsSocket.isXmmsOpen():
            xmmsSocket.send(cmd,data)
        return False
    def send_request_args(self, cmd, fmt, *args):
        xmmsSocket = XmmsSocket()
        if xmmsSocket.isXmmsOpen():
            xmmsSocket.sendf(cmd, fmt, args)
            return xmmsSocket.get_reply()
        return False
    def playlist_pos(self):
        return self.send_requestf(CMD_GET_PLAYLIST_POS,'=l')[0]
    def playlist_file(self , pos):
        return self.send_request_args(CMD_GET_PLAYLIST_FILE,'=l',pos)[:-1]
    def playlist_title(self, pos):
        return self.send_request_args(CMD_GET_PLAYLIST_TITLE,'=l',pos)[:-1]
    def playlist_time(self, pos):
        return self.send_requestf_args(CMD_GET_PLAYLIST_TIME,'=l','=l',pos)[0]
    def socket_info(self):
        """returns (bitrate, freq, nch)"""
        return self.send_requestf(CMD_GET_INFO, '=lll')
    def socket_playlist_length(self):
        return self.send_requestf(CMD_GET_PLAYLIST_LENGTH,'=l')[0]
    def socket_output_time(self):
        return self.send_requestf(CMD_GET_OUTPUT_TIME,'=l')[0]
    def currentSong(self):
        return self.playlist_title(self.playlist_pos())
    def nextSong( self ):
        self.send_command(CMD_PLAYLIST_NEXT)
    def isPlaying(self):
        if XmmsSocket().isXmmsOpen():
            return self.send_requestf(CMD_IS_PLAYING,'=l')[0]
        return False
class Xmms(CurrentSong.CurrentSong):
    def __init__( self ):
        CurrentSong.CurrentSong.__init__( self )
        self.currentSong = ''
        self.xmms = XmmsCommands()
    def getSongPlain( self ):
        if not self.isPlaying():
            return ''
        return self.xmms.currentSong()
    def getCurrentSong( self ):
        if not self.isPlaying():
            return ''
        song = '\\0Music\\01\\0' + self.xmms.currentSong() + '\\0\\0'
        return song
    def nextSong( self ):
        self.xmms.nextSong()
    def isPlaying( self ):
        if self.xmms.isPlaying():
            return True
        return False
    def isRunning( self ):
        if self.xmms.isPlaying():
            return True
        return False
    def getStatus( self ):
        if os.name != 'posix':
            return ( False,  'This plugin only works in posix systems' )
        if ERROR:
            return ( False, ERROR )
        return ( True, 'Ok' )
    def check( self ):
        if self.currentSong != self.getCurrentSong():
            self.currentSong = self.getCurrentSong()
            return True
        return False
