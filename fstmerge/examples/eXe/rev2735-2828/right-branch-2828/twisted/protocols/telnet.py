"""TELNET implementation, with line-oriented command handling.
"""
import warnings
warnings.warn(
    "As of Twisted 2.1, twisted.protocols.telnet is deprecated.  "
    "See twisted.conch.telnet for the current, supported API.",
    DeprecationWarning,
    stacklevel=2)
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO
from twisted import copyright
from twisted.internet import protocol
ESC =            chr(27) # ESC for doing fanciness
BOLD_MODE_ON =   ESC+"[1m" # turn bold on
BOLD_MODE_OFF=   ESC+"[m"  # no char attributes
NULL =            chr(0)  # No operation.
LF   =           chr(10)  # Moves the printer to the
CR =             chr(13)  # Moves the printer to the left
BEL =             chr(7)  # Produces an audible or
BS  =             chr(8)  # Moves the print head one
HT  =             chr(9)  # Moves the printer to the
VT =             chr(11)  # Moves the printer to the
FF =             chr(12)  # Moves the printer to the top
SE =            chr(240)  # End of subnegotiation parameters.
NOP=            chr(241)  # No operation.
DM =            chr(242)  # "Data Mark": The data stream portion
BRK=            chr(243)  # NVT character Break.
IP =            chr(244)  # The function Interrupt Process.
AO =            chr(245)  # The function Abort Output
AYT=            chr(246)  # The function Are You There.
EC =            chr(247)  # The function Erase Character.
EL =            chr(248)  # The function Erase Line
GA =            chr(249)  # The Go Ahead signal.
SB =            chr(250)  # Indicates that what follows is
WILL =          chr(251)  # Indicates the desire to begin
WONT =          chr(252)  # Indicates the refusal to perform,
DO =            chr(253)  # Indicates the request that the
DONT =          chr(254)  # Indicates the demand that the
IAC =           chr(255)  # Data Byte 255.
ECHO  =           chr(1)  # User-to-Server:  Asks the server to send
SUPGA =           chr(3)  # Supress Go Ahead...? "Modern" telnet servers
LINEMODE =       chr(34)  # I don't care that Jon Postel is dead.
HIDE  =         chr(133)  # The intention is that a server will send
NOECHO=         chr(131)  # User-to-Server:  Asks the server not to
iacBytes = {
    DO:   'DO',
    DONT: 'DONT',
    WILL: 'WILL',
    WONT: 'WONT',
    IP:   'IP'
    }
def multireplace(st, dct):
    for k, v in dct.items():
        st = st.replace(k, v)
    return st
class Telnet(protocol.Protocol):
    """I am a Protocol for handling Telnet connections. I have two
    sets of special methods, telnet_* and iac_*.
    telnet_* methods get called on every line sent to me. The method
    to call is decided by the current mode. The initial mode is 'User';
    this means that telnet_User is the first telnet_* method to be called.
    All telnet_* methods should return a string which specifies the mode
    to go into next; thus dictating which telnet_* method to call next.
    For example, the default telnet_User method returns 'Password' to go
    into Password mode, and the default telnet_Password method returns
    'Command' to go into Command mode.
    The iac_* methods are less-used; they are called when an IAC telnet
    byte is received. You can define iac_DO, iac_DONT, iac_WILL, iac_WONT,
    and iac_IP methods to do what you want when one of these bytes is
    received."""
    gotIAC = 0
    iacByte = None
    lastLine = None
    buffer = ''
    echo = 0
    delimiters = ['\r\n', '\r\000']
    mode = "User"
    def write(self, data):
        """Send the given data over my transport."""
        self.transport.write(data)
    def connectionMade(self):
        """I will write a welcomeMessage and loginPrompt to the client."""
        self.write(self.welcomeMessage() + self.loginPrompt())
    def welcomeMessage(self):
        """Override me to return a string which will be sent to the client
        before login."""
        x = self.factory.__class__
        return ("\r\n" + x.__module__ + '.' + x.__name__ +
                '\r\nTwisted %s\r\n' % copyright.version
                )
    def loginPrompt(self):
        """Override me to return a 'login:'-type prompt."""
        return "username: "
    def iacSBchunk(self, chunk):
        pass
    def iac_DO(self, feature):
        pass
    def iac_DONT(self, feature):
        pass
    def iac_WILL(self, feature):
        pass
    def iac_WONT(self, feature):
        pass
    def iac_IP(self, feature):
        pass
    def processLine(self, line):
        """I call a method that looks like 'telnet_*' where '*' is filled
        in by the current mode. telnet_* methods should return a string which
        will become the new mode.  If None is returned, the mode will not change.
        """
        mode = getattr(self, "telnet_"+self.mode)(line)
        if mode is not None:
            self.mode = mode
    def telnet_User(self, user):
        """I take a username, set it to the 'self.username' attribute,
        print out a password prompt, and switch to 'Password' mode. If
        you want to do something else when the username is received (ie,
        create a new user if the user doesn't exist), override me."""
        self.username = user
        self.write(IAC+WILL+ECHO+"password: ")
        return "Password"
    def telnet_Password(self, paswd):
        """I accept a password as an argument, and check it with the
        checkUserAndPass method. If the login is successful, I call
        loggedIn()."""
        self.write(IAC+WONT+ECHO+"*****\r\n")
        try:
            checked = self.checkUserAndPass(self.username, paswd)
        except:
            return "Done"
        if not checked:
            return "Done"
        self.loggedIn()
        return "Command"
    def telnet_Command(self, cmd):
        """The default 'command processing' mode. You probably want to
        override me."""
        return "Command"
    def processChunk(self, chunk):
        """I take a chunk of data and delegate out to telnet_* methods
        by way of processLine. If the current mode is 'Done', I'll close
        the connection. """
        self.buffer = self.buffer + chunk
        for delim in self.delimiters:
            idx = self.buffer.find(delim)
            if idx != -1:
                break
        while idx != -1:
            buf, self.buffer = self.buffer[:idx], self.buffer[idx+2:]
            self.processLine(buf)
            if self.mode == 'Done':
                self.transport.loseConnection()
            for delim in self.delimiters:
                idx = self.buffer.find(delim)
                if idx != -1:
                    break
    def dataReceived(self, data):
        chunk = StringIO()
        for char in data:
            if self.gotIAC:
                if self.iacByte:
                    if self.iacByte == SB:
                        if char == SE:
                            self.iacSBchunk(chunk.getvalue())
                            chunk = StringIO()
                            del self.iacByte
                            del self.gotIAC
                        else:
                            chunk.write(char)
                    else:
                        try:
                            getattr(self, 'iac_%s' % iacBytes[self.iacByte])(char)
                        except KeyError:
                            pass
                        del self.iacByte
                        del self.gotIAC
                else:
                    self.iacByte = char
            elif char == IAC:
                c = chunk.getvalue()
                if c:
                    why = self.processChunk(c)
                    if why:
                        return why
                    chunk = StringIO()
                self.gotIAC = 1
            else:
                chunk.write(char)
        c = chunk.getvalue()
        if c:
            why = self.processChunk(c)
            if why:
                return why
    def loggedIn(self):
        """Called after the user succesfully logged in.
        Override in subclasses.
        """
        pass
