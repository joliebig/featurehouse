import os
import sys
import time
import socket
import urllib
import gobject
import httplib
import urlparse
import traceback
import Socket
import MsnOIM
import Msnobj
import msn_tlp
import ContactData
import SoapManager
import Switchboard
import XmlTemplates
import SignalHandler
import ProfileManager
import common
class Msnp(ProfileManager.ProfileManager):
    '''This class give support to the MSNP13 protocol to use the MSN Messenger network'''
    __gsignals__ = {
        'user-list-change' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'error' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, 
            (gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'exception' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, 
            (gobject.TYPE_PYOBJECT,)),
        'message-received' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, (gobject.TYPE_STRING,)),
        'connection-problem' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'disconnected' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'connection-closed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'challenge' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'initial-status-change' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'status-change' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'status-online' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'status-offline' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'switchboard-invitation' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'add-notification' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING, gobject.TYPE_STRING, gobject.TYPE_STRING, 
             gobject.TYPE_STRING, gobject.TYPE_STRING)),
        'remove-notification' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING, gobject.TYPE_STRING, gobject.TYPE_STRING, \
             gobject.TYPE_STRING)),
        'personal-message-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'server-message' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'user-disconnected' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'logout' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'account-unconfirmed': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'self-nick-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'self-status-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,)),
        'self-personal-message-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'self-current-media-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_PYOBJECT)),
        'new-conversation' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_PYOBJECT)),
        'new-switchboard' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,)),
        'nudge-received' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,)),
        'initial-mail-notification' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            ()),
        'new-mail-notification' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'mail-movement-notification' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            ()),
        'nick-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'contact-status-change' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'msnobj-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,gobject.TYPE_BOOLEAN)),
        'user-online' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'user-offline' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,)),
        'send-message-error' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
        'offline-message-waiting' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,)),
        'offline-message-received' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,)),
        'display-picture-changed' : (gobject.SIGNAL_RUN_LAST, 
            gobject.TYPE_NONE, (gobject.TYPE_PYOBJECT, 
                    gobject.TYPE_PYOBJECT, gobject.TYPE_PYOBJECT,)),
        'custom-emoticon-transfered' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_PYOBJECT,gobject.TYPE_STRING)),
        'hidden-contact' : (gobject.SIGNAL_RUN_LAST, 
            gobject.TYPE_NONE, 
            (gobject.TYPE_PYOBJECT, gobject.TYPE_PYOBJECT)),
        'login-error': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT, )),
        'login-successful': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            ()),
        'switchboard' : (gobject.SIGNAL_RUN_LAST | gobject.SIGNAL_DETAILED, \
          gobject.TYPE_NONE, (gobject.TYPE_PYOBJECT, ) * 3),
    }
    CVR_STRING = "0x0c0a winnt 5.1 i386 MSNMSGR 8.0.0792 msmsgs %s"
    CLIENT_ID = 0x50000000 | 0x2  # msnc5 + reset capabilities
    CLIENT_ID |= 0x4     # ink
    CLIENT_ID |= 0x8000  # winks
    CLIENT_ID |= 0x40000 # voice clips
    def __init__(self, host, port, user, password, \
        userConfigDir, proxy = None, safemode = False):
        '''Constructor, intialize variables.'''
        gobject.GObject.__init__(self)
        if proxy == None:
            self.socket = Socket.Socket(host, port)
        else:
            gobject.threads_init()
            self.socket = Socket.HTTPSocket(host, port, proxy)
        self.user = user.lower()
        self.password = password
        self.MSPAuth = ''
        self.cacheDir = userConfigDir + os.sep + 'cache'
        self.userConfigDir = userConfigDir
        self.proxy = proxy
        self.safemode = safemode
        self.callbacks = None
        self.connected = False
        self.nick = ''
        self.status = 'FLN'
        self.lastCommand = ''
        self.switchboards = [] # the switchboards to process
        self.switchboardsByTid = {}
        self.demographics = {}
        self.signals = [] # gobject signals to disconnect on logout
        self.selfSignals = [] # msnp signals
        self.contactManager = ContactData.ContactList({})
        self.msnObjectsManager = Msnobj.MsnObjectsManager(user)
        self.msnOIM = MsnOIM.MsnOIM(self)
        self.p2p = common.DynamicDict(msn_tlp.P2PUser, True)
        self.p2p.msn = self
        self.currentMedia = ''
        self.personalMessage = ''
        self.msnobj = None # the msnobj for the display picture
        self.picture = None
        self.inboxMessages = 0
        self.inboxUnreadMessages = 0
        self.otherMessages = 0
        self.otherMessagesUnreaded = 0
        self.initialBLPTid = 0
        self.accountConfirmed = False
        self._expecting = ''
        self._allowedRedirects = 5
        self._onLogin = False
    def login(self):
        '''login to the server, after succesfully login you have to change your
            status to appear online'''
        self._allowedRedirects = 5
        self._onLogin = True
        self._redirect()
    def _redirect(self, host='', port=0):
        '''NS server redirect'''
        if host and port:
            try:
                self.socket.disconnectAll()
                self.socket.close()
            except:
                pass
            if self.proxy == None:
                self.socket = Socket.Socket(host, port)
            else:
                self.socket = Socket.PSocket(host, port, self.proxy)
        try:
            self.socket.establish()
            self.socket.sendCommand('VER', 'MSNP13 CVR0')
        except:
            self.emit('login-error', 'Connection problem')
            return
        self._expecting = 'VER'
        self.socket.connect('input', self._loginInputHandler)
    def _loginInputHandler(self, socket):
        retval = False
        if self._onLogin:
            try:
                retval = self._loginProcess(socket)
            except Socket.socket.error, desc:
                self.emit('login-error', 'Connection problem: ' + str(desc))
            except Exception, desc:
                exception = sys.exc_info()
                traceback.print_exception(*exception)
                self.emit('login-error', 'Login error: ' + str(desc))
        if retval == False:
            self.socket.disconnectAll()
    def _loginProcess(self, socket):
        '''Called when a message is received on the login phase'''
        (command, tid, params) = self.socket.receiveCommand()
        if command == 'XFR' and params[ :2 ] == 'NS':
            try:
                ns = params.split()[1].split(":") # 207.46.24.39:1863
                host = ns[0]                      # 207.46.24.39
                port = int(ns[1])                 # int(1863)
            except:
                self.emit('login-error', 'Incorrect parameters: ' + params)
                return
            if (self._allowedRedirects - 1) <= 0:
                self.emit('login-error', 'Too many redirects')
                return
            self._allowedRedirects -= 1
            self._redirect(host, port)
            return
        elif command == 'MSG':
            data = self.socket.receivePayload(int(params.split()[-1]))
            if self.parse_demographics(data):
                self.emit('login-successful')
                return
        elif command == 'GCF':
            self.socket.receivePayload(int(params))
        elif command == 'SBS':
            pass
        elif self._expecting and self._expecting != command:
            try:
                self.emit('login-error', 'Server error ' + str(command))
            except ValueError:
                self.emit('login-error', 'Unexpected message (%s)' % command)
            return False
        else:
            if command == 'VER':
                if not params.startswith('MSNP13'):
                    self.emit('login-error', 'Protocol not supported by server')
                    return False
                self.socket.sendCommand("CVR", Msnp.CVR_STRING % self.user)
                self._expecting = 'CVR'
            elif command == 'CVR':
                self.socket.sendCommand("USR", "TWN I " + self.user)
                self._expecting = 'USR'
            elif command == 'USR' and params.startswith("TWN S "):
                hash = params.split("TWN S ")[1]
                self.hash = hash
                self.passportid = self.passportAuth(hash)
                try:
                    self.t = self.passportid.split('&p=')[ 0 ]
                    self.t = self.t[2:]
                    self.MSPProf = self.passportid.split('&p=')[ 1 ]
                except:
                    self.emit('login-error', 'Incorrect passportid ' + \
                        self.passportid)
                    return False
                self.socket.sendCommand("USR" , "TWN S " + self.passportid)
                self._expecting = 'USR' # but not this one
            elif command == 'USR' and params.startswith("OK"):
                common.debug('We are in', 'core')
            elif command == 'USR':
                self.emit('login-error', 'Authentication error')
                return False
        return True
    def do_login_error(self, message):
        common.debug("login error ;_;", 'core')
        common.debug(message, 'core')
        try:
            self.socket.hangup()
        except:
            pass
        self._onLogin = False
    def do_login_successful(self):
        '''logged in :D'''
        common.debug("logged in", 'core')
        self.connected = True
        self.soapManager = SoapManager.SoapManager(self)
        self.soapManager.start()
        self.socket.disconnectAll()
        self.socket.connect('input', self.process)
        self.socket.connect('hangup', self.socketHangup)
        self.signals.append(gobject.timeout_add(500, self.soapManager.process))
        self.signals.append(gobject.timeout_add(5000, self.checkPing))
        mlCache = self.getCacheFileData(self.user + "_ml.xml")
        diCache = self.getCacheFileData(self.user + "_di.xml")
        if mlCache and diCache and not self.safemode:
            common.debug("parsing membership list", 'core')
            start = time.time()
            try:
                self.setMembershipListXml(mlCache)
                common.debug("done:" + str(time.time() - start), 'core')
                common.debug("parsing dynamic items", 'core')
                start = time.time()
                self.setDynamicItemsXml(diCache)
                self.getNickFromDynamicItems(diCache)
                common.debug("done:" + str(time.time() - start), 'core')
            except Exception, e: #TODO: pylint
                print "error parsing lists", e
                self.getMembershipListSync()
                self.getDynamicItemsSync()
            self.emit('user-list-change')
        else:
            self.getMembershipListSync()
            self.getDynamicItemsSync()
        self.initialBLPTid = self.socket.sendCommand("BLP" , "BL")
        if not self.safemode:
            for i in self.contactManager.getADL():
                self.socket.sendPayloadCommand("ADL", '', i)
        else:
            def safeADL(chunk, next):
                self.socket.sendPayloadCommand("ADL", '', chunk)
                if next:
                    gobject.timeout_add(1000, gobject.idle_add, safeADL, \
                        next.pop(0), next)
            ADLChunks = self.contactManager.getADL()
            if ADLChunks:
                safeADL(ADLChunks.pop(0), ADLChunks)
        self.changeNick(self.nick, initial=True)
        self.callbacks = SignalHandler.SignalHandler(self)
        self.selfSignals = []
        self.selfSignals.append(self.connect('status-change', \
            self.callbacks.statusChange))
        self.selfSignals.append(self.connect('switchboard-invitation', \
            self.callbacks.switchboardInvitation))
        self.selfSignals.append(self.connect('server-message', \
            self.callbacks.serverMessage))
        if mlCache and diCache and not self.safemode:
            self.getMembershipListAsync()
            self.getDynamicItemsAsync()
    def passportReAuth(self, hash=None):
        if hash == None:
            hash = self.hash
        self.passportid = self.passportAuth(hash)
        self.MSPAuth = self.passportid.split('&p')[0][2:]
        self.MSPProf = self.passportid.split('&p')[1]
    def passportAuth(self , hash):
        '''do the passport authenticaton, this is done connecting to loginnet.passport.com:443
        and sending a XML message described on XmlTemplates.passport'''
        common.debug('PASSPORT begin', 'core')
        ticket = urllib.unquote(hash).replace("," , "&amp;")
        body = XmlTemplates.passport%(self.user, common.escape(self.password), ticket)
        headers = { \
        "Accept" :  "text/*" , \
        "User-Agent" : "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)" , \
        "Host" : "loginnet.passport.com" , \
        "Content-Length" : str(len(body)) , \
        "Connection" : "Keep-Alive" , \
        "Cache-Control" : "no-cache" , \
        }
        _server="loginnet.passport.com"
        _url="/RST.srf"
        succeeded = False
        for i in range(5):
            response = None
            lastError = None
            for i in range(3):
                try:
                    conn = httplib.HTTPSConnection(_server,443)
                    conn.request("POST", _url, body, headers)
                    response = conn.getresponse()
                    common.debug('%s %s'%(response.status, response.reason), \
                        'core')
                    break
                except Exception, e:
                    lastError = e
            if response:
                data = response.read()
            else:
                raise common.AuthError, "Can't connect to HTTPS server: " + \
                    str(e)
            if data.find('<faultcode>psf:Redirect</faultcode>') > 0:
                _url = urlparse.urlparse(data.split('<psf:redirectUrl>')\
                    [ 1 ].split('</psf:redirectUrl>')[ 0 ])
                _server=_url[ 1 ]
                _url=_url[ 2 ]
                common.debug('Redirect to: %s %s' % (_server, _url), 'core')
            else:
                succeeded = True
                break
        if not succeeded:
            raise common.AuthError, 'Too many redirections'
        try:
            twnticket = data.split('<wsse:BinarySecurityToken Id="PPToken1">')\
                [ 1 ].split("</wsse:BinarySecurityToken>")[ 0 ]
            twnticket = twnticket.replace("&amp;" , "&")
        except Exception, e:
            common.debug(e, 'core')
            common.debug(data, 'core')
            try:
                faultstring = data.split("<faultstring>")\
                    [ 1 ].split("</faultstring>")[ 0 ]
            except:
                faultstring = ''
            raise common.AuthError, faultstring
        return twnticket
    def ping(self):
        '''send a ping to the server and register the timestamp of the command'''
        try:
            self.socket.ping()
        except (IOError, socket.error):
            self.emit('connection-closed')
    def checkConnection(self):
        '''return true if the time between the last ping and the response is
        lower than max_ping_delay'''
        if self.socket.ping_enabled and \
           (self.socket.lastqng < self.socket.lastpng) and \
           (time.time() - self.socket.lastpng) > self.socket.max_ping_delay:
            return False
        return True
    def checkPing(self):
        '''check connection and ping if needed
        A return value False doesn't mean broken connection,
        it means "disconnect this timeout"'''
        if not self.connected:
            return False
        elif not self.socket.ping_enabled:
            return False
        elif self.socket.lastpng == 0:
            self.ping()
        elif not self.checkConnection():
            self.emit('connection-problem')
            print 'connection problem'
            return False
        elif (int(time.time()) - self.socket.lastpng) > \
             self.socket.secs_between_pings:
            self.ping()
        return True
    def socketHangup(self, socket):
        '''The socket got into IO_HUP or IO_ERR status'''
        common.debug("Socket hangup/error", 'core')
        self.emit('connection-closed')
        return False
    def process(self, obj=None):
        '''read a command from the server and process it'''
        (command, tid, params) = self.socket.receiveCommand()
        try:
            self.processCommand(command, tid, params)
        except:
            print 'FATAL ERROR on msn.process(): '
            self.emit('exception', sys.exc_info())
            return False
        return True
    def processCommand(self, command, tid, params):
        if command == 'CHL':
            self.callbacks.challenge(self, command, tid, params)
            self.socket.on_login = False
        elif command == 'ILN':
            self.callbacks.statusChange(self, command, tid, params)
            gobject.idle_add(self.emit, 'initial-status-change', command, tid, params)
        elif command == 'NLN':
            self.callbacks.statusOnline(self, command, tid, params)
            self.emit('status-online', command, tid, params)
        elif command == 'FLN':
            self.callbacks.statusOffline(self, command, tid, params)
            self.emit('status-offline', command, tid, params)
        elif command == 'RNG':
            self.emit('switchboard-invitation', command, tid, params)
        elif command == 'ADL':
            if params != 'OK':
                payload = self.socket.receivePayload(int(params))
                email = payload.split('<c n="')[1].split('"')[0] + '@' + \
                    payload.split('<ml><d n="')[1].split('"')[0]
                nick = ''
                try:
                    nick = payload.split(' f="')[1].split('"')[0]
                    nick = urllib.unquote(nick)
                except:
                    pass
                lists = self.contactManager.lists
                if email not in lists['Block'] and email not in lists['Allow']:
                    self.emit('add-notification', command, tid, params, \
                        email, nick)
        elif command == 'RML':
            if params != 'OK':
                payload = self.socket.receivePayload(int(params))
                email = payload.split('<c n="')[1].split('"')[0] + '@' + \
                    payload.split('<ml><d n="')[1].split('"')[0]
                self.emit('remove-notification', command, tid, params, email)
        elif command == 'UBX':
            payload = self.socket.receivePayload(int(params))
            payload = payload.decode('utf-8', 'replace').encode('utf-8')
            common.debug('<<< ' + payload, 'core')
            if int(params) > 0:
                try:
                    self.parseUBX(command, tid, params, payload)
                except Exception, e:
                    common.debug('Unable to handle UBX: ' + str(e))
        elif command == 'XFR':
            connstring = ' '.join([command, tid, params])
            self.switchboardsByTid[int(tid)].setConnectionString(connstring)
        elif command == 'MSG':
            try:
                payload = self.socket.receivePayload(int(params.split()[1]))
                self.emit('server-message', command, tid, params, payload)
            except Exception, e:
                common.debug('Exception in msnp.process, continuing', 'core')
                common.debug('(EE) ' + str(e), 'core')
        elif command == 'NOT':
            payload = self.socket.receivePayload(int(tid))
        elif command == 'OUT':
            self.emit('user-disconnected', tid, params)
        elif command == 'QNG':
            self.socket.onQng(tid)
        elif command == '209' and self.initialBLPTid == tid:
            self.accountConfirmed = False
            self.emit('account-unconfirmed')
        self.lastCommand = command
    def parseUBX(self, command, tid, params, payload):
        '''this function parses the UBX payload, and sets the personal
        message or current media'''
        pm = payload.split('<PSM>')[ 1 ].split('</PSM>')[ 0 ]
        pm = common.unescape(pm)
        self.contactManager.setContactPersonalMessage(tid, pm)
        if payload.find('<CurrentMedia>') != -1:
            media = payload.split('<CurrentMedia>')[1]\
                .split('</CurrentMedia>')[0]
            mhead = media.find('\\0Music\\01\\0')
            if mhead != -1:
                media = '\xe2\x99\xab ' + media[mhead+12:]
                margs = media.split('\\0')
                media = margs[0]
                for args in range(1, len(margs)):
                    media = media.replace('{%s}' % (args-1), margs[args])
                media = common.unescape(media)
                self.contactManager.setContactPersonalMessage(tid, media)
        if self.lastCommand != 'ILN':
            self.emit('personal-message-changed', tid, pm)
            contact = self.contactManager.getContact(tid)
            self.emit('user-attr-changed', contact)
    def changeStatus(self , status):
        '''change the current status'''
        if status == 'FLN' or status == 'offline':
            return
        msnobj = ''
        if self.msnobj:
            msnobj = ' ' + self.msnobj.quote()
        else:
            msnobj = ' 0'
        clientid = ' ' + str(self.CLIENT_ID)
        if common.status_table.has_key(status):
            self.socket.sendCommand("CHG" , common.status_table[ status ] + clientid + msnobj)
            self.status = common.status_table[ status ]
            self.emit('self-status-changed', self.status)
        elif common.reverse_status.has_key(status):
            self.socket.sendCommand("CHG" , status + clientid + msnobj)
            self.status = status
            self.emit('self-status-changed', self.status)
    def sendDL(self, command, email, type):
        '''send ADL or RML on a single contact'''
        self.sendDLs(command, {email: type})
    def sendDLs(self, command, usersdict):
        '''send ADL or RML
        userslist is a dict {email: type}
        allowed values for type:
        FL Forward List Users who were added to your contact list
        RL Reverse List Users who added you to their contact list
        AL Allow List Users who are able to see your status
        BL Block List Users who are blocked from seeing your status'''
        payloads = self.contactManager.buildDL(usersdict, initial=False)
        for payload in payloads:
            self.socket.sendPayloadCommand(command, '', payload)
    def logout(self):
        '''disconnect from the server'''
        self.emit('logout')
        self.socket.disconnectAll()
        try:
            self.socket.send('OUT\r\n')
        except:
            pass
        self.socket.hangup()
        if not self.connected:
            return
        self.connected = False
        for signal in self.signals:
            gobject.source_remove(signal)
        self.signals = []
        for signal in self.selfSignals:
            self.disconnect(signal)
        self.selfSignals = []
        self.contactManager = ContactData.ContactList({})
        self.msnOIM.destroy()
        self.msnOIM = None
        self.callbacks = None
        try:
            self.soapManager.destroy()
            self.soapManager.join() # thread
            self.soapManager = None
        except:
            pass
        for sb in self.switchboards:
            try:
                sb.leaveChat()
                sb.msn = None
            except:
                pass
        self.switchboards = []
        self.switchboardsByTid = {}
    def newSwitchboard(self):
        '''create a new Switchboard and return it'''
        tid = self.socket.tid
        new = Switchboard.Switchboard(tid, self, 'requested')
        self.on_new_switchboard(new)
        self.switchboardsByTid[tid] = new
        self.socket.sendCommand('XFR', 'SB')
        return new
    def removeClosedSwitchboards(self):
        closed = []
        for sb in self.switchboards:
            if sb.status in ('closed', 'error'):
                closed.append(sb)
        for sb in closed:
            self.switchboards.remove(sb)
        del closed
    def on_display_picture_received(self, switchboard, msnobj, data, email):
        '''called when a display picture has been received'''
        contact = self.contactManager.getContact(email)
        filename = contact.displayPicturePath
        try:
            open(os.path.join(self.cacheDir, filename), 'wb').write(data)
        except IOError:
            print "can't save display picture"
        else:
            self.emit('display-picture-changed', switchboard, msnobj, email)
    def on_custom_emoticon_received(self, switchboard, msnobj, data, email):
        '''called when a custom emoticon has been received'''
        open(msnobj.filename,'wb').write(data)
        self.emit('custom-emoticon-transfered', email, msnobj, msnobj.filename)
    def on_new_switchboard(self, switchboard):
        '''called when a new switchboad is created, its not a callback
        from a signal'''
        self.switchboards.append(switchboard)
        self.emit('new-switchboard', switchboard)
    def getSwitchboard(self, email):
        '''try to retrieve an existing switchboard that has email as the first
        user and its not a group chat. Clean up closed switchboards and create
        a new switchboard if no existing switchboard matches.'''
        self.removeClosedSwitchboards()
        switchboard = None
        for sb in self.switchboards:
            if not sb.isGroupChat() and sb.firstUser == email:
                switchboard = sb
        if not switchboard:
            switchboard = self.newSwitchboard()
            switchboard.invite(email)
        return switchboard
    def getGroups(self):
        '''return the groups in the userlist'''
        return self.contactManager.groups
    def getGroupNames(self):
        '''Returns a list with the group names in the user list'''
        groups = self.contactManager.getGroupNames()
        groups.sort( key= lambda x: x.lower() )
        return groups
    def checkPending(self):
        '''Return all the pending users'''
        l = []
        lists = self.contactManager.lists
        for email in lists[ 'Pending' ]:
            if email not in lists[ 'Block' ] and email not in lists[ 'Allow' ]:
                l.append(email)
        return l
    def cacheFileExist(self, fileName):
        '''return true if the file exist in the cache directory'''
        return os.path.isfile(self.cacheDir + os.sep + fileName)
    def newCacheFile(self, fileName, data):
        '''create the file and put data on it'''
        f = open(self.cacheDir + os.sep + fileName, 'w')
        f.write(data)
        f.close()
    def getCacheFileData(self, fileName):
        '''get the data in filename, return None otherwise'''
        if self.cacheFileExist(fileName):
            f = open(self.cacheDir + os.sep + fileName, 'r')
            data = f.read()
            f.close()
            return data
        else:
            return None
    def setDisplayPicture(self, filename):
        '''try to open the picture and set the msnobj'''
        if filename == '':
            self.msnobj = None
        else:
            self.msnobj = self.msnObjectsManager.create('', filename, \
                type=Msnobj.Msnobj.DISPLAY_PICTURE)
            myself = self.contactManager.getContact(self.user)
            if myself != None:
                myself.displayPicturePath = filename
        if self.status != 'FLN':
            self.changeStatus(self.status)
    def createCustomEmoticon(self, shortcut, filename):
        self.msnObjectsManager.create(shortcut, filename, type=Msnobj.Msnobj.CUSTOM_EMOTICON)
    def getMsnObjectsManager(self):
        return self.msnObjectsManager
    def getUser(self):
        return self.user
    def getNick(self):
        return self.nick
    def setDebug(self, debug, binary):
        common.debugFlag = debug
        common.binaryFlag = binary
    def parse_demographics(self, payload):
        '''parse the demographic data and add it ot the demographics dict'''
        mspauth = False
        for line in payload.split("\r\n"):
            try:
                key, value = line.split(":")
                self.demographics.update({key.strip(): value.strip()})
            except:
                pass
            if line.startswith("MSPAuth: "):
                self.MSPAuth = line.split("MSPAuth: ")[ 1 ]
                mspauth = True
        return mspauth
gobject.type_register(Msnp)
