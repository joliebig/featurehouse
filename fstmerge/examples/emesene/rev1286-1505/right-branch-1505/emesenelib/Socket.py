'''This module provides "interfaces to connect to external hosts",
including only MSNP sockets by now. (future: http[s], DC)
'''
import sys
import time
import Queue
import socket
import select
import gobject
import httplib
import threading
import traceback
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO
import common
PAYLOAD_COMMANDS = ['UBX', 'GCF', 'MSG', 'NOT']
IGNORE_COMMANDS = ['SBS']
class BaseConnection(gobject.GObject):
    '''This is the base class to all sockets (including http method ones),
    and higher-level connections (http or https)
    The signals below behave similar to io_add_watch in a normal socket, but
    exposing a simple, unified interface for all different socket types'''
    __gsignals__ = {
        'input': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'hangup': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'output': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
    }
    def __init__(self):
        gobject.GObject.__init__(self)
        self._hung = False
        self._signals = {'input': [], 'hangup': [], 'output': []}
    def _update_events(self):
        pass
    def connect(self, name, *args):
        '''Connects a signal and stores the identifier'''
        retval = gobject.GObject.connect(self, name, *args)
        self._signals[name].append(retval)
        self._update_events()
        return retval
    def disconnect(self, identifier, update=True):
        if identifier:
            gobject.GObject.disconnect(self, identifier)
        for signal in self._signals:
            if identifier in self._signals[signal]:
                self._signals[signal].remove(identifier)
        if update:
            self._update_events()
    def disconnectAll(self):
        '''Disconnects all the signal ids stored'''
        for identifiers in self._signals.values():
            for identifier in identifiers:
                self.disconnect(identifier, update=False)
        self._update_events()
    def hangup(self):
        if not self._hung:
            self._hung = True
            self.emit('hangup')
            self.close()
            self.disconnectAll()
class BaseSocket(BaseConnection):
    '''This is the base class to Socket and HTTPSocket
    Some high-level methods are implemented here too'''
    ping_enabled = None
    def __init__(self, host, port):
        BaseConnection.__init__(self)
        self._buffer = ''
        self._host = host
        self._port = port
        self.tid = 1
        self.lastsent = int(time.time())
        self.lastpng = 0
        self.lastqng = 0
        self.max_ping_delay = 10
        self.secs_between_pings = 20
        self.on_login = True
    def sendCommand(self, command, params=''):
        '''send a command through the socket and increment the tid'''
        if params != "":
            text = "%s %s %s\r\n" % (command, self.tid, params)
        else:
            text = "%s %s\r\n" % (command, self.tid)
        self.tid += 1
        self.send(text)
        return self.tid
    def sendPayloadCommand(self, command, params, payload):
        '''send a command that has an indicator of the body length'''
        if type(payload) == unicode:
            payload = str(payload.encode('utf-8'))
        text = command + " " + str(self.tid) + " "
        if params != '':
            text += params + " "
        text += str(len(payload)) + '\r\n' + payload
        self.tid += 1
        self.send(text)
        return self.tid
    def send(self, text):
        pass
    def receiveCommand(self):
        return ('', 0, '')
    def receivePayload(self):
        return ''
    def close(self):
        pass
    def ping(self):
        pass
    def onQng(self, tid):
        pass
    def select(self):
        return ([], [], [True])   # error!
    def establish(self, timeout=None):
        pass
class Socket(BaseSocket):
    '''This represents a MSNP protocol socket over plain TCP'''
    ping_enabled = True
    def __init__(self, host, port):
        BaseSocket.__init__(self, host, port)
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setblocking(0)
        self._event_id = 0
        self._watch_mask = 0
        self._update_events()
    def _update_events(self):
        '''Calls io_add_watch again if the mask changed
        (by connecting/disconnecting signals)'''
        mask = 0
        if len(self._signals['input']):
            mask |= gobject.IO_IN
        if len(self._signals['output']):
            mask |= gobject.IO_OUT
        if len(self._signals['hangup']):
            mask |= gobject.IO_HUP | gobject.IO_ERR
        if mask == self._watch_mask:
            return
        self._watch_mask = mask
        if self._event_id != 0:
            gobject.source_remove(self._event_id)
        self._event_id = 0
        if mask:
            self._event_id = gobject.io_add_watch(self.socket,
                mask, self._event)
    def _event(self, fd, condition):
        if self._isOnError() or (condition & (gobject.IO_ERR | gobject.IO_HUP)):
            self.hangup()
        elif (condition & gobject.IO_IN):
            self.emit('input')
        elif (condition & gobject.IO_OUT):
            self.emit('output')
        return True
    def send(self, text):
        '''Sends raw bytes'''
        common.debug(">>> " + text, 'socket')
        self.lastcmd = int(time.time())
        try:
            if not len(select.select([], [self.socket], [], 60)[1]):
                raise socket.error
            retval = self.socket.send(text)
        except Exception, e:
            common.debug("Error on send" + str(e), 'socket')
            self.hangup()
            return ''
        return retval
    def _recv(self, bytes):
        '''Receives raw bytes'''
        if not self._canReceive():
            return ''
        try:
            if not len(select.select([self.socket], [], [], 60)[0]):
                raise socket.error
            return self.socket.recv(bytes)
        except Exception, e:
            common.debug("Error on recv" + str(e), 'socket')
            self.hangup()
            return None
    def _receiveLine(self):
        '''Receives data from the socket and return it'''
        buffer = StringIO()
        chunk = self._recv(1)
        if chunk is None: return ''
        while chunk != '\n' and chunk != '':
            buffer.write(chunk)
            chunk = self._recv(1)
            if chunk is None: return ''
        message = buffer.getvalue().rstrip('\r')
        common.debug("<<< " + message, 'socket')
        self.lastcmd = int(time.time())
        return message
    def receiveCommand(self):
        '''Parses the receive() return value
        in a (command, tid, params) tuple'''
        list = self._receiveLine().split(' ', 2)
        command, tid, params = ('', '0', '')
        if len(list) >= 1: command = list[0]
        if len(list) >= 2: tid = list[1]
        if len(list) >= 3: params = list[2]
        return (command, tid, params)
    def receivePayload(self, length):
        '''receive a number of characters from an earlier command'''
        buffer = []
        received = 0
        while received < length:
            chunk = self._recv(length - received)
            if chunk is None: return ''
            received += len(chunk)  # not concatenation
            buffer.append(chunk)
        return ''.join(buffer)
    def ping(self):
        '''ping the server if idle, errors are handled by core.py'''
        if not self.on_login and \
           (int(time.time()) - self.lastcmd) > self.secs_between_pings:
            self.send('PNG\r\n')
            self.lastpng = int(time.time())
    def onQng(self, tid):
        '''called by core.process()'''
        self.lastqng = int(time.time())
        self.max_ping_delay = int(int(tid) * 85 / 100)
        self.secs_between_pings = int(int(tid) * 9 / 10)
    def establish(self, timeout=None):
        '''establish is another name for connect :P
        connect to the host an port given in the contructor
        throw an exception if cannot connect'''
        self.socket.settimeout(timeout)
        try:
            self.socket.connect((self._host, self._port))
        except:
            self.socket.settimeout(None)
            raise
        self.socket.settimeout(None)
    def close(self):
        self.socket.close()
        self.disconnectAll()
    def _canSend(self):
        '''Calls select() to determine if the socket is ready to write'''
        fds = select.select([self.socket], [], [], 0)
        return len(fds[0]) == 1
    def _canReceive(self):
        '''Calls select() to determine if the socket is ready to read''' 
        fds = select.select([], [self.socket], [], 0)
        return len(fds[1]) == 1
    def _isOnError(self):
        '''Determines if the socket is on error'''
        if self._hung:
            return True
        try:
            fds = select.select([], [], [self.socket], 0)
            return len(fds[2]) == 1
        except:
            return True
    def select(self):
        '''return a select.select like tuple'''
        return select.select([self.socket], [self.socket], [], 0)
class Proxy:
    '''Just a class to hold the proxy data'''
    def __init__(self, host='', port='80', \
        user='', password=''):
        '''constructor of the class, just set the class attributes'''
        self.host = host
        self.port = str(port)
        self.user = user
        self.password = password
class HTTPSocket(BaseSocket):
    '''This class represent an abstraction of HTTPMethod that has the
    same methods that the Socket class to make easy the use of this
    on core class, it behaves the same way'''
    ping_enabled = False
    def __init__(self, host, port, proxy=None, serverType='NS'):
        BaseSocket.__init__(self, host, port)
        self.inQueue = Queue.Queue(0)
        self.outQueue = Queue.Queue(0)
        self.responseBuffer = ''
        self.httpRequestManager = HttpRequestManager(self,
            host, port, proxy, serverType)
        self.httpRequestManager.start()
        self.hasReceived = False
    def send(self, text):
        '''send text with no format, this is usefull for commands like OUT\r\n
        also its used internally'''
        common.debug(">>> " + text, 'socket')
        self.inQueue.put(text)
    def _getBuffer(self, wait=False):
        if self.responseBuffer == '':
            try:
                newbuffer = self.outQueue.get(True, 0.01)
            except Queue.Empty:
                return ''
            self.responseBuffer = newbuffer
    def receiveCommand(self):
        '''Parses the receive() return value
        in a (command, tid, params) tuple'''
        self._getBuffer()
        if self.responseBuffer.find('\r\n') != -1:
            received, self.responseBuffer = \
                self.responseBuffer.split('\r\n', 1)
        else:
            received = self.responseBuffer
            self.responseBuffer = ''
        common.debug("<<< " + received, 'socket')
        self.lastcmd = int(time.time())
        list = received.split(' ', 2)
        command, tid, params = ('', '0', '')
        if len(list) >= 1: command = list[0]
        if len(list) >= 2: tid = list[1]
        if len(list) >= 3: params = list[2]
        return (command, tid, params)
    def receivePayload(self, length):
        '''receive a number of characters from an earlier command'''
        length = int(length) 
        received = 0
        buffer = []
        while received < length:
            self._getBuffer(True)
            bytes = length - received
            chunk, self.responseBuffer = self.responseBuffer[:bytes], \
                                         self.responseBuffer[bytes:]
            received += len(chunk) + 4  # not concatenation
            buffer.append(chunk)
        return ''.join(buffer)
    def close(self, *args):
        '''called on logout'''
        self.httpRequestManager.quit()
        self.disconnectAll()
    def select(self):
        if self.httpRequestManager.open:
            return ([True], [True], [])
        else:
            common.debug("select: session is closed", 'socket')
            return ([], [], [])
    def _input_handler(self):
        '''Called by a normal priority idle_add'''
        self.emit('input')
        return False
    def _hangup_handler(self):
        '''Same as above but for hangup'''
        self.emit('hangup')
        return False
class HttpRequestManager(threading.Thread):
    '''This class make the request via Http method in a thread'''
    def __init__(self, parent, destip, port, proxy, desttype):
        '''Contructor'''
        threading.Thread.__init__(self)
        self.parent = parent
        self.inQueue = parent.inQueue
        self.outQueue = parent.outQueue
        self.host = 'gateway.messenger.hotmail.com'
        self.port = port
        self.proxy = proxy
        self.type = desttype
        self.path = '/gateway/gateway.dll?Action=open&Server=' + desttype + \
            '&IP=' + destip
        self.open = True
        self.connection = None
        self.data = ''
        self.sessionID = ''
        self.dataWaiting = False # indicate if some data is waiting
        if self.proxy.host:
            addr = self.proxy.host + ':' + self.proxy.port
        elif desttype == 'NS':
            addr = 'gateway.messenger.hotmail.com:80'
        else:
            addr = destip + ':80'
        self.connection = httplib.HTTPConnection(addr)
    def quit(self):
        '''force the thread to finish'''
        while self.inGetNoWait() != None:
            pass
        self.inQueue.put("quit")
        self.open = False
        gobject.idle_add(self.parent._hangup_handler,
            priority=gobject.PRIORITY_DEFAULT)
        common.debug(self.getName() + ' closed', 'psocket')
    def run(self):
        '''the thread main loop'''
        nextreq = ''
        while self.open:
            if nextreq:
                req = nextreq
                nextreq = ''
            else:
                req = self.inwait(3, 'poll')
            common.debug(self.getName() + ' doing ' + \
                str(req)[:40].strip() + '...', 'psocket')
            if req == 'poll':
                if not self.outQueue.empty():
                    gobject.idle_add(self.parent._input_handler,
                        priority=gobject.PRIORITY_DEFAULT)
                while True:
                    req = self.inGetNoWait()
                    if req == None:
                        break
                    elif req != 'poll':
                        nextreq = req
                        break
                data = self.poll()
            elif req == 'quit':
                return
            else:
                data = self.request(payload=req)
            while len(data) > 0:
                tmp = data.split('\r\n', 1)
                if len(tmp) == 1:
                    tmp = [tmp[0], '']
                line, data = tmp
                linesplit = line.split()
                if linesplit[0] not in IGNORE_COMMANDS:
                    self.outQueue.put(line)
                if linesplit[0] in PAYLOAD_COMMANDS:
                    size = int(linesplit[-1])
                    payload, data = data[:size], data[size:]
                    self.outQueue.put(payload)
                gobject.idle_add(self.parent._input_handler,
                    priority=gobject.PRIORITY_DEFAULT)
        return 
    def poll(self):
        return self.request(path='/gateway/gateway.dll?'
            'Action=poll&SessionID=' + self.sessionID)
    def request(self, method='POST', path=None, payload=''):
        if path is None:
            path = self.path
        url = 'http://' + self.host + path
        def dorequest():
            self.connection.putrequest(method, url,
                                       skip_accept_encoding=True,
                                       skip_host=True)
            self.connection.putheader("Accept", "*/*")
            self.connection.putheader("Accept-Language", "en-us")
            self.connection.putheader("User-Agent", "MSMSGS")
            self.connection.putheader("Host", self.host)
            self.connection.putheader("Proxy-Connection", "Keep-Alive")
            self.connection.putheader("Connection", "Keep-Alive")
            self.connection.putheader("Pragma", "no-cache")
            self.connection.putheader("Content-Type",
                "application/x-msn-messenger")
            self.connection.putheader("Content-Length", str(len(payload)))
            self.connection.endheaders()
            self.connection.send(payload)
            return self.getHttpResponse()
        for i in range(5):
            try:
                return dorequest()
            except Exception, e:
                traceback.print_exception(*sys.exc_info())
            self.connection.close()
            time.sleep(2)
            self.connection.connect()
        self.quit()
        return 'OUT'
    def getHttpResponse(self):
        response = self.connection.getresponse()
        text = response.read().strip()
        if response.status == 500:
            common.debug("500 internal server error", "psocket")
            self.quit()
            return 'OUT'
        elif response.status != 200:
            raise httplib.HTTPException("Server not available")
        try:
            data = response.getheader('x-msn-messenger', '')
            if data.count("Session=close"):
                common.debug("Session closed", "socket")
                self.quit()
                return 'OUT'
            self.sessionID = data.split("; GW-IP=")[0].replace("SessionID=", "")
            self.gatewayIP = data.split("; ")[1].replace("GW-IP=", "")
            self.host = self.gatewayIP
            self.path = "/gateway/gateway.dll?SessionID=" + self.sessionID
        except Exception,e:
            common.debug('In getHttpResponse: ' + str(e), 'socket')
            common.debug('Data: "%s"' % data, 'socket')
        return text
    def inGetNoWait(self):
        try:
            return self.inQueue.get(True, 0.01)
        except Queue.Empty:
            return None
    def inwait(self, secs, default):
        '''waits n seconds or returns if there's something in the in queue'''
        try:
            return self.inQueue.get(True, secs)
        except Queue.Empty:
            return default
