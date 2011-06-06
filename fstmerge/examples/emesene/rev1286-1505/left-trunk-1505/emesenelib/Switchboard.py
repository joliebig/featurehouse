import os
import time
import uuid
import base64
import urllib
import random
import gobject
import Msnobj
import Socket # emesenelib
import p2p.transfers
import common
class Switchboard(gobject.GObject):
    '''This class represents a switchboard connection and provides methods
    and signals to interact with it'''
    __gsignals__ = {
        'typing' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,)),
        'message-sent' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT, gobject.TYPE_PYOBJECT, 
                gobject.TYPE_PYOBJECT)),
        'nudge-sent' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            ()),
        'ink-sent' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            ()),
        'action-sent' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,)),
        'user-join' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,)),
        'user-leave' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,)),
        'message' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING, gobject.TYPE_STRING, gobject.TYPE_STRING, 
             gobject.TYPE_STRING, gobject.TYPE_STRING)),
        'action-message' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING, gobject.TYPE_STRING)),
        'ink-message' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING, gobject.TYPE_STRING)),
        'nudge': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,)),
        'status-change': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'custom-emoticon-received': \
            (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_PYOBJECT,)),
        'wink': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_STRING,gobject.TYPE_PYOBJECT,)),
    } 
    MESSAGE_LIMIT = 1202
    def __init__(self, id, msn, type, status = 'pending'):
        '''class contructor
        type can be 'requested' or 'invited', its 'requested' if we request the
        switchboard and guess what invited means? :P.
        status is pending initialy, then when we process the connection string is
        connected and is stablished when someone join the conversation, 
        if something goes wrong the status is closed'''
        gobject.GObject.__init__(self)
        self.id = id
        self.status = status
        self.validStatus = ['pending', 'connected', 'established',
                            'closed', 'error']
        self.invalidTransitions = {
            'connected': ['pending'],
            'established': ['pending', 'connected']
        }
        self.connectionString = ''
        self.msn = msn
        self.user = self.msn.user.lower()
        self.proxy = self.msn.proxy
        self.host = ''
        self.port = 0
        self.authenticationType = ''
        self.authenticationString = ''
        self.sessionID = ''
        self.command = ''
        self.socket = None
        self.members = {}
        self.messageQueue = [] 
        self.invitationQueue = []
        self.inkSessions = {}
        self.pendingCEs = {}
        self.packet_buffer = {}
        self.p2p_output_id = 0
        self.firstMessage = False
        self.firstUser = ''
        self.error = 0
        self.started = time.time()
    def __repr__(self):
        return '<Switchboard users: ' + str(self.members.keys()) + '>' 
    def emit(self, signal, *args):
        gobject.GObject.emit(self, signal, *args)
        if self.msn:
            self.msn.emit('switchboard::' + signal, self, signal, args)
    def parse(self):
        '''parse the connection string to fill the attributes'''
        if self.connectionString == '':
            return        
        if self.connectionString.split()[0] == 'XFR':
            (command, tid, sb, host,
             authenticationType,
             authenticationString,
             u, server) = self.connectionString.split()[:8]
        else:
            (command, sessionID, host,
             authenticationType,
             authenticationString,
             user, userName, u, server) = self.connectionString.split()[:9]
            self.sessionID = sessionID
            self.addMember(user, userName, initial=True)
        self.command = command
        self.host = host.split(':')[0]
        self.port = int(host.split(':')[1])
        self.authenticationType = authenticationType
        self.authenticationString = authenticationString
    def leaveChat(self):
        '''leave the conversation'''
        self.setStatus('closed')
        try:
            self.socket.send('OUT\r\n')
        except Exception, e:
            pass
    def socketHangup(self, obj=None):
        '''The socket got into IO_HUP or IO_ERR status'''
        common.debug(str(self) + ' hangup/error', 'switchboard')
        if self.status != 'error':
            self.setStatus('closed')
        return False
    def process(self, obj=None):
        '''read the info from Socket and process the info'''
        if self.socket == None:
            return False
        def close(message, status='closed'):
            common.debug(message, 'switchboard')
            self.setStatus(status)
        try:
            fds = self.socket.select()
            if len(fds[0]) < 1 or len(fds[1]) < 1:
                return True
            (command, tid, params) = self.socket.receiveCommand()
        except Exception, e:
            close(str(self) + ' error, closing (select/receive)\n' + str(e))
            return False
        if command == '':
            close('received empty command, the socket is broken')
            return False
        elif command == 'MSG':
            try:
                message = self.socket.receivePayload(int(params.split()[1]))
            except Exception, e:
                close(str(self) + ' error, closing (payload)\n' + str(e))
                return False
            nick = params.split()[0]
            common.debug(message, 'switchboard')
            try:
                header, body = splitMsg(message)
            except IndexError:
                common.debug("malformed message", "switchboard")
            if 'Message-ID' in header:
                message_id = header['Message-ID']
                if not self.packet_buffer.has_key(message_id):
                    self.packet_buffer[message_id] = MultiPacketBuffer()
                buf = self.packet_buffer[message_id]
                buf.append_chunk(header, body)
                if buf.is_complete():
                    header, body = buf.get_message()
                    del self.packet_buffer[message_id]
                else:
                    return # message not complete yet
            Type = ''
            if "Content-Type" in header:
                Type = header["Content-Type"]
            if Type.startswith('text/plain'):
                if not self.firstMessage:
                    self.firstMessage = True
                    self.msn.emit('new-conversation', self.firstUser, self)
                format = ''
                if 'X-MMS-IM-Format' in header:
                    format = header['X-MMS-IM-Format']
                try:
                    charset = Type.split('text/plain; charset=')[1]
                except IndexError:
                    charset = ''
                self.emit('message', tid, nick, body, format, charset)
                self.msn.emit('message-received', tid)
                if tid in self.pendingCEs:
                    for mail, msnobj, name in self.pendingCEs[tid]:
                        self.getCustomEmoticon(mail, msnobj, name)
                    del self.pendingCEs[tid]
            elif Type == 'text/x-msnmsgr-datacast':
                if body.find('ID: 1') != -1:
                    if not self.firstMessage:
                        self.firstMessage = True
                        self.msn.emit('new-conversation', self.firstUser, self)
                    self.msn.emit('nudge-received', tid)
                    self.emit('nudge', tid)
                elif body.find('ID: 2') != -1:
                    data = body.split("Data: ")[1]
                    if not self.firstMessage:
                        self.firstMessage = True
                        self.msn.emit('new-conversation', self.firstUser, self)
                    msnobj = Msnobj.createFromString(data, False)
                    self.get_wink(tid, msnobj)
                    self.emit('wink', tid, msnobj)
                elif body.find('ID: 4') != -1:
                    if not self.firstMessage:
                        self.firstMessage = True
                        self.msn.emit('new-conversation', self.firstUser, self)
                    data = body.split("Data: ")[1] 
                    self.emit('action-message', tid, data)
                    self.msn.emit('message-received', tid)
            elif Type == 'text/x-msmsgscontrol':
                self.emit('typing', tid)
            elif Type == 'text/x-mms-emoticon' or \
                 Type == 'text/x-mms-animemoticon':
                if not self.firstMessage:
                    self.firstMessage = True
                    self.msn.emit('new-conversation', self.firstUser, self)
                self.parseCustomEmoticon(message)
            elif Type == 'application/x-msnmsgrp2p' and \
                 'P2P-Dest' in header and \
                 header['P2P-Dest'].lower() == self.user.lower():
                self.msn.p2p[tid].receive_message(body)
            else:
                common.debug("Unhandled content type: " + str(header),
                    'switchboard')
        elif command == 'USR':
            if params.split(' ')[0] == 'OK':
                self.setStatus('connected')
            else:
                close('can\'t connect to switchboard: USR ' + str(params))
        elif command == 'IRO':
            (currentNumber, totalNumber, mail, nick, clientID) = \
                params.split(' ')
            self.addMember(mail.lower(), nick)
            self.emit('user-join', mail.lower())
            self.setStatus('established') 
        elif command == 'JOI':
            (nick, clientID) = params.split()[:2] # likely to break..
            self.addMember(tid, nick)
            self.emit('user-join', tid)
            self.setStatus('established') 
        elif command == 'ANS':
            if params == 'OK':
                self.setStatus('connected')
            else:
                close('can\'t connect to switchboard: ANS ' + str(params))
        elif command == 'BYE':
            wasGroupChat = self.isGroupChat()
            self.leave(tid)
            if params == '1' or wasGroupChat:
                self.emit('user-leave', tid)
        try:
            self.error = int(command)
            if len(self.members) == 0:
                close('server error %d, closing switchboard' % self.error, \
                    'error')
                return False
        except ValueError:
            pass
        return True
    def parseCustomEmoticon(self, message):
        body = message.split('\r\n\r\n')[1]
        l = body.split('\t')
        d = {}
        while len(l) > 0:
            if len(l) < 2:
                break
            shortcut = l.pop(0)
            msnobjString = l.pop(0)
            msnobj = Msnobj.createFromString(msnobjString, False)
            if msnobj != None:
                self.emit('custom-emoticon-received', shortcut, msnobj)
                filename = shortcut + '_' + msnobj.sha1d + '.tmp'
                filename = urllib.quote(filename).replace('/', '_')
                completeFileName = self.msn.cacheDir + os.sep + filename
                if not os.path.isfile(completeFileName):
                    mail = msnobj.getCreator()
                    if mail not in self.pendingCEs:
                        self.pendingCEs[mail] = []
                    self.pendingCEs[mail].append((mail, msnobj, \
                        completeFileName))
                else:
                    self.msn.emit('custom-emoticon-transfered', None, \
                        msnobj, completeFileName)
    def p2p_set_output_connected(self, value):
        '''(dis)connects socket 'output'  signal'''
        if self.socket:
            if value:
                if self.p2p_output_id == 0:
                    self.p2p_output_id = self.socket.connect('output', 
                        self.msn.p2p[self.firstUser].output_ready, self)
            else:
                self.socket.disconnect(self.p2p_output_id)
                self.p2p_output_id = 0
    def p2p_send(self, message, mail):
        '''Sends a p2p message, called by P2PManager'''
        self.sendMessage(str(message), '', 'application/x-msnmsgrp2p',
            'D', 'P2P-Dest: ' + str(mail))
    def sendMessage(self, msg='', format='', \
                     contentType='text/plain; charset=UTF-8', \
                     acknowledgeType='A', extraHeader=''):
        header = "MIME-Version: 1.0\r\n"
        if contentType != "":
            header += "Content-Type: " + contentType + "\r\n"
        if extraHeader != "":
            header += extraHeader + '\r\n'
        if contentType == 'text/plain; charset=UTF-8':
            msg = msg[:1100] #TODO: use something like MAX_MESSAGE_LENGTH
            self.emit('message-sent', msg, format, 
                contentType.split('charset=')[1])
        if format != "":
            header += format + "\r\n\r\n"
        else:
            header += "\r\n"
        if self.status != 'established':
            self.messageQueue.append({
                'msg': msg, \
                'header': header, \
                'acknowledgeType': acknowledgeType, \
            })
        elif self.status == 'established':
            try:
                self.socket.sendPayloadCommand('MSG', acknowledgeType, \
                    header + msg)
            except Exception, e:
                common.debug('socket error on switchboard, closing',
                    'switchboard')
                common.debug(str(e), 'switchboard')
                self.setStatus('closed')
    def flushMessageQueue(self):
        '''send all the unsended messages'''
        if self.status != 'established' or len(self.messageQueue) == 0:
            return
        for i in self.messageQueue:
            try:
                self.socket.sendPayloadCommand('MSG', \
                    i['acknowledgeType'], i['header'] + i['msg'])
            except Exception, e:
                common.debug('socket error on switchboard, ' + \
                    'closing switchboard', 'switchboard')
                common.debug(str(e), 'switchboard')
                self.setStatus('closed')
        self.messageQueue = []
        common.debug('message queue flushed', 'switchboard')
    def sendNudge(self):
        '''a easy method to send a nudge'''
        self.sendMessage('ID: 1\r\n\r\n', '', 'text/x-msnmsgr-datacast')
        self.emit('nudge-sent')
    def sendAction(self, data):
        '''a easy method to send an action message - this works with MSNC6+'''
        self.sendMessage('ID: 4\r\nData: %s\r\n' % data, '',
            'text/x-msnmsgr-datacast')
        self.emit('action-sent', data)
    def sendIsTyping(self):
        '''a easy method to send the is typing message'''
        self.sendMessage('\r\n', '', 'text/x-msmsgscontrol\r\nTypingUser: ' + 
            self.user)
    def invite(self, mail):
        '''invite somebody to the switchboard conversation'''
        mail = mail.lower()
        if self.firstUser == '':
            self.firstUser = mail
        if self.status in ('connected', 'established') and \
           not mail in self.members.keys():
            try:
                self.socket.sendCommand('CAL', mail)
            except Exception, e:
                common.debug('socket error on switchboard, ' +
                    'closing switchboard', 'switchboard')
                common.debug(str(e), 'switchboard')
                self.setStatus('closed')
                return
            if mail in self.invitationQueue:
                self.invitationQueue.pop(self.invitationQueue.index(mail))
        elif not mail in self.invitationQueue:
            self.invitationQueue.append(mail.lower())
    def leave(self, mail):
        '''remove the mail from the members list'''
        if mail.lower() in self.members.keys():
            if len(self.members) == 1:
                self.setStatus('closed')
            else:
                del self.members[mail.lower()]
    def setConnectionString(self, connectionString):
        '''we received the conecction string (the first response of a XFR
        or the RNG) and here we process it'''
        self.connectionString = connectionString        
        self.parse()
        self.connectSocket()
    def connectSocket(self):
        '''connect the socket'''
        if self.proxy != None:
            self.socket = Socket.HTTPSocket(self.host, self.port, \
                self.proxy, 'SB')
        else:
            try:
                self.socket = Socket.Socket(self.host, self.port)
                self.socket.establish(5)
            except Exception, e:  # socket.error seems to fail sometimes
                common.debug("can't connect to switchboard, socket error", \
                    'switchboard')
                common.debug(str(e), 'switchboard')
                self.setStatus('closed')
                return
        self.socket.connect('input', self.process)
        self.socket.connect('hangup', self.socketHangup)
        if self.command == 'XFR':
            try:
                self.socket.sendCommand("USR", self.user + " " + \
                    self.authenticationString)
            except Exception, e:
                common.debug('socket error on switchboard, ' + \
                    'closing switchboard', 'switchboard')
                common.debug(str(e), 'switchboard')
                self.setStatus('closed')
        else: #RNG
            try:
                self.socket.sendCommand("ANS", self.user + " " + \
                    self.authenticationString + " " + self.sessionID)
            except Exception, e:
                common.debug('socket error on switchboard, ' + \
                    'closing switchboard', 'switchboard')
                common.debug(str(e), 'switchboard')
                self.setStatus('closed')
        if len(self.members) == 1:
            self.msn.p2p[self.firstUser].register(self)
        else:
            self.msn.p2p[self.firstUser].unregister(self)
    def addMember(self, mail, nick, initial=False):
        '''add a member to the members dict'''
        mail = mail.lower()
        self.members[mail] = nick.replace('%20', ' ')
        if not initial:
            if len(self.members) == 1:
                self.firstUser = mail
                self.msn.p2p[mail].register(self)
            else:
                self.msn.p2p[mail].unregister(self)
    def setStatus(self, status):
        '''set the status, the status can be 'pending', 'stablished' or 'closed'
        PLEASE change the status here and not directly because we may want
        to do thing when a status is changed
        XXX-DX: i think we need a property here, we can't ask "PLEASE" that
        way...'''
        if status in self.validStatus and self.status != status:
            if self.invalidTransitions.has_key(self.status) and \
               status in self.invalidTransitions[self.status]:
                return
            self.status = status
            if status in ('closed', 'error'):
                self.msn.p2p[self.firstUser].unregister(self)
                if self.socket:
                    self.socket.hangup()
            if status == 'error':
                try:
                    self.socket.send('OUT\r\n')
                except Exception, e:
                    pass
                self.socket = None
            if status == 'connected':
                for i in self.invitationQueue:
                    self.invite(i)
                self.invitationQueue = []
            elif self.status == 'established':
                self.flushMessageQueue()
            self.emit('status-change')
    def getId(self): #FIXME: getter
        '''return the id of the switchboard, this is a unique identification
        of the switchboards. The value is the value of the Trid of the
        command that created the switchboard or the sessionid if the other
        user started the conversation, the value doesnt matter, what
        matters is that its unique.'''
        return self.id
    def getOnlineUsers(self):
        '''This method returns a list ol mails of the contacts who are
        not offline'''
        return self.msn.contactManager.getOnlineUsers()
    def getMembers(self):
        '''return a list of the members in the switchboard'''
        return self.members.keys()
    def isGroupChat(self):
        return (len(self.members) > 1)
    def getInvitedMembers(self):
        '''return a list of the members invited but not joined'''
        return self.invitationQueue
    def getDisplayPicture(self, email):
        '''start a P2P session to get the display picture'''
        if self.msn is None:
            return
        email = email.lower()
        contact = self.msn.contactManager.getContact(email)
        if contact == None:
            common.debug('contact (' + email + ') not found in ' + \
                'getDisplayPicture', 'switchboard')
            return
        msnobj = contact.msnobj
        if msnobj == None:
            common.debug(email + ' has no msnobj in getDisplayPicture', \
                'switchboard')
            return
        filename = os.path.join(self.msn.cacheDir, contact.displayPicturePath)
        if not os.path.exists(filename):
            p2p.transfers.Receiver(self.msn.p2p[email], msnobj)
        else:
            self.msn.emit("display-picture-changed", self, msnobj, email)
    def getCustomEmoticon(self, email, msnobj, filename):
        email = email.lower()
        msnobj.filename = filename
        p2p.transfers.Receiver(self.msn.p2p[email], msnobj)
    def sendCustomEmoticons(self, message):
        msnObjs = []
        msnObj = ''
        i = 0
        msnOM = self.msn.getMsnObjectsManager()
        for CE in msnOM.getIds():
            if i == 4:
                i = 0
                msnObjs.append(msnObj)
                msnObj = ''
            if message.find(CE) != -1:
                msnObj += CE + '\t' + str(msnOM.getById(CE)) + '\t'
                i += 1
        if msnObj != '':
            msnObjs.append(msnObj)
        for msnObj in msnObjs:
            self.sendMessage(msnObj, contentType='text/x-mms-animemoticon')
    def get_wink(self, email, msnobj):
        p2p.transfers.Receiver(self.msn.p2p[email], msnobj)
class MultiPacketBuffer:
    '''this class represents the buffer for a multichunk MIME-message'''
    def __init__(self):
        '''initialize the buffer'''
        self.chunks_total = 0
        self.chunks_received = 0
        self.body = []
        self.header = {}
    def append_chunk(self, header, body):
        '''appends a new chunk to the buffer'''
        if 'Chunks' in header:
            self.chunks_total = int(header['Chunks'])
        for key, val in header.iteritems():
            if key not in ['Chunks', 'Chunk', 'Message-ID']:
                self.header[key] = val
        self.chunks_received += 1
        self.body.append(body)
    def get_message(self):
        '''returns a (header, body) tuple.
        header is a dict, body is a string'''
        return (self.header, ''.join(self.body))
    def is_complete(self):
        '''returns True if we have all chunks, False otherwise'''
        return (self.chunks_received == self.chunks_total)
def splitMsg(message):
    '''return header(dict), body(str)'''
    part = message.split('\r\n\r\n')
    def htuple(x):
        parts = x.split(': ')
        return (parts[0], ': '.join(parts[1:]))
    header = dict([htuple(i) for i in part[0].split('\r\n')])
    body = '\r\n\r\n'.join(part[1:])
    return header, body
