import struct
import urllib
import md5
import Msnobj
import Switchboard
import common
_PRODUCT_KEY = 'O4BG@C7BWLYQX?5G'
_PRODUCT_ID = 'PROD01065C%ZFN6F' 
MSNP11_MAGIC_NUM = 0x0E79A9C1
def doChallenge(chlData):
    md5digest = md5.md5(chlData + _PRODUCT_KEY).digest()
    md5Ints = struct.unpack("<llll", md5digest)
    md5Ints = [(x & 0x7fffffff) for x in md5Ints]
    chlData += _PRODUCT_ID
    amount = 8 - len(chlData) % 8
    chlData += "".zfill(amount)
    chlInts = struct.unpack("<%di" % (len(chlData)/4), chlData)
    high = 0
    low = 0
    i = 0
    while i < len(chlInts) - 1:
        temp = chlInts[i]
        temp = (MSNP11_MAGIC_NUM * temp) % 0x7FFFFFFF
        temp += high
        temp = md5Ints[0] * temp + md5Ints[1]
        temp = temp % 0x7FFFFFFF
        high = chlInts[i + 1]
        high = (high + temp) % 0x7FFFFFFF
        high = md5Ints[2] * high + md5Ints[3]
        high = high % 0x7FFFFFFF
        low = low + high + temp
        i += 2
    high = littleEndify((high + md5Ints[1]) % 0x7FFFFFFF)
    low = littleEndify((low + md5Ints[3]) % 0x7FFFFFFF)
    key = (high << 32L) + low
    key = littleEndify(key, "Q")
    longs = [x for x in struct.unpack(">QQ", md5digest)]
    longs = [littleEndify(x, "Q") for x in longs]
    longs = [x ^ key for x in longs]
    longs = [littleEndify(abs(x), "Q") for x in longs]
    out = ""
    for x in longs:
        x = hex(long(x))
        x = x[2:-1]
        x = x.zfill(16)
        out += x.lower()
    return out
def littleEndify(num, c="L"):
    return struct.unpack(">" + c, struct.pack("<" + c, num))[0]
class SignalHandler( object ):
    '''This class represent the default signal handler for core
    you should inherit from this class, if you know what you are doing
    you can create your own base class without inheriting from this one'''
    def __init__( self, msn ):
        '''contructor'''
        self.msn = msn
        self.socket = self.msn.socket
    def challenge( self, msnp, command, tid, params ):
        '''reply a challenge made from the server'''
        out = doChallenge( params )
        self.socket.sendPayloadCommand( 'QRY' , _PRODUCT_ID , out )
    def statusChange( self, msnp, command, tid, params ):
        '''handle a status change'''
        global flag
        t = params.split( " " )
        status = t[ 0 ]
        email = t[ 1 ].lower()
        nick = urllib.unquote(t[2])
        nick = nick.decode('utf-8', 'replace').encode('utf-8')
        oldStatus = self.msn.contactManager.getContactStatus( email )
        self.msn.contactManager.setContactStatus( email, status )
        self.msn.contactManager.setContactNick( email, nick )
        if len( t ) == 5:
            msnobj = t[ 4 ]
            contact = self.msn.contactManager.getContact( email )
            if contact != None:
                msnObject = Msnobj.createFromString( msnobj )
                if contact.msnobj == None or \
                   (msnObject != None and msnObject != contact.msnobj):
                    contact.msnobj = msnObject
                    self.msn.emit( 'msnobj-changed', msnObject, (oldStatus=='FLN') )     
        if self.msn.contactManager.getContactNick( email ) != nick:
            self.msn.emit( 'nick-changed', email, nick )     
        if self.msn.contactManager.getContactStatus(email) != status:
            self.msn.emit('contact-status-change', email, status)   
        contact = self.msn.contactManager.getContact(email)
        self.msn.emit('user-attr-changed', contact)
    def statusOnline( self, msnp, command, tid, params ):
        '''handle a friend going online'''
        status = tid
        t = params.split(' ')
        email = t[ 0 ].lower()
        nick = urllib.unquote(t[1])
        nick = nick.decode('utf-8', 'replace').encode('utf-8')
        oldStatus = self.msn.contactManager.getContactStatus( email )
	
        self.msn.contactManager.setContactStatus( email, status )
        if len( t ) == 4:
            msnobj = t[ 3 ]
            contact = self.msn.contactManager.getContact( email )
            if contact != None:
                msnObject = Msnobj.createFromString( msnobj )
                if contact.msnobj == None or \
                   (msnObject != None and \
                    msnObject != contact.msnobj):
                    contact.msnobj = msnObject
                    self.msn.emit( 'msnobj-changed', msnObject, 
                                    (oldStatus=='FLN') )
        if self.msn.contactManager.getContactNick( email ) != nick:
            self.msn.emit( 'nick-changed', email, nick )
        self.msn.contactManager.setContactNick( email, nick )
        if oldStatus != status:
            common.debug('%s (%s) changed status from %s to %s' % \
            (nick, email, oldStatus, status), 'signal handler')
            self.msn.emit( 'user-online', email, oldStatus )
            self.msn.emit( 'contact-status-change', email, status )
        contact = self.msn.contactManager.getContact( email )
        self.msn.emit( 'user-attr-changed', contact )   
    def statusOffline( self, msnp, command, tid, params ):
        '''handle a friend going offline'''
        email = tid.lower()
        self.msn.contactManager.setContactStatus( email, 'FLN' )
        common.debug('%s disconnected' % (email,), 'signal handler')
        self.msn.emit( 'user-offline', email )
        contact = self.msn.contactManager.getContact( email )
        self.msn.emit( 'user-attr-changed', contact )
        self.msn.emit('contact-status-change', email, 'FLN')
    def switchboardInvitation( self, msnp, command, tid, params ):
        '''handle the switchboard invitation, this is the first message
        that a buddy send to you when he wants to chat with you'''
        completeCommand = command + ' ' + tid + ' ' + params
        if len( completeCommand.split( ' ' ) ) != 9:
            print 'invalid message in switchboardInvitation, not opening switchboard'
            return
        ( command, sessionID, host, authenticationType, 
        authenticationString, user, userName, u, 
        server ) = completeCommand.split( ' ' )
        user = user.lower()
        sb = Switchboard.Switchboard( tid, msnp, 'invited' )
        msnp.on_new_switchboard(sb)
        sb.setConnectionString( completeCommand )
    def serverMessage( self, msnp, command, tid, params, payload ):
        '''called when we receive a MSG in the main socket'''
        payloadData = {}
        for line in payload.split('\r\n'):
            try:
                key, value = line.split(': ',1)
                payloadData.update({key:value})
                common.debug(line)
            except:
                pass
        if payloadData.has_key('Content-Type') == True:
            ct = payloadData['Content-Type'].split('; ')[0]
            if( ct == 'text/x-msmsgsinitialemailnotification' ):
                self.msn.inboxUnreadMessages = int(
                    payloadData['Inbox-Unread'] )
                self.msn.otherMessagesUnreaded=int(
                    payloadData['Folders-Unread'] )
                self.msn.emit( 'initial-mail-notification' )
            elif( ct == 'text/x-msmsgsinitialmdatanotification' ):
                self.msn.msnOIM.parseMailData( payloadData['Mail-Data'] )
                try:
                    self.msn.inboxMessages=int(
                        payload.split('<I>')[1].split('</I>')[0])
                except: pass    
                try:
                    self.msn.inboxUnreadMessages=int(
                        payload.split('<IU>')[1].split('</IU>')[0])
                except: pass
                try:
                    self.msn.otherMessages=int(
                        payload.split('<O>')[1].split('</O>')[0])
                except: pass
                try:
                    self.msn.otherMessagesUnreaded=int(
                        payload.split('<OU>')[1].split('</OU>')[0])
                except: pass
                self.msn.emit( 'initial-mail-notification' )
                if self.msn.msnOIM.getMessageCount() > 0:
                    self.msn.emit( 'offline-message-waiting', 
                        self.msn.msnOIM )
            elif( ct == 'text/x-msmsgsemailnotification' ):
                From = payloadData['From']
                FromAddr = payloadData['From-Addr']
                Subject = payloadData['Subject']
                MessageURL = payloadData['Message-URL']
                PostURL = payloadData['Post-URL']
                id = payloadData['id']
                if payloadData['Dest-Folder'] == 'ACTIVE':
                    self.msn.inboxMessages += 1
                    self.msn.inboxUnreadMessages +=1
                else:
                    self.msn.otherMessages += 1
                    self.msn.otherMessagesUnreaded +=1  
                self.msn.emit( 'new-mail-notification', 
                    From, FromAddr, Subject, MessageURL , PostURL, id )
            elif( ct == 'text/x-msmsgsactivemailnotification' ):
                src = payloadData['Src-Folder']
                dest = payloadData['Dest-Folder']
                delta = int( payloadData['Message-Delta'] )
                if dest == 'trAsH':
                    if src == 'ACTIVE':
                        self.msn.inboxMessages -= delta
                        self.msn.inboxUnreadMessages -=delta
                    else:
                        self.msn.otherMessages -= delta
                        self.msn.otherMessagesUnreaded -=delta
                elif dest == 'ACTIVE':
                    if src == 'ACTIVE':
                        self.msn.inboxUnreadMessages -= delta
                    elif src != 'trAsH':
                        self.msn.otherMessages -= delta
                        self.msn.otherMessagesUnreaded -=delta
                        self.msn.inboxMessages += delta
                        self.msn.inboxUnreadMessages +=delta
                    else:
                        self.msn.inboxMessages += delta
                        self.msn.inboxUnreadMessages +=delta
                else:
                    if src == 'ACTIVE':
                        self.msn.inboxMessages -= delta
                        self.msn.inboxUnreadMessages -=delta
                        self.msn.otherMessages += delta
                        self.msn.otherMessagesUnreaded +=delta
                    elif src == dest:
                        self.msn.otherMessagesUnreaded -=delta
                    else:
                        self.msn.otherMessages += delta
                        self.msn.otherMessagesUnreaded +=delta
                self.msn.emit( 'mail-movement-notification' )
            elif( ct == 'text/x-msmsgsoimnotification' ):
                self.msn.msnOIM.parseMailData( payloadData['Mail-Data'] )
                if self.msn.msnOIM.getMessageCount() > 0:
                    self.msn.emit( 'offline-message-waiting', 
                        self.msn.msnOIM )
            elif ct == 'text/x-msmsgsprofile':
               msnp.parse_demographics(payload) 
            else:
                print 'Unhandled Content Type:', ct
        else:
            print 'Unhandled MSG, printing Raw Data:'
            print payload
