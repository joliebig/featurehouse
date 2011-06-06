from uuid import uuid4
from string import Template
import XmlTemplates
import SignalHandler
import SoapManager
from xml.dom.minidom import parseString
import email
import time
import common
from xml.parsers.expat import ExpatError
class SoapHandle( object ):
    def __init__( self, msnOIM, template, d ):
        self.template = template
        self.d = d
        self._passportid = msnOIM._passportid
        self.msnOIM = msnOIM
        self.msn = self.msnOIM.msn
        self.retrys = 0
    def handle(self, response):
        '''Handle the Soap Request'''
        to, message = self._parseSentMessage()
        try:
            xml = parseString(response.body)
        except ExpatError, e:
            self.msn.emit('send-message-error', to, message, str(e))
            print to, message, str(e)
            return
        if( xml.getElementsByTagName('TweenerChallenge').length>0 ):
            try:
                self.tweenerChallenge(xml)
            except Exception, e:
                self.msn.emit( 'send-message-error', to, message, str(e))
            return self.retry(response)
        if(xml.getElementsByTagName( 'LockKeyChallenge').length>0 ):
            self.lockKeyChallenge( xml )
            return self.retry(response)
        if xml.getElementsByTagName('GetMessageResult').length > 0:
            self.msn.emit('offline-message-received',
                self.msnOIM.parseOIM(response))
        fault = ''
        if xml.getElementsByTagName('faultcode').length > 0:
            fault = xml.getElementsByTagName('faultcode')[0]\
                .firstChild.nodeValue
            if fault.strip() != '':
                print "OIM: line 69:",
                print to, message, fault
                return
    def _parseSentMessage(self):
        try:
            to = self.d['to']
            message = self.d['content'].decode('base64')
            return (to, message)
        except:
            return ('', '')
    def retry(self, response):
        '''Retry to send Soap Request with new data'''
        if self.retrys < 3:
            self.retrys += 1
            body = self.template.safe_substitute( self.d )
            newRequest = SoapManager.SoapRequest( response.action,
                response.host, response.port, response.path, body,
                response.callback )
            SoapManager.put( newRequest )
        elif self.retrys > 3:
            to, message = self._parseSentMessage()
            self.msn.emit( 'send-message-error', to, message, 'Auth Error' )
    def tweenerChallenge( self, xml ):
        '''Retreive new PassportId'''
        hash = xml.getElementsByTagName('TweenerChallenge')[0].firstChild.nodeValue
        self.msn.passportReAuth(hash)
        self.msnOIM._passportid =  self._passportid = self.msn.passportid
        self.d['passport'] = self._passportid.replace('&','&amp;')
        s=self._passportid[2:]
        t,p = s.split('&p=')
        self.msnOIM._t = t
        self.msnOIM._p = p
        self.d['t'], self.d['p'] = t, p
    def lockKeyChallenge( self, xml ):
        '''Retreive new lockKey'''
        hash = xml.getElementsByTagName('LockKeyChallenge')[0].firstChild.nodeValue
        lockKey = SignalHandler.doChallenge(str(hash))
        self.msnOIM._lockKey = lockKey
        self.msnOIM._lockKey = self.d['lockKey'] = lockKey
class MsnOIM(object):
    '''This is the class to handle msn oim'''
    def __init__(self,msn):
        self.msn=msn
        self._lockKey=''
        self._passportid=''
        self._seq=1
        self._t=''
        self._p=''
        self.msg_list = []
    def destroy( self ):
        self.msn = None
        self.msg_list = []
    def getMessageCount( self ):
        '''Return the number of messages waiting'''
        return len( self.msg_list )
    def getMessages( self ):
        '''Return the msg_lst'''
        return self.msg_list
    def send(self,to,msg):
        '''Send an offline message to friend'''
        common.debug('OIM To: ' + str(to), 'msnoim')
        soapTemplate=Template(XmlTemplates.send_message)
        nick='=?%s?%s?=%s?=' % ('utf-8','B',self.msn.nick.encode('base64').strip())
        d=dict(memberName=self.msn.user,friendlyName=nick,ver='MSNP13',buildVer='8.0.0792',to=to,passport=self._passportid.replace('&','&amp;'),appid=SignalHandler._PRODUCT_ID,lockKey=self._lockKey,seqNum=self._seq,runId=uuid4().__str__(),content=msg.encode('base64').strip())
        self._seq=self._seq+1
        self.soapRequest('http://messenger.msn.com/ws/2004/09/oim/Store','Send OIM','ows.messenger.msn.com',443,'/OimWS/oim.asmx',soapTemplate, d)
    def parseMailData(self,mailData):
        ''' Parse the mail data xml'''
        if mailData == 'too-large':
            return self.tooLarge()
        mailDataDom=parseString(mailData)
        for m in mailDataDom.getElementsByTagName('M'):
            item={}
            for node in m.childNodes:
                if node.nodeType!=node.TEXT_NODE and node.childNodes.length==1:
                    item[node.nodeName]=node.firstChild.nodeValue
            self.msg_list.append(item)        
        return False
    def parseOIM( self, response ):
        data = response.body.split('<GetMessageResult>')[1].split('</GetMessageResult>')[0]
        oim = email.message_from_string( data )
        user = self.parseFrom( oim.get('From') )
        message = oim.get_payload().decode('base64')
        date = email.Utils.parsedate_tz( oim.get( 'Date' ) )
        date = time.localtime( email.Utils.mktime_tz( date ) )
        return [user, date, message]
    def tooLarge( self ):
        '''Return the mail data using soap if there are too many OIMs'''
        t,p=self.msn.passportid[2:].split('&p=')
        request = SoapManager.SoapRequest( 'http://www.hotmail.msn.com/ws/2004/09/oim/rsi/GetMetadata',\
                                'rsi.hotmail.com', 443,\
                                '/rsi/rsi.asmx', \
                                XmlTemplates.getMailData%(t,p) )
        SoapManager.put( request )
        response = SoapManager.get()
        return self.parseMailData( response.body )
    def retrieve(self,message_id):
        '''retreive an oim message by message_id'''
        if(self.msn.passportid!=''):
            s=self.msn.passportid[2:]
            self._t,self._p=s.split('&p=')
        soapTemplate=Template(XmlTemplates.retreive_message)
        d=dict(t=self._t,p=self._p,mid=message_id)
        self.soapRequest('http://www.hotmail.msn.com/ws/2004/09/oim/rsi/GetMessage','Retreive OIM','rsi.hotmail.com',443,'/rsi/rsi.asmx',soapTemplate,d)
        self.delete( message_id )
    def getMessage( self, xmlString ):
        '''Return the message from the xml'''
        dom=parseString(xmlString)
        return dom.getElementsByTagName('GetMessageResult')[0].firstChild.nodeValue
    def getSender( self, xml ):
        '''return the From of the message'''
        try:
            return xml.split( 'From: =?' )[ 1 ].split( '&lt;' )[ 1 ].split( '&gt;' )[ 0 ]
        except:
            return '?'
    def parseFrom( self, mail ):
        '''Parse encoded from'''
        data = mail.split('?')
        if len(data) < 3:
            return {'name': '', 'addr': ''}
        if data[2] == 'B':
            name = data[3].decode('base64').decode( data[1] )
            addr = data[4][6:][:-4]
        elif data[2] == 'Q':
            name = addr = 'TODO'
        return { 'name':name, 'addr':addr }
    def delete(self,message_id):
        '''Delete an oim message'''
        if(self.msn.passportid!=''):
            s=self.msn.passportid[2:]
            self._t,self._p=s.split('&p=')
        soapTemplate=Template(XmlTemplates.delete_message)
        d=dict(t=self._t,p=self._p,mid=message_id)
        self.soapRequest('http://www.hotmail.msn.com/ws/2004/09/oim/rsi/DeleteMessages','Delete OIM','rsi.hotmail.com',443,'/rsi/rsi.asmx',soapTemplate,d)
    def soapRequest(self,url,description,server,port,path,soapTemplate,d):
        '''Soap request method handle Tweener challenge and Lock key challenge'''
        soapHandle = SoapHandle( self, soapTemplate, d )
        body=soapTemplate.safe_substitute(d)
        request = SoapManager.SoapRequest( url, server, port, path, body, soapHandle.handle )
        SoapManager.put( request )
