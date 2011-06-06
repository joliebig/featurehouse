import email
import time
from uuid import uuid4
from string import Template
from xml.parsers.expat import ExpatError
from xml.dom.minidom import parseString
import SignalHandler
import common
import soap.templates
import soap.manager
class SoapHandle(object):
    def __init__(self, msnOIM, template, d):
        self.template = template
        self.d = d
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
        if(xml.getElementsByTagName('LockKeyChallenge').length>0):
            self.lockKeyChallenge(xml)
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
            body = self.template.safe_substitute(self.d)
            soap.manager.repeat_request(response, body)
        elif self.retrys > 3:
            to, message = self._parseSentMessage()
            self.msn.emit( 'send-message-error', to, message, 'Auth Error' )
    def lockKeyChallenge(self, xml):
        '''Retreive new lockKey'''
        hash = xml.getElementsByTagName('LockKeyChallenge')[0]\
                                                .firstChild.nodeValue
        lockKey = SignalHandler.doChallenge(str(hash))
        self.msnOIM._lockKey = lockKey
        self.msnOIM._lockKey = self.d['lockKey'] = lockKey
class MsnOIM(object):
    '''This is the class to handle msn oim'''
    def __init__(self,msn):
        self.msn = msn
        self._lockKey = ''
        self._passportid = ''
        self._t = self._p = ''
        self._seq = 1
        self.msg_list = []
    def destroy(self):
        self.msn = None
        self.msg_list = []
    def getMessageCount(self):
        '''Return the number of messages waiting'''
        return len(self.msg_list)
    def getMessages(self):
        '''Return the msg_lst'''
        return self.msg_list
    def get_tokens(self):
        self._passportid = self.msn.tokens['messengersecure.live.com']\
                ['security'].replace('&', '&amp;')
        self._t, self._p = self.msn.tokens['messenger.msn.com']\
                                           ['security'][2:].split('&p=')
    def send(self, to, msg):
        '''Send an offline message to friend'''
        self.get_tokens()
        common.debug('OIM To: ' + str(to), 'msnoim')
        soapTemplate = Template(soap.templates.send_message)
        nick = '=?%s?%s?=%s?=' % ('utf-8','B', 
                     self.msn.nick.encode('base64').strip())
        d = dict(memberName = self.msn.user, friendlyName = nick,
                 ver = 'MSNP15', buildVer='8.0.0792',
                 to = to, passport = self._passportid,
                 appid = SignalHandler._PRODUCT_ID,
                 lockKey = self._lockKey, seqNum = self._seq,
                 runId = uuid4().__str__(), content = msg.encode('base64')\
                          .strip())
        self._seq=self._seq+1
        self.soapRequest('http://messenger.live.com/ws/2006/09/oim/Store2',
                'Send OIM','ows.messenger.msn.com',443,'/OimWS/oim.asmx',
                 soapTemplate, d)
    def parseMailData(self, mailData):
        ''' Parse the mail data xml'''
        if mailData == 'too-large':
            return self.tooLarge()
        mailDataDom = parseString(mailData)
        for m in mailDataDom.getElementsByTagName('M'):
            item={}
            for node in m.childNodes:
                if node.nodeType!=node.TEXT_NODE: #and node.childNodes.length==1:
                    item[node.nodeName]=node.firstChild.nodeValue
            self.msg_list.append(item)
        return False
    def parseOIM(self, response):
        data = response.body.split('<GetMessageResult>')\
                                   [1].split('</GetMessageResult>')[0]
        oim = email.message_from_string(data)
        user = self.parseFrom(oim.get('From'))
        message = oim.get_payload().decode('base64')
        date = email.Utils.parsedate_tz(oim.get('Date'))
        date = time.localtime(email.Utils.mktime_tz(date))
        return (user, date, message)
    def tooLarge(self):
        '''Return the mail data using soap if there are too many OIMs'''
        request = soap.manager.SoapRequest(
                'http://www.hotmail.msn.com/ws/2004/09/oim/rsi/GetMetadata',\
                                'rsi.hotmail.com', 443,\
                                '/rsi/rsi.asmx', \
                                soap.templates.getMailData%(self._t, self._p))
        soap.manager.put(request)
        response = soap.manager.get()
        return self.parseMailData(response.body)
    def retrieve(self):
        '''retreive an oim message by message_id'''
        self.get_tokens()
        soapTemplate = Template(soap.templates.retreive_message)
        for message in self.msg_list:
            template_dict = dict(t=self._t, p=self._p, mid=message['I'])
            self.soapRequest(
                'http://www.hotmail.msn.com/ws/2004/09/oim/rsi/GetMessage',
                'Retreive OIM','rsi.hotmail.com', 443 ,'/rsi/rsi.asmx',
                soapTemplate, template_dict)
            self.delete(message)
    def getMessage(self, xmlString):
        '''Return the message from the xml'''
        dom = parseString(xmlString)
        return dom.getElementsByTagName('GetMessageResult')[0].firstChild.nodeValue
    def getSender(self, xml):
        '''return the From of the message'''
        try:
            return xml.split('From: =?')[1].split('&lt;')[1].split('&gt;')[0]
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
    def delete(self, message):
        '''Delete an oim message'''
        msg_id = message['I']
        self.msg_list.remove(message)
        soapTemplate = Template(soap.templates.delete_message)
        d = dict(t = self._t, p = self._p, mid = msg_id)
        self.soapRequest(
                'http://www.hotmail.msn.com/ws/2004/09/oim/rsi/DeleteMessages',
                'Delete OIM', 'rsi.hotmail.com',443 ,'/rsi/rsi.asmx',
                soapTemplate, d)
    def soapRequest(self,url,description,server,port,path,soapTemplate,d):
        '''Soap request method handle 
        Lock key challenge'''
        soapHandle = SoapHandle(self, soapTemplate, d)
        body = soapTemplate.safe_substitute(d)
        soap.manager.do_request(url, server, port, path, body,
                                              soapHandle.handle)
