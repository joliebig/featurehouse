'''this module includes all MSNSLP related stuff'''
import random
class SLPMessage(object):
    '''Represents a MSNSLP/1.0 message
    No session initiation here, it's just a higher level interface
    to build/parse SLP messages'''
    TEMPLATE = '%s\r\n' \
        'To: <msnmsgr:%s>\r\n' \
        'From: <msnmsgr:%s>\r\n' \
        'Via: MSNSLP/1.0/TLP ;branch={%s}\r\n' \
        'CSeq: %s \r\n' \
        'Call-ID: {%s}\r\n' \
        'Max-Forwards: %s\r\n' \
        'Content-Type: %s\r\n' \
        'Content-Length: %s\r\n' \
        '\r\n' \
        '%s'
    INVITE_METHOD = 'INVITE MSNMSGR:%s MSNSLP/1.0'
    BYE_METHOD = 'BYE MSNMSGR:%s MSNSLP/1.0'
    ACK_METHOD = 'ACK MSNMSGR:%s MSNSLP/1.0'
    OK_STATUS = 'MSNSLP/1.0 200 OK'
    DECLINE_STATUS = 'MSNSLP/1.0 603 Decline'
    CONTENT_SESSION_REQ = 'application/x-msnmsgr-sessionreqbody'
    CONTENT_SESSION_CLOSE = 'application/x-msnmsgr-sessionclosebody'
    CONTENT_TRANS_RESP = 'application/x-msnmsgr-transrespbody'
    CONTENT_TRANS_REQ = 'application/x-msnmsgr-transreqbody'
    EUFGUID_DISPLAY = '{A4268EEC-FEC5-49E5-95C3-F126696BDBF6}'
    EUFGUID_FILE = '{5D3E02AB-6190-11D3-BBBB-00C04F795683}'
    EUFGUID_WEBCAM = '{4BD96FC0-AB17-4425-A14A-439185962DC8}'  
    EUFGUID_WEBCAM_ASK = '{1C9AA97E-9C05-4583-A3BD-908A196F1E92}'
    def __init__(self, message=None, session=None):
        '''initializes most attributes to empty'''
        self.method = ''
        self.to_ = ''
        self.from_ = ''
        self.via = ''
        self.cseq = '0'
        self.call_id = ''
        self.max_forwards = '0'
        self.content_type = ''
        self.body = {}
        if session:
            self.to_ = session.to_
            self.from_ = session.from_
            self.via = session.via
            self.call_id = session.call_id
        elif message:
            self.parse(message)
    def parse(self, message):
        '''parses a message and sets the attributes to rebuild it
        the param must be without binary headers
        SLPErrors should me handled replying 500 internal server error'''
        try:
            header, body = message.split('\r\n\r\n', 1)
        except ValueError:
            raise SLPError("The message/body separator is missing")
        header = header.split("\r\n")
        body = body.split("\r\n")
        self.body = to_dict(body)
        try:
            self.method = header.pop(0)
        except IndexError:
            raise SLPError("SLP Message header is empty "
                "(or starts with \\r\\n\\r\\n)")
        headerparams = to_dict(header)
        try:
            self.to_ = between(headerparams['To'], "<msnmsgr:", ">")
            self.from_ = between(headerparams['From'], "<msnmsgr:", ">")
            self.via = between(headerparams['Via'], ";branch={", "}")
            self.cseq = headerparams['CSeq'].strip()
            self.call_id = between(headerparams['Call-ID'], "{", "}")
            self.max_forwards = headerparams['Max-Forwards'].strip()
            self.content_type = headerparams['Content-Type'].strip()
        except KeyError:
            raise SLPError("Some of the required MSNSLP fields are missing")
    def __str__(self):
        '''return the message as a string'''
        context = None
        if 'Context' in self.body:
            context = self.body.pop('Context')  # take it out
        items = self.body.items()
        items.sort()
        if context:  # add it back 
            items.append(('Context', context))
            self.body['Context'] = context
        lines = []
        for key, value in items:
            lines.append(str(key) + ': ' + str(value))
        body = '\r\n'.join(lines) + '\r\n\r\n\0'
        return SLPMessage.TEMPLATE % (self.method, self.to_, self.from_,
            self.via, self.cseq, self.call_id, self.max_forwards,
            self.content_type, len(body), body)
class SLPError(Exception):
    '''A simple, named, easy to catch exception for all SLP parsing errors'''
    pass
def random_id():  #XXX
    '''return a random id according to msnp2p rules'''
    guid = []
    guid.append('%04X' % random.randint( 0x1111 , 0xAAFF ))
    guid.append('%04X' % random.randint( 0x1111 , 0xAAFF ) + "-")
    guid.append('%04X' % random.randint( 0x1111 , 0xAAFF ) + "-")
    guid.append('%04X' % random.randint( 0x1111 , 0xAAFF ) + "-")
    guid.append('%04X' % random.randint( 0x1111 , 0xAAFF ) + "-")
    guid.append('%04X' % random.randint( 0x1111 , 0xAAFF ))
    guid.append('%04X' % random.randint( 0x1111 , 0xAAFF ))
    guid.append('%04X' % random.randint( 0x1111 , 0xAAFF ))
    return ''.join(guid)
def contains(string, substring):
    '''return True if string contains substring'''
    return (string.find(substring) != -1)
def between(string, start, end):
    '''Returns the string between start and end
    I wouldn't recommend using this outside this module...'''
    try:
        return string.split(start)[1].split(end)[0]
    except IndexError:
        raise SLPError("Couldn't parse field " + repr(string))
def to_dict(lines):
    '''Splits the list "lines" by ": " and returns a dict'''
    parameters = {}
    for line in lines:
        try:
            key, value = line.split(': ')
            parameters[key] = value
        except ValueError:
            continue
    return parameters
def invite(session, context, eufguid=SLPMessage.EUFGUID_DISPLAY):
    message = SLPMessage(session=session)
    message.method = SLPMessage.INVITE_METHOD % message.to_
    message.content_type = SLPMessage.CONTENT_SESSION_REQ
    message.body['EUF-GUID'] = eufguid
    message.body['SessionID'] = session.session_id
    message.body['AppID'] = session.app_id
    message.body['Context'] = context
    return message
def ok(session):
    message = SLPMessage(session=session)
    message.method = SLPMessage.OK_STATUS
    message.content_type = SLPMessage.CONTENT_SESSION_REQ
    message.cseq = '1'
    message.body['SessionID'] = session.session_id
    return message
def webcam_invite(session):
    message = SLPMessage(session=session)
    message.method = SLPMessage.INVITE_METHOD % message.to_
    message.content_type = SLPMessage.CONTENT_TRANS_REQ
    message.cseq = '0'
    message.body['Bridges'] = 'TRUDPv1 TCPv1'
    message.body['NetID'] = '-1280904111'
    message.body['Conn-Type'] = 'Firewall'
    message.body['UPnPNat'] = 'false'
    message.body['ICF'] = 'false'
    return message
def decline(session):
    message = SLPMessage(session=session)
    message.method = SLPMessage.DECLINE_STATUS
    message.content_type = SLPMessage.CONTENT_SESSION_CLOSE
    message.cseq = '1'
    message.body['SessionID'] = session.session_id
    return message
def bye(session):
    message = SLPMessage(session=session)
    message.method = SLPMessage.BYE_METHOD % message.to_
    message.content_type = SLPMessage.CONTENT_SESSION_CLOSE
    return message
