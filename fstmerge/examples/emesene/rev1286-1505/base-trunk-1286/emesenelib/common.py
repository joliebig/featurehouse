import sys
import xml.dom.minidom
import xml.sax.saxutils
status_table = {
    'online':       'NLN',
    'away':         'AWY',
    'busy':         'BSY',
    'brb':          'BRB',
    'phone':        'PHN',
    'lunch':        'LUN',
    'invisible':    'HDN',
    'idle':         'IDL',
    'offline':      'FLN',
}
reverse_status = {
    'NLN':        'online',
    'AWY':        'away',
    'BSY':        'busy',
    'BRB':        'brb',
    'PHN':        'phone',
    'LUN':        'lunch',
    'HDN':        'invisible',
    'IDL':        'idle',
    'FLN':        'offline',
}
dic = {
    '\"'    :    '&quot;',
    '\''    :    '&apos;'
}
dic_inv = {
    '&quot;'    :'\"',
    '&apos;'    :'\''
}
debugFlag = False
binaryFlag = False
disabled_sections = []
def debug(data, section='default'):
    '''print debug data to the console if the debug flag is set
    and section not in disabled_sections
    '''
    if debugFlag and section not in disabled_sections:
        sys.stdout.write('\r')
        if not binaryFlag:
            data = ''.join([x for x in list(str(data)) \
                if (ord(x) < 127 and ord(x) > 31) or x in '\r\n\t'])
        sys.stdout.write(str(data).__repr__()[1:-1].replace( '\\r\\n', '\n' ))
        if not str( data ).endswith( '\n' ):
            sys.stdout.write( '\n' )
def escape(string):
    return xml.sax.saxutils.escape(string, dic)
def unescape(string):
    return xml.sax.saxutils.unescape(string, dic_inv)
def dumpXmlToFile(string, file_name):
    dom = xml.dom.minidom.parseString(string)
    f = file(file_name, 'w')
    f.write(dom.toprettyxml())
    f.close()
def parseSoapFault( response ):
    '''parse the xml response to extract the fault'''
    body = response.body
    try:
        ml = xml.dom.minidom.parseString( body )
        fault = ml.getElementsByTagName( 'soap:Fault' )[ 0 ]
    except:
        return 'unknown'
    try:
        return fault.getElementsByTagName( 'errorstring' )[ 0 ].childNodes[ 0 ].data
    except IndexError:
        return fault.getElementsByTagName( 'faultstring' )[ 0 ].childNodes[ 0 ].data
class LoginError( Exception ):
    def __init__( self, value ):
        self.value = value
    def __str__( self ):
        return "Couldn't perform login: " + repr( self.value )
class AuthError( Exception ):
    def __init__( self, value ):
        self.value = value
    def __str__( self ):
        return 'Authentication error: ' + repr( self.value )
class RedirectionError( Exception ):
    def __init__( self, value ):
        self.value = value
    def __str__( self ):
        return 'Redirection error: ' + repr( self.value )
class ProtocolError( Exception ):
    def __init__( self, value ):
        self.value = value
    def __str__( self ):
        return 'Protocol error: ' + repr( self.value )
class SoapError( Exception ):
    def __init__( self, value, response ):
        self.value = value
        self.responseStatus = response.status
        body = response.body
        ml = xml.dom.minidom.parseString( body )
        fault = ml.getElementsByTagName( 'soap:Fault' )[ 0 ]
        try:
            self.faultcode = fault.getElementsByTagName( 'errorstring' )[ 0 ].childNodes[ 0 ].data
        except IndexError:
            self.faultcode = fault.getElementsByTagName( 'faultstring' )[ 0 ].childNodes[ 0 ].data
    def __str__( self ):
        return repr( self.value ) + ' ' + repr( self.responseStatus ) \
                    + ': ' + repr( self.faultcode )
class DynamicDict(dict):
    '''A dict that creates items on the fly'''
    def __init__(self, constructor, case_insensitive=False):
        self._items = {}
        self.constructor = constructor
        self.case_insensitive = case_insensitive
    def __repr__(self):
        return '<DynamicDict of ' + str(len(self._items)) + ' items>'
    def __getitem__(self, key):
        '''Returns "key" from this dict, if it doesn't exists, it's created'''
        if self.case_insensitive:
            key = key.lower()
        if key not in self._items:
            new = self.constructor(self, key)
            self._items[key] = new
        return self._items[key]
    def __setitem__(self, key, value):
        '''Raises an error when trying to set items'''
        raise TypeError('Read only')
