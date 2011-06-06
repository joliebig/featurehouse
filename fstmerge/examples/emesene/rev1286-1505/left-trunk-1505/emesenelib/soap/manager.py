import emesenelib.common as common
import sys
import Queue
import threading
import httplib
requestQueue = Queue.Queue( 0 )
responseQueue = Queue.Queue( 0 )
def put(request):
    global requestQueue
    if isinstance(request, SoapRequest):
        common.debug("soap.manager.put(%s)" % request, "soap")
    requestQueue.put(request)
def get():
    global responseQueue
    retval = responseQueue.get()
    if isinstance(retval, SoapRequest):
        common.debug("soap.manager.get() --> %s" % retval, "soap")
    return retval
def getNoBlock():
    global responseQueue
    return responseQueue.get(True, 0.01)
def process():
    '''run the callbacks if some response is in responseQueue'''
    global responseQueue
    while True:
        try:
            response = getNoBlock()
        except Queue.Empty:
            break
        common.debug("soap.manager.process(): %s" % response, "soap")
        if response.callback:
            if hasattr(response.callback, '__name__'):
                common.debug("soap.manager.process(): Calling %s()" % \
                    response.callback.__name__, "soap")
            response.callback(response, *response.args)
    return True
def do_request(action, host, port, path, body, callback=None, args=(),
               sync=False):
    '''Adds a new request to the queue and optionally waits for response'''
    put(SoapRequest(action, host, port, path, body,
        callback=callback, args=args))
    if sync:
        response = get()
        if response and response.callback:
            response.callback(response, *response.args)
def repeat_request(request, body):
    '''Repeats a request with a different body'''
    put(SoapRequest(request.action, request.host, request.port, request.path,
        body, request.callback))
class SoapException(Exception):
    pass
class SoapRequest(object):
    '''This class represents a soapRequest'''
    def __init__(self, action, host, port, path, body, callback=None, args=()):
        '''Constructor'''
        self.action = action
        self.host = host
        self.port = port
        self.path = path
        self.body = body
        self.callback = callback
        self.args = args
        self.extraCookie = ''
        self.errorCount = 0
        self.status = (0, '')
    def __repr__(self):
        action = self.action.split("/")[-1]
        if self.status != (0, ''):
            return '<SoapRequest %s %s>' % (action, self.status)
        else:
            return '<SoapRequest %s>' % action
class SoapManager( threading.Thread ):
    '''This class is a thread that wait for soapRequest in the requestQueue
    and make the request, then add the response to the responseQueue'''
    def __init__( self, msn ):
        threading.Thread.__init__( self )
        self.msn = msn
    def process( self ):
        try:
            return process()
        except:
            self.msn.emit('exception', sys.exc_info())
    def destroy( self ):
        global requestQueue, responseQueue
        try:
            while True:
                tmp = responseQueue.get(True, 0.01)
                for i in dir(tmp):
                    if not i.startswith('_'):
                        setattr(tmp, i, None)
        except Queue.Empty:
            pass
        try:
            while True:
                tmp = requestQueue.get(True, 0.01)
                for i in dir(tmp):
                    if not i.startswith('_'):
                        setattr(tmp, i, None)
        except Queue.Empty:
            pass
        put('quit')
        self.msn = None
    def run( self ):
        global requestQueue, responseQueue
        req = None
        while True:
            if not req:
                req = requestQueue.get()
            if req == 'quit' or self.msn == None:
                break
            else:
                try:
                    responseQueue.put(self.makeSoapRequest(req))
                except Exception, e:
                    common.debug('soap.manager run() Error: ' + str(e), 'soap')
                    if req.errorCount < 2:
                        req.errorCount += 1
                        continue # retry
            req = None
        return False
    def makeSoapRequest(self, soapRequest, retry=True):
        common.debug('soap.manager makeSoapRequest(): %s' % soapRequest, 'soap')
        soapRequest.body = soapRequest.body.replace("&tickettoken;",
                              self.msn.tokens['contacts.msn.com']['security']\
                              .replace('&', '&amp;'))
        headers = {
            "SOAPAction": soapRequest.action,
            "Content-Type": "text/xml; charset=utf-8",
            "Host": soapRequest.host,
            "Content-Length": str(len(soapRequest.body)),
            "User-Agent": "MSN Explorer/9.0 (MSN 8.0; TmstmpExt)",
            "Connection": "Keep-Alive",
            "Cache-Control": "no-cache",
        }
        conn = None
        response = None
        if soapRequest.port == 443:
            conn = httplib.HTTPSConnection(soapRequest.host, soapRequest.port)  
        else:
            conn = httplib.HTTPConnection(soapRequest.host, soapRequest.port)
        conn.request("POST", soapRequest.path, soapRequest.body, headers)
        response = conn.getresponse()
        soapResponse = SoapRequest(soapRequest.action, soapRequest.host,
            soapRequest.port, soapRequest.path, response.read(),
            soapRequest.callback, soapRequest.args)
        soapResponse.status = (response.status, response.reason)
        if soapResponse.body.count('TweenerChallenge') or \
           soapResponse.body.count('LockKeyChallenge'):
            retry = False
        if retry and soapResponse.body.count('AuthenticationFailure') or \
                     soapResponse.body.count('PassportAuthFail'):
            self.msn.passportReAuth()
            soapResponse = self.makeSoapRequest(soapRequest, False)
        return soapResponse
