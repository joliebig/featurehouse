"""
*Introduction*
Dibbler is a Python web application framework.  It lets you create web-based
applications by writing independent plug-in modules that don't require any
networking code.  Dibbler takes care of the HTTP side of things, leaving you
to write the application code.
*Plugins and Methlets*
Dibbler uses a system of plugins to implement the application logic.  Each
page maps to a 'methlet', which is a method of a plugin object that serves
that page, and is named after the page it serves.  The address
`http://server/spam` calls the methlet `onSpam`.  `onHome` is a reserved
methlet name for the home page, `http://server/`.  For resources that need a
file extension (eg. images) you can use a URL such as `http://server/eggs.gif`
to map to the `onEggsGif` methlet.  All the registered plugins are searched
for the appropriate methlet, so you can combine multiple plugins to build
your application.
A methlet needs to call `self.writeOKHeaders('text/html')` followed by
`self.write(content)`.  You can pass whatever content-type you like to
`writeOKHeaders`, so serving images, PDFs, etc. is no problem.  If a methlet
wants to return an HTTP error code, it should call (for example)
`self.writeError(403, "Forbidden")` instead of `writeOKHeaders`
and `write`.  If it wants to write its own headers (for instance to return
a redirect) it can simply call `write` with the full HTTP response.
If a methlet raises an exception, it is automatically turned into a "500
Server Error" page with a full traceback in it.
*Parameters*
Methlets can take parameters, the values of which are taken from form
parameters submitted by the browser.  So if your form says
`<form action='subscribe'><input type="text" name="email"/> ...` then your
methlet should look like `def onSubscribe(self, email=None)`.  It's good
practice to give all the parameters default values, in case the user navigates
to that URL without submitting a form, or submits the form without filling in
any parameters.  If you have lots of parameters, or their names are determined
at runtime, you can define your methlet like this:
`def onComplex(self, **params)` to get a dictionary of parameters.
*Example*
Here's a web application server that serves a calendar for a given year:
>>> import Dibbler, calendar
>>> class Calendar(Dibbler.HTTPPlugin):
...     _form = '''<html><body><h3>Calendar Server</h3>
...                <form action='/'>
...                Year: <input type='text' name='year' size='4'>
...                <input type='submit' value='Go'></form>
...                <pre>%s</pre></body></html>'''
...
...     def onHome(self, year=None):
...         if year:
...             result = calendar.calendar(int(year))
...         else:
...             result = ""
...         self.writeOKHeaders('text/html')
...         self.write(self._form % result)
...
>>> httpServer = Dibbler.HTTPServer(8888)
>>> httpServer.register(Calendar())
>>> Dibbler.run(launchBrowser=True)
Your browser will start, and you can ask for a calendar for the year of
your choice.  If you don't want to start the browser automatically, just call
`run()` with no arguments - the application is available at
http://localhost:8888/ .  You'll have to kill the server manually because it
provides no way to stop it; a real application would have some kind of
'shutdown' methlet that called `sys.exit()`.
By combining Dibbler with an HTML manipulation library like
PyMeld (shameless plug - see http://entrian.com/PyMeld for details) you can
keep the HTML and Python code separate.
*Building applications*
You can run several plugins together like this:
>>> httpServer = Dibbler.HTTPServer()
>>> httpServer.register(plugin1, plugin2, plugin3)
>>> Dibbler.run()
...so many plugin objects, each implementing a different set of pages,
can cooperate to implement a web application.  See also the `HTTPServer`
documentation for details of how to run multiple `Dibbler` environments
simultaneously in different threads.
*Controlling connections*
There are times when your code needs to be informed the moment an incoming
connection is received, before any HTTP conversation begins.  For instance,
you might want to only accept connections from `localhost` for security
reasons.  If this is the case, your plugin should implement the
`onIncomingConnection` method.  This will be passed the incoming socket
before any reads or writes have taken place, and should return True to allow
the connection through or False to reject it.  Here's an implementation of
the `localhost`-only idea:
>>> def onIncomingConnection(self, clientSocket):
>>>     return clientSocket.getpeername()[0] == clientSocket.getsockname()[0]
*Advanced usage: Dibbler Contexts*
If you want to run several independent Dibbler environments (in different
threads for example) then each should use its own `Context`.  Normally
you'd say something like:
>>> httpServer = Dibbler.HTTPServer()
>>> httpServer.register(MyPlugin())
>>> Dibbler.run()
but that's only safe to do from one thread.  Instead, you can say:
>>> myContext = Dibbler.Context()
>>> httpServer = Dibbler.HTTPServer(context=myContext)
>>> httpServer.register(MyPlugin())
>>> Dibbler.run(myContext)
in as many threads as you like.
*Dibbler and asyncore*
If this section means nothing to you, you can safely ignore it.
Dibbler is built on top of Python's asyncore library, which means that it
integrates into other asyncore-based applications, and you can write other
asyncore-based components and run them as part of the same application.
By default, Dibbler uses the default asyncore socket map.  This means that
`Dibbler.run()` also runs your asyncore-based components, provided they're
using the default socket map.  If you want to tell Dibbler to use a
different socket map, either to co-exist with other asyncore-based components
using that map or to insulate Dibbler from such components by using a
different map, you need to use a `Dibbler.Context`.  If you're using your own
socket map, give it to the context: `context = Dibbler.Context(myMap)`.  If
you want Dibbler to use its own map: `context = Dibbler.Context({})`.
You can either call `Dibbler.run(context)` to run the async loop, or call
`asyncore.loop()` directly - the only difference is that the former has a
few more options, like launching the web browser automatically.
*Self-test*
Running `Dibbler.py` directly as a script runs the example calendar server
plus a self-test.
"""

__author__ = "Richie Hindle <richie@entrian.com>"

__credits__ = "Tim Stone"

try:

    import io as StringIO

except ImportError:

    import io



import sys, re, time, traceback, base64

import socket, asyncore, asynchat, cgi, urllib.parse, webbrowser

try:

    "".rstrip("abc")

except TypeError:

    RSTRIP_CHARS_AVAILABLE = False

else:

    RSTRIP_CHARS_AVAILABLE = True



from spambayes.port import md5

class  BrighterAsyncChat (asynchat.async_chat) :
	"""An asynchat.async_chat that doesn't give spurious warnings on
    receiving an incoming connection, lets SystemExit cause an exit, can
    flush its output, and will correctly remove itself from a non-default
    socket map on `close()`."""
	    def __init__(self, conn=None, map=None):

        """See `asynchat.async_chat`."""

        asynchat.async_chat.__init__(self, conn)

        self.__map = map

        self._closed = False
 def handle_connect(self):

        """Suppresses the asyncore "unhandled connect event" warning."""

        pass
 def handle_error(self):

        """Let SystemExit cause an exit."""

        type, v, t = sys.exc_info()

        if type == SystemExit:

            raise

        else:

            asynchat.async_chat.handle_error(self)
 def flush(self):

        """Flush everything in the output buffer."""

        while (self.producer_fifo or self.ac_out_buffer) and not self._closed:

            self.initiate_send()
 def close(self):

        """Remove this object from the correct socket map."""

        self._closed = True

        self.del_channel(self.__map)

        self.socket.close()

class  Context :
	"""See the main documentation for details of `Dibbler.Context`."""
	    def __init__(self, asyncMap=asyncore.socket_map):

        self._HTTPPort = None  

        self._map = asyncMap
 def pop(self, key):

        return self._map.pop(key)
 def keys(self):

        return list(self._map.keys())
 def __len__(self):

        return len(self._map)

_defaultContext = Context() class  Listener (asyncore.dispatcher) :
	"""Generic listener class used by all the different types of server.
    Listens for incoming socket connections and calls a factory function
    to create handlers for them."""
	    def __init__(self, port, factory, factoryArgs,
                 socketMap=_defaultContext._map):

        """Creates a listener object, which will listen for incoming
        connections when Dibbler.run is called:
         o port: The TCP/IP (address, port) to listen on. Usually '' -
           meaning bind to all IP addresses that the machine has - will be
           passed as the address.  If `port` is just an int, an address of
           '' will be assumed.
         o factory: The function to call to create a handler (can be a class
           name).
         o factoryArgs: The arguments to pass to the handler factory.  For
           proper context support, this should include a `context` argument
           (or a `socketMap` argument for pure asyncore listeners).  The
           incoming socket will be prepended to this list, and passed as the
           first argument.  See `HTTPServer` for an example.
         o socketMap: Optional.  The asyncore socket map to use.  If you're
           using a `Dibbler.Context`, pass context._map.
        See `HTTPServer` for an example `Listener` - it's a good deal smaller
        than this description!"""

        asyncore.dispatcher.__init__(self, map=socketMap)

        self.socketMap = socketMap

        self.factory = factory

        self.factoryArgs = factoryArgs

        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        s.setblocking(False)

        self.set_socket(s, self.socketMap)

        self.set_reuse_addr()

        if type(port) != type(()):

            port = ('', port)

        try:

            self.bind(port)

        except socket.error:

            print("port", port, "in use", file=sys.stderr)

            raise

        self.listen(5)
 def handle_accept(self):

        """Asyncore override."""

        result = self.accept()

        if result:

            clientSocket, clientAddress = result

            args = [clientSocket] + list(self.factoryArgs)

            self.factory(*args)

class  HTTPServer (Listener) :
	"""A web server with which you can register `HTTPPlugin`s to serve up
    your content - see `HTTPPlugin` for detailed documentation and examples.
    `port` specifies the TCP/IP (address, port) on which to run, defaulting
    to ('', 80).
    `context` optionally specifies a `Dibbler.Context` for the server.
    """
	    NO_AUTHENTICATION     = "None"
	    BASIC_AUTHENTICATION  = "Basic"
	    DIGEST_AUTHENTICATION = "Digest"
	    def __init__(self, port=('', 80), context=_defaultContext):

        """Create an `HTTPServer` for the given port."""

        Listener.__init__(self, port, _HTTPHandler,
                          (self, context), context._map)

        self._plugins = []

        try:

            context._HTTPPort = port[1]

        except TypeError:

            context._HTTPPort = port
 def register(self, *plugins):

        """Registers one or more `HTTPPlugin`-derived objects with the
        server."""

        for plugin in plugins:

            self._plugins.append(plugin)
 def requestAuthenticationMode(self):

        """Override: HTTP Authentication. It should return a value among
        NO_AUTHENTICATION, BASIC_AUTHENTICATION and DIGEST_AUTHENTICATION.
        The two last values will force HTTP authentication respectively
        through Base64 and MD5 encodings."""

        return self.NO_AUTHENTICATION
 def isValidUser(self, name, password):

        """Override: Return True for authorized logins."""

        return True
 def getPasswordForUser(self, name):

        """Override: Return the password associated to the specified user
        name."""

        return ''
 def getRealm(self):

        """Override: Specify the HTTP authentication realm."""

        return "Dibbler application server"
 def getCancelMessage(self):

        """Override: Specify the cancel message for an HTTP Authentication."""

        return "You must log in."

class  _HTTPHandler (BrighterAsyncChat) :
	"""This is a helper for the HTTP server class - one of these is created
    for each incoming request, and does the job of decoding the HTTP traffic
    and driving the plugins."""
	    _login_splitter = re.compile('([a-zA-Z]+)=(".*?"|.*?),?')
	    def __init__(self, clientSocket, server, context):

        BrighterAsyncChat.__init__(self, map=context._map)

        BrighterAsyncChat.set_socket(self, clientSocket, context._map)

        self._context = context

        self._server = server

        self._request = ''

        self.set_terminator('\r\n\r\n')

        self._bufferedHeaders = []

        self._headersWritten = False

        for plugin in self._server._plugins:

            if not plugin.onIncomingConnection(clientSocket):

                self.close()
 def collect_incoming_data(self, data):

        """Asynchat override."""

        self._request = self._request + data
 def found_terminator(self):

        """Asynchat override."""

        requestLine, headers = (self._request+'\r\n').split('\r\n', 1)

        try:

            method, url, version = requestLine.strip().split()

        except ValueError:

            self.writeError(400, "Malformed request: '%s'" % requestLine)

            self.close_when_done()

            return

        method = method.upper()

        unused, unused, path, unused, query, unused = urllib.parse.urlparse(url)

        cgiParams = cgi.parse_qs(query, keep_blank_values=True)

        if self.get_terminator() == '\r\n\r\n' and method == 'POST':

            match = re.search(r'(?i)content-length:\s*(\d+)', headers)

            contentLength = int(match.group(1))

            if contentLength > 0:

                self.set_terminator(contentLength)

                self._request = self._request + '\r\n\r\n'

                return

        if type(self.get_terminator()) is type(1):

            self.set_terminator('\r\n\r\n')

            body = self._request.split('\r\n\r\n', 1)[1]

            match = re.search(r'(?i)content-type:\s*([^\r\n]+)', headers)

            contentTypeHeader = match.group(1)

            contentType, pdict = cgi.parse_header(contentTypeHeader)

            if contentType == 'multipart/form-data':

                bodyFile = io.StringIO(body)

                cgiParams.update(cgi.parse_multipart(bodyFile, pdict))

            else:

                cgiParams.update(cgi.parse_qs(body, keep_blank_values=True))

        params = {}

        for name, value in cgiParams.items():

            params[name] = value[0]

        headersRegex = re.compile('([^:]*):\s*(.*)')

        headersDict = dict([headersRegex.match(line).groups(2)
                           for line in headers.split('\r\n')
                           if headersRegex.match(line)])

        serverAuthMode = self._server.requestAuthenticationMode()

        if serverAuthMode != HTTPServer.NO_AUTHENTICATION:

            authResult = False

            authHeader = headersDict.get('Authorization')

            if authHeader:

                authMatch = re.search('(\w+)\s+(.*)', authHeader)

                authenticationMode, login = authMatch.groups()

                if authenticationMode == HTTPServer.BASIC_AUTHENTICATION:

                    authResult = self._basicAuthentication(login)

                elif authenticationMode == HTTPServer.DIGEST_AUTHENTICATION:

                    authResult = self._digestAuthentication(login, method)

                else:

                    print("Unknown mode: %s" % authenticationMode, file=sys.stderr)

            if not authResult:

                self.writeUnauthorizedAccess(serverAuthMode)

        if path == '/':

            path = '/Home'

        pieces = path[1:].split('.')

        name = 'on' + ''.join([piece.capitalize() for piece in pieces])

        for plugin in self._server._plugins:

            if hasattr(plugin, name):

                plugin._handler = self

                try:

                    getattr(plugin, name)(**params)

                    if self._bufferedHeaders:

                        self.write(None)

                except:

                    eType, eValue, eTrace = sys.exc_info()

                    if eType == SystemExit:

                        contextMap = self._context._map

                        for dispatcher in list(contextMap.values()):

                            if isinstance(dispatcher, Listener):

                                dispatcher.close()

                        def isProtected(dispatcher):

                            return not isinstance(dispatcher, _HTTPHandler)

                        while len(list(filter(isProtected, list(contextMap.values())))) > 0:

                            asyncore.poll(timeout=1, map=contextMap)

                        raise SystemExit

                    message = """<h3>500 Server error</h3><pre>%s</pre>"""

                    details = traceback.format_exception(eType, eValue, eTrace)

                    details = '\n'.join(details)

                    self.writeError(500, message % cgi.escape(details))

                plugin._handler = None

                break

        else:

            self.onUnknown(path, params)

        self.close_when_done()
 def onUnknown(self, path, params):

        """Handler for unknown URLs.  Returns a 404 page."""

        self.writeError(404, "Not found: '%s'" % path)
 def writeOKHeaders(self, contentType, extraHeaders={}):

        """Reflected from `HTTPPlugin`s."""

        timeNow = time.gmtime(time.time())

        httpNow = time.strftime('%a, %d %b %Y %H:%M:%S GMT', timeNow)

        headers = []

        headers.append("HTTP/1.1 200 OK")

        headers.append("Connection: close")

        headers.append('Content-Type: %s; charset="utf-8"' % contentType)

        headers.append("Date: %s" % httpNow)

        for name, value in list(extraHeaders.items()):

            headers.append("%s: %s" % (name, value))

        headers.append("")

        headers.append("")

        self._bufferedHeaders = headers
 def writeError(self, code, message):

        """Reflected from `HTTPPlugin`s."""

        headers = []

        if not self._headersWritten:

            headers.append("HTTP/1.0 %d Error" % code)

            headers.append("Connection: close")

            headers.append('Content-Type: text/html; charset="utf-8"')

            headers.append("")

            headers.append("")

        self.push("%s<html><body>%s</body></html>" % \
                  ('\r\n'.join(headers), message))
 def write(self, content):

        """Reflected from `HTTPPlugin`s."""

        headers = []

        if self._bufferedHeaders:

            headers = self._bufferedHeaders

            self._bufferedHeaders = None

            self._headersWritten = True

        if content is None:

            content = ''

        self.push('\r\n'.join(headers) + str(content))
 def writeUnauthorizedAccess(self, authenticationMode):

        """Access is protected by HTTP authentication."""

        if authenticationMode == HTTPServer.BASIC_AUTHENTICATION:

            authString = self._getBasicAuthString()

        elif authenticationMode == HTTPServer.DIGEST_AUTHENTICATION:

            authString = self._getDigestAuthString()

        else:

            self.writeError(500, "Inconsistent authentication mode.")

            return

        headers = []

        headers.append('HTTP/1.0 401 Unauthorized')

        headers.append('WWW-Authenticate: ' + authString)

        headers.append('Connection: close')

        headers.append('Content-Type: text/html; charset="utf-8"')

        headers.append('')

        headers.append('')

        self.write('\r\n'.join(headers) + self._server.getCancelMessage())

        self.close_when_done()
 def _getDigestAuthString(self):

        """Builds the WWW-Authenticate header for Digest authentication."""

        authString  = 'Digest realm="' + self._server.getRealm() + '"'

        authString += ', nonce="' + self._getCurrentNonce() + '"'

        authString += ', opaque="0000000000000000"'

        authString += ', stale="false"'

        authString += ', algorithm="MD5"'

        authString += ', qop="auth"'

        return authString
 def _getBasicAuthString(self):

        """Builds the WWW-Authenticate header for Basic authentication."""

        return 'Basic realm="' + self._server.getRealm() + '"'
 def _getCurrentNonce(self):

        """Returns the current nonce value. This value is a Base64 encoding
        of current time plus 20 minutes. This means the nonce will expire 20
        minutes from now."""

        timeString = time.asctime(time.localtime(time.time() + 20*60))

        if RSTRIP_CHARS_AVAILABLE:

            return base64.encodestring(timeString).rstrip('\n=')

        else:

            def rstrip(s, chars):

                if not s:

                    return s

                if s[-1] in chars:

                    return rstrip(s[:-1])

                return s

            return rstrip(base64.encodestring(timeString), '\n=')
 def _isValidNonce(self, nonce):

        """Check if the specified nonce is still valid. A nonce is invalid
        when its time converted value is lower than current time."""

        padAmount = len(nonce) % 4

        if padAmount > 0:

            padAmount = 4 - padAmount

        nonce += '=' * (len(nonce) + padAmount)

        decoded = base64.decodestring(nonce)

        return time.time() < time.mktime(time.strptime(decoded))
 def _basicAuthentication(self, login):

        """Performs a Basic HTTP authentication. Returns True when the user
        has logged in successfully, False otherwise."""

        userName, password = base64.decodestring(login).split(':')

        return self._server.isValidUser(userName, password)
 def _digestAuthentication(self, login, method):

        """Performs a Digest HTTP authentication. Returns True when the user
        has logged in successfully, False otherwise."""

        def stripQuotes(s):

            return (s[0] == '"' and s[-1] == '"') and s[1:-1] or s

        options  = dict(self._login_splitter.findall(login))

        userName = stripQuotes(options["username"])

        password = self._server.getPasswordForUser(userName)

        nonce    = stripQuotes(options["nonce"])

        A1  = "%s:%s:%s" % (userName, self._server.getRealm(), password)

        HA1 = md5(A1).hexdigest()

        A2  = "%s:%s" % (method, stripQuotes(options["uri"]))

        HA2 = md5(A2).hexdigest()

        unhashedDigest = ""

        if "qop" in options:

            if not options["nc"]:

                options["nc"] = "00000001"

            if not options["qop"]:

                options["qop"] = "auth"

            unhashedDigest = "%s:%s:%s:%s:%s:%s" % \
                            (HA1, nonce,
                             stripQuotes(options["nc"]),
                             stripQuotes(options["cnonce"]),
                             stripQuotes(options["qop"]), HA2)

        else:

            unhashedDigest = "%s:%s:%s" % (HA1, nonce, HA2)

        hashedDigest = md5(unhashedDigest).hexdigest()

        return (stripQuotes(options["response"]) == hashedDigest and
                self._isValidNonce(nonce))

class  HTTPPlugin :
	"""Base class for HTTP server plugins.  See the main documentation for
    details."""
	    def __init__(self):

        pass
 def onIncomingConnection(self, clientSocket):

        """Implement this and return False to veto incoming connections."""

        return True
 def writeOKHeaders(self, contentType, extraHeaders={}):

        """A methlet should call this with the Content-Type and optionally
        a dictionary of extra headers (eg. Expires) before calling
        `write()`."""

        return self._handler.writeOKHeaders(contentType, extraHeaders)
 def writeError(self, code, message):

        """A methlet should call this instead of `writeOKHeaders()` /
        `write()` to report an HTTP error (eg. 403 Forbidden)."""

        return self._handler.writeError(code, message)
 def write(self, content):

        """A methlet should call this after `writeOKHeaders` to write the
        page's content."""

        return self._handler.write(content)
 def flush(self):

        """A methlet can call this after calling `write`, to ensure that
        the content is written immediately to the browser.  This isn't
        necessary most of the time, but if you're writing "Please wait..."
        before performing a long operation, calling `flush()` is a good
        idea."""

        return self._handler.flush()
 def close(self, flush=True):

        """Closes the connection to the browser.  You should call `close()`
        before calling `sys.exit()` in any 'shutdown' methlets you write."""

        if flush:

            self.flush()

        return self._handler.close()

def run(launchBrowser=False, context=_defaultContext):

    """Runs a `Dibbler` application.  Servers listen for incoming connections
    and route requests through to plugins until a plugin calls `sys.exit()`
    or raises a `SystemExit` exception."""

    if launchBrowser:

        try:

            url = "http://localhost:%d/" % context._HTTPPort

            webbrowser.open_new(url)

        except webbrowser.Error as e:

            print("\n%s.\nPlease point your web browser at %s." % (e, url))

    asyncore.loop(map=context._map)
 def runTestServer(readyEvent=None):

    """Runs the calendar server example, with an added `/shutdown` URL."""

    import calendar

    class Calendar(HTTPPlugin):

        _form = '''<html><body><h3>Calendar Server</h3>
                   <form action='/'>
                   Year: <input type='text' name='year' size='4'>
                   <input type='submit' value='Go'></form>
                   <pre>%s</pre></body></html>'''

        def onHome(self, year=None):

            if year:

                result = calendar.calendar(int(year))

            else:

                result = ""

            self.writeOKHeaders('text/html')

            self.write(self._form % result)

        def onShutdown(self):

            self.writeOKHeaders('text/html')

            self.write("<html><body><p>OK.</p></body></html>")

            self.close()

            sys.exit()

    httpServer = HTTPServer(8888)

    httpServer.register(Calendar())

    if readyEvent:

        readyEvent.set()

    run(launchBrowser=True)
 def test():

    """Run a self-test."""

    import threading, urllib.request, urllib.parse, urllib.error

    testServerReady = threading.Event()

    threading.Thread(target=runTestServer, args=(testServerReady,)).start()

    testServerReady.wait()

    page = urllib.request.urlopen("http://localhost:8888/?year=2003").read()

    if page.find('January') != -1:

        print("Self test passed.")

    else:

        print("Self-test failed!")

    input("Press any key to shut down the application server...")

    page = urllib.request.urlopen("http://localhost:8888/shutdown").read()

    if page.find('OK') != -1:

        print("Shutdown OK.")

    else:

        print("Shutdown failed!")
 if __name__ == '__main__':

    test()

 if __name__ == '__main__':

    test()





try:

    import cStringIO as StringIO

except ImportError:

    import StringIO



try:

    "".rstrip("abc")

except TypeError:

    RSTRIP_CHARS_AVAILABLE = False

else:

    RSTRIP_CHARS_AVAILABLE = True



