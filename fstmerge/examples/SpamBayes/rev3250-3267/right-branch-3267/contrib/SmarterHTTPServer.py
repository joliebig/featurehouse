"""Smarter HTTP Server.
This module builds on SimpleHTTPServer, adding 'methlet' invokation by
handling urls with a file extension of .methlet.  In this instance, the
so-called filename actually names a method on the handler, which is invoked
with a single parameter, a dictionary of the url's parsed query string.
This class is intended to be subclassed, with subclasses adding the
appropriate methlet methods for the application being served.
"""
__version__ = "0.6"
__all__ = ["SmarterHTTPRequestHandler"]
import os
import posixpath
import http.server
import http.server
import urllib.request, urllib.parse, urllib.error
import cgi
import mimetypes
import re
try:
    import io as StringIO
except ImportError:
    import io
class SmarterHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    """Smarter HTTP request handler based on SimpleHTTPRequestHandler.
    Adds GET with parameters, which calls a method.
    """
    server_version = "SmarterHTTP/" + __version__
    def send_head(self):
        """Common code for GET and HEAD commands.
        This sends the response code and MIME headers.
        Return value is either a file object (which has to be copied
        to the outputfile by the caller unless the command was HEAD,
        and must be closed by the caller under all circumstances), or
        None, in which case the caller has nothing further to do.
        """
        path, parms = self.translate_path(self.path)
        f = None
        if os.path.isdir(path):
            if hasattr(self, 'homepage'):
                path = 'homepage.methlet'
            else:
                for index in "index.html", "index.htm":
                    index = os.path.join(path, index)
                    if os.path.exists(index):
                        path = index
                        break
                else:
                    return self.list_directory(path)
        ctype = self.guess_type(path)
        if ctype != 'application/method':
            if ctype.startswith('text/'):
                mode = 'r'
            else:
                mode = 'rb'
            try:
                f = open(path, mode)
            except IOError:
                self.send_error(404, "File not found")
                return None
            else:
                self.send_response(200)
                self.send_header("Content-type", ctype)
                self.end_headers()
        else:
            head, tail = os.path.split(path)
            methname = tail.split('.')[0]
            pdict = {}
            if parms:
                pdict = cgi.parse_qs(parms, False)
            if hasattr(self, methname):
                self.send_response(200)
                retstr = getattr(self, methname)(pdict)
                f = io.StringIO(retstr)
                self.send_header("Content-type", 'text/html')
                self.end_headers()
            else:
                self.send_error(404, "File not found")
                return None
        return f
    def translate_path(self, url):
        """Translate a /-separated PATH to the local filename syntax.
        Components that mean special things to the local file system
        (e.g. drive or directory names) are ignored.  (XXX They should
        probably be diagnosed.)
        """
        parmre = re.compile(r'^(.*)[\?](.*)$')
        match = parmre.search(url)
        if match:
            path = match.group(1)
            parms = match.group(2)
        else:
            path = url
            parms = None
        path = posixpath.normpath(urllib.parse.unquote(path))
        words = path.split('/')
        words = [_f for _f in words if _f]
        path = os.getcwd()
        for word in words:
            drive, word = os.path.splitdrive(word)
            head, word = os.path.split(word)
            if word in (os.curdir, os.pardir): continue
            path = os.path.join(path, word)
        return (path, parms)
    def guess_type(self, path):
        """Guess the type of a file.
        Argument is a PATH (a filename).
        Return value is a string of the form type/subtype,
        usable for a MIME Content-type header.
        The default implementation looks the file's extension
        up in the table self.extensions_map, using text/plain
        as a default; however it would be permissible (if
        slow) to look inside the data to make a better guess.
        """
        base, ext = posixpath.splitext(path)
        if ext in self.extensions_map:
            return self.extensions_map[ext]
        ext = ext.lower()
        if ext in self.extensions_map:
            return self.extensions_map[ext]
        else:
            return self.extensions_map['']
    extensions_map = mimetypes.types_map.copy()
    extensions_map.update({
        '': 'application/octet-stream', # Default
        '.py': 'text/plain',
        '.c': 'text/plain',
        '.h': 'text/plain',
        '.methlet': 'application/method',
        })
def test(HandlerClass = SmarterHTTPRequestHandler,
         ServerClass = http.server.HTTPServer):
    http.server.test(HandlerClass, ServerClass)
if __name__ == '__main__':
    test()
