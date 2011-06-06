"""Usage: %(program)s [options] IP:PORT
Where:
    -h
        show usage and exit
    -p FILE
        use pickle FILE as the persistent store.  loads data from this file
        if it exists, and saves data to this file at the end.
    -d FILE
        use DBM store FILE as the persistent store.
    -o section:option:value
        set [section, option] in the options database to value
    IP
        IP address to bind (use 0.0.0.0 to listen on all IPs of this machine)
    PORT
        Port number to listen to.
"""
import getopt
import sys
import xmlrpclib
import SimpleXMLRPCServer
from spambayes import hammie, Options
from spambayes import storage
class ReusableSimpleXMLRPCServer(SimpleXMLRPCServer.SimpleXMLRPCServer):
    allow_reuse_address = True
program = sys.argv[0] # For usage(); referenced by docstring above
class XMLHammie(hammie.Hammie):
    def score(self, msg, *extra):
        try:
            msg = msg.data
        except AttributeError:
            pass
        return hammie.Hammie.score(self, msg, *extra)
    def filter(self, msg, *extra):
        try:
            msg = msg.data
        except AttributeError:
            pass
        return xmlrpclib.Binary(hammie.Hammie.filter(self, msg, *extra))
def usage(code, msg=''):
    """Print usage message and sys.exit(code)."""
    if msg:
        print >> sys.stderr, msg
        print >> sys.stderr
    print >> sys.stderr, __doc__
    sys.exit(code)
def main():
    """Main program; parse options and go."""
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hd:p:o:')
    except getopt.error, msg:
        usage(2, msg)
    options = Options.options
    for opt, arg in opts:
        if opt == '-h':
            usage(0)
        elif opt == '-o':
            options.set_from_cmdline(arg, sys.stderr)
    dbname, usedb = storage.database_type(opts)
    if len(args) != 1:
        usage(2, "IP:PORT not specified")
    ip, port = args[0].split(":")
    port = int(port)
    bayes = storage.open_storage(dbname, usedb)
    h = XMLHammie(bayes)
    server = ReusableSimpleXMLRPCServer(
        (ip, port),
        SimpleXMLRPCServer.SimpleXMLRPCRequestHandler)
    server.register_instance(h)
    server.serve_forever()
if __name__ == "__main__":
    main()
