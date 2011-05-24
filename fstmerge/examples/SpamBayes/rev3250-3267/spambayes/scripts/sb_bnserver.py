"""Usage: %(program)s [options] FILE
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
    -a seconds
        timeout in seconds between requests before this server terminates
    -A number
        terminate this server after this many requests
    FILE
        unix domain socket used on which we listen    
"""

import os, getopt, sys, socketserver, traceback, select, socket, errno

program = sys.argv[0]

def usage(code, msg=''):

    """Print usage message and sys.exit(code)."""

    if msg:

        print(msg, file=sys.stderr)

        print(file=sys.stderr)

    print(__doc__, file=sys.stderr)

    sys.exit(code)
 def main():

    """Main program; parse options and go."""

    try:

        opts, args = getopt.getopt(sys.argv[1:], 'hd:p:o:a:A:')

    except getopt.error as msg:

        usage(2, msg)

    if len(args) != 1:

        usage(2, "socket not specified")

    try:

        server = BNServer(args[0], BNRequest)

    except socket.error as e:

        if e[0] == errno.EADDRINUSE:

            pass   

        else:

            raise  

    else:

        try:

            from spambayes import Options, storage

            options = Options.options

            for opt, arg in opts:

                if opt == '-h':

                    usage(0)

                elif opt == '-o':

                    options.set_from_cmdline(arg, sys.stderr)

                elif opt == '-a':

                    server.timeout = float(arg)

                elif opt == '-A':

                    server.number = int(arg)

            h = make_HammieFilter()

            h.dbname, h.usedb = storage.database_type(opts)

            server.hammie = h

            server.serve_until_idle()

            h.close()

        finally:

            try:

                os.unlink(args[0])

            except EnvironmentError:

                pass
 class  NowIdle (Exception) :
	pass
class  BNServer (SocketServer.UnixStreamServer) :
	allow_reuse_address = True
	    timeout = 10.0
	    number = 100
	    def serve_until_idle(self):

        try:

            for i in range(self.number):

                self.handle_request()

        except NowIdle:

            pass
 def get_request(self):

        r, w, e = select.select([self.socket], [], [], self.timeout)

        if r:

            return self.socket.accept()

        else:

            raise NowIdle()

class  BNRequest (SocketServer.StreamRequestHandler) :
	def handle(self):

        switches = self.rfile.readline()

        body = self.rfile.read()

        try:

            response = self._calc_response(switches, body)

            self.wfile.write('0\n%d\n'%(len(response),))

            self.wfile.write(response)

        except:

            response = traceback.format_exception_only(sys.exc_info()[0],
                                                       sys.exc_info()[1])[0]

            self.wfile.write('1\n%d\n'%(len(response),))

            self.wfile.write(response)
 def _calc_response(self, switches, body):

        switches = switches.split()

        actions = []

        opts, args = getopt.getopt(switches, 'fgstGS')

        h = self.server.hammie

        for opt, arg in opts:

            if opt == '-f':

                actions.append(h.filter)

            elif opt == '-g':

                actions.append(h.train_ham)

            elif opt == '-s':

                actions.append(h.train_spam)

            elif opt == '-t':

                actions.append(h.filter_train)

            elif opt == '-G':

                actions.append(h.untrain_ham)

            elif opt == '-S':

                actions.append(h.untrain_spam)

        if actions == []:

            actions = [h.filter]

        from spambayes import mboxutils

        msg = mboxutils.get_message(body)

        for action in actions:

            action(msg)

        return mboxutils.as_string(msg, 1)

def make_HammieFilter():

    from spambayes import Options

    path = os.path.split(Options.__file__)[0]+'/../scripts'

    if path not in sys.path:

        sys.path.append(path)

    from sb_filter import HammieFilter

    return HammieFilter()
 if __name__ == "__main__":

    main()

 if __name__ == "__main__":

    main()





