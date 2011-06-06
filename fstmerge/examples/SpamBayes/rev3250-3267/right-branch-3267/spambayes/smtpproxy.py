"""A SMTP proxy to train a Spambayes database.
You point SMTP Proxy at your SMTP server(s) and configure your email
client(s) to send mail through the proxy (i.e. usually this means you use
localhost as the outgoing server).
To setup, enter appropriate values in your Spambayes configuration file in
the "SMTP Proxy" section (in particular: "remote_servers", "listen_ports",
and "use_cached_message").  This configuration can also be carried out via
the web user interface offered by POP3 Proxy and IMAP Filter.
To use, simply forward/bounce mail that you wish to train to the
appropriate address (defaults to spambayes_spam@localhost and
spambayes_ham@localhost).  All other mail is sent normally.
(Note that IMAP Filter and POP3 Proxy users should not execute this script;
launching of SMTP Proxy will be taken care of by those applicatons).
There are two main forms of operation.  With both, mail to two
(user-configurable) email addresses is intercepted by the proxy (and is
*not* sent to the SMTP server) and used as training data for a Spambayes
database.  All other mail is simply relayed to the SMTP server.
If the "use_cached_message" option is False, the proxy uses the message
sent as training data.  This option is suitable for those not using
POP3 Proxy or IMAP Filter, or for those that are confident that their
mailer will forward/bounce messages in an unaltered form.
If the "use_cached_message" option is True, the proxy examines the message
for a unique spambayes identification number.  It then tries to find this
message in the pop3proxy caches and on the imap servers.  It then retrieves
the message from the cache/server and uses *this* as the training data.
This method is suitable for those using POP3 Proxy and/or IMAP Filter, and
avoids any potential problems with the mailer altering messages before
forwarding/bouncing them.
To use, enter the required SMTP server data in your configuration file and
run sb_server.py
"""
__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>"
__credits__ = "Tim Stone, all the Spambayes folk."
todo = """
 o It would be nice if spam/ham could be bulk forwarded to the proxy,
   rather than one by one.  This would require separating the different
   messages and extracting the correct ids.  Simply changing to find
   *all* the ids in a message, rather than stopping after one *might*
   work, but I don't really know.  Richie Hindle suggested something along
   these lines back in September '02.
 o Suggestions?
Testing:
 o Test with as many clients as possible to check that the
   id is correctly extracted from the forwarded/bounced message.
MUA information:
A '*' in the Header column signifies that the smtpproxy can extract
the id from the headers only.  A '*' in the Body column signifies that
the smtpproxy can extract the id from the body of the message, if it
is there.
                                                        Header  Body
*** Windows 2000 MUAs ***
Eudora 5.2 Forward                                         *     *
Eudora 5.2 Redirect                                              *
Netscape Messenger (4.7) Forward (inline)                  *     *
Netscape Messenger (4.7) Forward (quoted) Plain                  *
Netscape Messenger (4.7) Forward (quoted) HTML                   *
Netscape Messenger (4.7) Forward (quoted) Plain & HTML           *
Netscape Messenger (4.7) Forward (attachment) Plain        *     *
Netscape Messenger (4.7) Forward (attachment) HTML         *     *
Netscape Messenger (4.7) Forward (attachment) Plain & HTML *     *
Outlook Express 6 Forward HTML (Base64)                          *
Outlook Express 6 Forward HTML (None)                            *
Outlook Express 6 Forward HTML (QP)                              *
Outlook Express 6 Forward Plain (Base64)                         *
Outlook Express 6 Forward Plain (None)                           *
Outlook Express 6 Forward Plain (QP)                             *
Outlook Express 6 Forward Plain (uuencoded)                      *
http://www.endymion.com/products/mailman Forward                     *
M2 (Opera Mailer 7.01) Forward                                   *
M2 (Opera Mailer 7.01) Redirect                            *     *
The Bat! 1.62i Forward (RFC Headers not visible)                 *
The Bat! 1.62i Forward (RFC Headers visible)               *     *
The Bat! 1.62i Redirect                                          *
The Bat! 1.62i Alternative Forward                         *     *
The Bat! 1.62i Custom Template                             *     *
AllegroMail 2.5.0.2 Forward                                      *
AllegroMail 2.5.0.2 Redirect                                     *
PocoMail 2.6.3 Bounce                                            *
PocoMail 2.6.3 Bounce                                            *
Pegasus Mail 4.02 Forward (all headers option set)         *     *
Pegasus Mail 4.02 Forward (all headers option not set)           *
Calypso 3 Forward                                                *
Calypso 3 Redirect                                         *     *
Becky! 2.05.10 Forward                                           *
Becky! 2.05.10 Redirect                                          *
Becky! 2.05.10 Redirect as attachment                      *     *
Mozilla Mail 1.2.1 Forward (attachment)                    *     *
Mozilla Mail 1.2.1 Forward (inline, plain)                 *1    *
Mozilla Mail 1.2.1 Forward (inline, plain & html)          *1    *
Mozilla Mail 1.2.1 Forward (inline, html)                  *1    *
*1 The header method will only work if auto-include original message
is set, and if view all headers is true.
"""
import re
import socket
import sys
import email
from spambayes import Dibbler
from spambayes import message
from spambayes.tokenizer import textparts
from spambayes.tokenizer import try_to_repair_damaged_base64
from spambayes.Options import options
from sb_server import _addressPortStr, ServerLineReader
from sb_server import _addressAndPort
class SMTPProxyBase(Dibbler.BrighterAsyncChat):
    """An async dispatcher that understands SMTP and proxies to a SMTP
    server, calling `self.onTransaction(command, args)` for each
    transaction.
    self.onTransaction() should return the command to pass to
    the proxied server - the command can be the verbatim command or a
    processed version of it.  The special command 'KILL' kills it (passing
    a 'QUIT' command to the server).
    """
    def __init__(self, clientSocket, serverName, serverPort):
        Dibbler.BrighterAsyncChat.__init__(self, clientSocket)
        self.request = ''
        self.set_terminator('\r\n')
        self.command = ''           # The SMTP command being processed...
        self.args = ''              # ...and its arguments
        self.isClosing = False      # Has the server closed the socket?
        self.inData = False
        self.data = []
        self.blockData = False
        if not self.onIncomingConnection(clientSocket):
            self.push("421 Connection not allowed\r\n")
            self.close_when_done()
            return
        self.serverSocket = ServerLineReader(serverName, serverPort,
                                             self.onServerLine)
    def onIncomingConnection(self, clientSocket):
        """Checks the security settings."""
        remoteIP = clientSocket.getpeername()[0]
        trustedIPs = options["smtpproxy", "allow_remote_connections"]
        if trustedIPs == "*" or remoteIP == clientSocket.getsockname()[0]:
            return True
        trustedIPs = trustedIPs.replace('.', '\.').replace('*', '([01]?\d\d?|2[04]\d|25[0-5])')
        for trusted in trustedIPs.split(','):
            if re.search("^" + trusted + "$", remoteIP):
                return True
        return False
    def onTransaction(self, command, args):
        """Overide this.  Takes the raw command and returns the (possibly
        processed) command to pass to the email client."""
        raise NotImplementedError
    def onProcessData(self, data):
        """Overide this.  Takes the raw data and returns the (possibly
        processed) data to pass back to the email client."""
        raise NotImplementedError
    def onServerLine(self, line):
        """A line of response has been received from the SMTP server."""
        if not line:
            self.isClosing = True
        self.push(line)
        self.onResponse()
    def collect_incoming_data(self, data):
        """Asynchat override."""
        self.request = self.request + data
    def found_terminator(self):
        """Asynchat override."""
        verb = self.request.strip().upper()
        if verb == 'KILL':
            self.socket.shutdown(2)
            self.close()
            raise SystemExit
        if self.request.strip() == '':
            self.command = self.args = ''
        else:
            if self.request[:10].upper() == "MAIL FROM:":
                splitCommand = self.request.split(":", 1)
            elif self.request[:8].upper() == "RCPT TO:":
                splitCommand = self.request.split(":", 1)
            else:
                splitCommand = self.request.strip().split(None, 1)
            self.command = splitCommand[0]
            self.args = splitCommand[1:]
        if self.inData == True:
            self.data.append(self.request + '\r\n')
            if self.request == ".":
                self.inData = False
                cooked = self.onProcessData("".join(self.data))
                self.data = []
                if self.blockData == False:
                    self.serverSocket.push(cooked)
                else:
                    self.push("250 OK\r\n")
        else:
            cooked = self.onTransaction(self.command, self.args)
            if cooked is not None:
                self.serverSocket.push(cooked + '\r\n')
        self.command = self.args = self.request = ''
    def onResponse(self):
        if self.isClosing:
            self.close_when_done()
        self.command = ''
        self.args = ''
        self.isClosing = False
class BayesSMTPProxyListener(Dibbler.Listener):
    """Listens for incoming email client connections and spins off
    BayesSMTPProxy objects to serve them."""
    def __init__(self, serverName, serverPort, proxyPort, trainer):
        proxyArgs = (serverName, serverPort, trainer)
        Dibbler.Listener.__init__(self, proxyPort, BayesSMTPProxy,
                                  proxyArgs)
        print('SMTP Listener on port %s is proxying %s:%d' % \
               (_addressPortStr(proxyPort), serverName, serverPort))
class BayesSMTPProxy(SMTPProxyBase):
    """Proxies between an email client and a SMTP server, inserting
    judgement headers.  It acts on the following SMTP commands:
    o RCPT TO:
        o Checks if the recipient address matches the key ham or spam
          addresses, and if so notes this and does not forward a command to
          the proxied server.  In all other cases simply passes on the
          verbatim command.
     o DATA:
        o Notes that we are in the data section.  If (from the RCPT TO
          information) we are receiving a ham/spam message to train on,
          then do not forward the command on.  Otherwise forward verbatim.
    Any other commands are merely passed on verbatim to the server.
    """
    def __init__(self, clientSocket, serverName, serverPort, trainer):
        SMTPProxyBase.__init__(self, clientSocket, serverName, serverPort)
        self.handlers = {'RCPT TO': self.onRcptTo, 'DATA': self.onData,
                         'MAIL FROM': self.onMailFrom}
        self.trainer = trainer
        self.isClosed = False
        self.train_as_ham = False
        self.train_as_spam = False
    def send(self, data):
        try:
            return SMTPProxyBase.send(self, data)
        except socket.error:
            self.close()
    def close(self):
        if not self.isClosed:
            self.isClosed = True
            SMTPProxyBase.close(self)
    def stripAddress(self, address):
        """
        Strip the leading & trailing <> from an address.  Handy for
        getting FROM: addresses.
        """
        if '<' in address:
            start = address.index('<') + 1
            end = address.index('>')
            return address[start:end]
        else:
            return address
    def onTransaction(self, command, args):
        handler = self.handlers.get(command.upper(), self.onUnknown)
        return handler(command, args)
    def onProcessData(self, data):
        if self.train_as_spam:
            self.trainer.train(data, True)
            self.train_as_spam = False
            return ""
        elif self.train_as_ham:
            self.trainer.train(data, False)
            self.train_as_ham = False
            return ""
        return data
    def onRcptTo(self, command, args):
        toFull = self.stripAddress(args[0])
        if toFull == options["smtpproxy", "spam_address"]:
            self.train_as_spam = True
            self.train_as_ham = False
            self.blockData = True
            self.push("250 OK\r\n")
            return None
        elif toFull == options["smtpproxy", "ham_address"]:
            self.train_as_ham = True
            self.train_as_spam = False
            self.blockData = True
            self.push("250 OK\r\n")
            return None
        else:
            self.blockData = False
        return "%s:%s" % (command, ' '.join(args))
    def onData(self, command, args):
        self.inData = True
        if self.train_as_ham == True or self.train_as_spam == True:
            self.push("354 Enter data ending with a . on a line by itself\r\n")
            return None
        return command + ' ' + ' '.join(args)
    def onMailFrom(self, command, args):
        """Just like the default handler, but has the necessary colon."""
        rv = "%s:%s" % (command, ' '.join(args))
        return rv
    def onUnknown(self, _command, _args):
        """Default handler."""
        return self.request
class SMTPTrainer(object):
    def __init__(self, classifier, state=None, imap=None):
        self.classifier = classifier
        self.state = state
        self.imap = imap
    def extractSpambayesID(self, data):
        msg = email.message_from_string(data, _class=message.SBHeaderMessage)
        id = msg.get(options["Headers", "mailid_header_name"])
        if id is not None:
            return id
        id = self._find_id_in_text(msg.as_string())
        if id is not None:
            return id
        for part in textparts(msg):
            try:
                text = part.get_payload(decode=True)
            except:
                text = part.get_payload(decode=False)
                if text is not None:
                    text = try_to_repair_damaged_base64(text)
            if text is not None:
                id = self._find_id_in_text(text)
                return id
        return None
    header_pattern = re.escape(options["Headers", "mailid_header_name"])
    header_pattern += r":\s*(\</th\>\s*\<td\>\s*)?([\d\-]+)"
    header_re = re.compile(header_pattern)
    def _find_id_in_text(self, text):
        mo = self.header_re.search(text)
        if mo is None:
            return None
        return mo.group(2)
    def train(self, msg, isSpam):
        try:
            use_cached = options["smtpproxy", "use_cached_message"]
        except KeyError:
            use_cached = True
        if use_cached:
            id = self.extractSpambayesID(msg)
            if id is None:
                print("Could not extract id")
                return
            self.train_cached_message(id, isSpam)
        msg = email.message_from_string(msg, _class=message.SBHeaderMessage)
        id = msg.setIdFromPayload()
        msg.delSBHeaders()
        if id is None:
            self.classifier.learn(msg.tokenize(), isSpam)
        else:
            if msg.GetTrained() == (not isSpam):
                self.classifier.unlearn(msg.tokenize(), not isSpam)
                msg.RememberTrained(None)
            if msg.GetTrained() is None:
                self.classifier.learn(msg.tokenize(), isSpam)
                msg.RememberTrained(isSpam)
    def train_cached_message(self, id, isSpam):
        if not self.train_message_in_pop3proxy_cache(id, isSpam) and \
           not self.train_message_on_imap_server(id, isSpam):
            print("Could not find message (%s); perhaps it was " \
                  "deleted from the POP3Proxy cache or the IMAP " \
                  "server.  This means that no training was done." % (id, ))
    def train_message_in_pop3proxy_cache(self, id, isSpam):
        if self.state is None:
            return False
        sourceCorpus = None
        for corpus in [self.state.unknownCorpus, self.state.hamCorpus,
                       self.state.spamCorpus]:
            if corpus.get(id) is not None:
                sourceCorpus = corpus
                break
        if corpus is None:
            return False
        if isSpam == True:
            targetCorpus = self.state.spamCorpus
        else:
            targetCorpus = self.state.hamCorpus
        targetCorpus.takeMessage(id, sourceCorpus)
        self.classifier.store()
        return True
    def train_message_on_imap_server(self, id, isSpam):
        if self.imap is None:
            return False
        msg = self.imap.FindMessage(id)
        if msg is None:
            return False
        if msg.GetTrained() == (not isSpam):
            msg.get_substance()
            msg.delSBHeaders()
            self.classifier.unlearn(msg.tokenize(), not isSpam)
            msg.RememberTrained(None)
        if msg.GetTrained() is None:
            msg.get_substance()
            msg.delSBHeaders()
            self.classifier.learn(msg.tokenize(), isSpam)
            msg.RememberTrained(isSpam)
        self.classifier.store()
        return True
def LoadServerInfo():
    servers = []
    proxyPorts = []
    if options["smtpproxy", "remote_servers"]:
        for server in options["smtpproxy", "remote_servers"]:
            server = server.strip()
            if server.find(':') > -1:
                server, port = server.split(':', 1)
            else:
                port = '25'
            servers.append((server, int(port)))
    if options["smtpproxy", "listen_ports"]:
        splitPorts = options["smtpproxy", "listen_ports"]
        proxyPorts = list(map(_addressAndPort, splitPorts))
    if len(servers) != len(proxyPorts):
        print("smtpproxy:remote_servers & smtpproxy:listen_ports are " + \
              "different lengths!")
        sys.exit()
    return servers, proxyPorts
def CreateProxies(servers, proxyPorts, trainer):
    """Create BayesSMTPProxyListeners for all the given servers."""
    proxyListeners = []
    for (server, serverPort), proxyPort in zip(servers, proxyPorts):
        listener = BayesSMTPProxyListener(server, serverPort, proxyPort,
                                          trainer)
        proxyListeners.append(listener)
    return proxyListeners
