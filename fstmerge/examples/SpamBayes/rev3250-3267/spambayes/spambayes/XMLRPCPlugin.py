"""
XML-RPC plugin for SpamBayes core server.
This plugin opens an XML-RPC server in a separate thread listening to the
given host and port (default localhost:5001).  In Python 2.5 and later it
also enforces a path (default /sbrpc).
SECURITY NOTE: The XML-RPC plugin provide *NO SECURITY*.  It would be
unwise to listen to anything besides 'localhost'.  Similarly, when
running the core_server configured with the XML-RPC plugin it's quite
likely that the main core_server interface will have to listen to
something other than localhost to allow administrators to administer
it remotely.  Access to that URL should only be available to a set of
trusted administrators, probably by proxy through some other webserver
which provides the necessary authentication support.
The XML-RPC server exposes the following methods:
    score(form_dict, extra_tokens) -> (score, evidence)
        Scores a dictionary representing the contents of a web
        submission form and a list of any extra tokens provided
        by the caller.  The return value is a list containing
        the spam probability of the input and a set of (token,
        probability) pairs for the most significant tokens.
    score_mime(msg, encoding) -> (score, evidence)
        Scores a MIME message (a string encoded using encoding).
        The return value is as for the score method.
The following options are available in the Plugin section of the options.
    xmlrpc_host - host to listen to (default: localhost)
    xmlrpc_port - port to listen to (default: 8001)
    xmlrpc_path - path to support (default: /sbrpc)
"""

__author__ = "Skip Montanaro <skip@pobox.com>"

__credits__ = "All the Spambayes folk."

import threading

import xmlrpc.client

import time

from email import Message, message_from_string

from xmlrpc.server import SimpleXMLRPCServer

from spambayes.CorePlugin import Plugin, PluginUI

from spambayes.Options import _, options

from spambayes.tokenizer import tokenize

import spambayes.message

from spambayes import storage

from spambayes import FileCorpus

class  XMLRPCUI (PluginUI) :
	plugin_map = (
        (_('XML-RPC Options'), None),
        ('Plugin',            'xmlrpc_path'),
        ('Plugin',            'xmlrpc_host'),
        ('Plugin',            'xmlrpc_port'),
        )
class  XMLRPCPlugin (Plugin) :
	def __init__(self, name, ui):

        Plugin.__init__(self, name, ui)

        host = options["Plugin", "xmlrpc_host"]

        port = options["Plugin", "xmlrpc_port"]

        path = options["Plugin", "xmlrpc_path"]

        self.server = SimpleXMLRPCServer((host, port))

        self.server.RequestHandlerClass.rpc_paths = (path,)

        self.server.register_instance(self)

        self.thread = threading.Thread(target=self.server.serve_forever)

        self.thread.setDaemon(True)

        self.thread.start()
 def _dispatch(self, method, params):

        if method in ("score", "score_mime", "train", "train_mime"):

            return getattr(self, method)(*params)

        else:

            raise xmlrpc.client.Fault(404, '"%s" is not supported' % method)
 def train(self, form_dict, extra_tokens, attachments, is_spam=True):

        newdict = {}

        for (i, k) in list(form_dict.items()):

            if isinstance(k, str):

                k = k.encode("utf-8")

            newdict[i] = k

        mime_message = form_to_mime(newdict, extra_tokens, attachments)

        mime_message = str(mime_message.as_string(), "utf-8").encode("utf-8")

        self.train_mime(mime_message, "utf-8", is_spam)

        return ""
 def train_mime(self, msg_text, encoding, is_spam):

        if self.state.bayes is None:

            self.state.create_workers()

        if isinstance(msg_text, str):

            msg_text = str(msg_text, encoding)

        if isinstance(msg_text, str):

            msg_text = msg_text.encode("utf-8")

        msg = message_from_string(msg_text,
                                  _class=spambayes.message.SBHeaderMessage)

        if is_spam:

            desired_corpus = "spamCorpus"

        else:

            desired_corpus = "hamCorpus"

        if hasattr(self, desired_corpus):

            corpus = getattr(self, desired_corpus)

        else:

            if hasattr(self, "state"):

                corpus = getattr(self.state, desired_corpus)

                setattr(self, desired_corpus, corpus)

                self.msg_name_func = self.state.getNewMessageName

            else:

                if is_spam:

                    fn = storage.get_pathname_option("Storage",
                                                     "spam_cache")

                else:

                    fn = storage.get_pathname_option("Storage",
                                                     "ham_cache")

                storage.ensureDir(fn)

                if options["Storage", "cache_use_gzip"]:

                    factory = FileCorpus.GzipFileMessageFactory()

                else:

                    factory = FileCorpus.FileMessageFactory()

                age = options["Storage", "cache_expiry_days"]*24*60*60

                corpus = FileCorpus.ExpiryFileCorpus(age, factory, fn,
                                                     '[0123456789\-]*',
                                                     cacheSize=20)

                setattr(self, desired_corpus, corpus)

                class UniqueNamer(object):

                    count = -1

                    def generate_name(self):

                        self.count += 1

                        return "%10.10d-%d" % (int(time.time()), self.count)

                Namer = UniqueNamer()

                self.msg_name_func = Namer.generate_name

        key = self.msg_name_func()

        mime_message = str(msg.as_string(), "utf-8").encode("utf-8")

        msg = corpus.makeMessage(key, mime_message)

        msg.setId(key)

        corpus.addMessage(msg)

        msg.RememberTrained(is_spam)
 def train_spam(self, form_dict, extra_tokens, attachments):

        pass
 def train_ham(self, form_dict, extra_tokens, attachments):

        pass
 def score(self, form_dict, extra_tokens, attachments):

        """Score a dictionary + extra tokens."""

        newdict = {}

        for (i, k) in list(form_dict.items()):

            if isinstance(k, str):

                k = k.encode("utf-8")

            newdict[i] = k

        mime_message = form_to_mime(newdict, extra_tokens, attachments)

        mime_message = str(mime_message.as_string(), "utf-8").encode("utf-8")

        return self.score_mime(mime_message, "utf-8")
 def score_mime(self, msg_text, encoding):

        """Score a message representing a MIME document.
        The msg argument will be a string in the given encoding.
        """

        if self.state.bayes is None:

            self.state.create_workers()

        if isinstance(msg_text, str):

            msg_text = str(msg_text, encoding)

        if isinstance(msg_text, str):

            msg_text = msg_text.encode("utf-8")

        msg = message_from_string(msg_text,
                                  _class=spambayes.message.SBHeaderMessage)

        tokens = tokenize(msg)

        prob, clues = self.state.bayes.spamprob(tokens, evidence=True)

        msg.addSBHeaders(prob, clues)

        self.state.record_classification(msg.GetClassification(), prob)

        if not self.state.is_test and options["Storage", "cache_messages"]:

            msg.setId(self.state.getNewMessageName())

            makeMessage = self.state.unknownCorpus.makeMessage

            message = makeMessage(msg.getId(), msg.as_string())

            self.state.unknownCorpus.addMessage(message)

        return prob

def form_to_mime(form, extra_tokens, attachments):

    """Encode submission form bits as a MIME message.
    form - a dictionary of key/value pairs representing the form's contents
    extra_tokens - a sequence of synthetic tokens generated by the caller.
      For example, if you include a honeypot hidden field in your form, you
      might generate a synthetic token which tells if it was filled in or not.
      You might also generate tokens which indicate how long a submitting
      username has existed or how many successful posts that username has
      submitted.
    attachments - list of dictionaries describing an attachment.
      The 'payload' key is required.  If there is no 'content-type' key
      'application/octet-stream' is assumed.  If 'content-transfer-encoding'
      is given it will be added to the headers of the attachment.  Note that
      the keys are case-sensitive and must be lower case.
    """

    msg = Message.Message()

    msg.set_type("multipart/digest")

    msg.add_header("Subject", "Form submission")

    msg.add_header("From", "SpamBayes XMLRPC Plugin <webmaster@localhost>")

    main = Message.Message()

    main.set_type("text/plain")

    main.set_payload("\n".join(["%s:%s" % (k, v) for (k, v) in list(form.items())]))

    msg.attach(main)

    extra = Message.Message()

    extra.set_type("text/plain")

    extra.set_payload("\n".join(extra_tokens))

    msg.attach(extra)

    for content in attachments:

        mime_type = content.get("content-type") or "application/octet-stream"

        attachment = Message.Message()

        if "content-transfer-encoding" in content:

            attachment.add_header("Content-Transfer-Encoding",
                                  content["content-transfer-encoding"])

        attachment.set_type(mime_type)

        attachment.set_payload(content["payload"])

        msg.attach(attachment)

    return msg
 def register():

    return XMLRPCPlugin("XMLRPC", XMLRPCUI())






