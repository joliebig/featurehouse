"""POP3Proxy and SMTPProxy Web Interface
Classes:
    ProxyUserInterface - Interface class for pop3proxy and smtpproxy
Abstract:
This module implements a browser based Spambayes user interface for the
POP3, IMAP4 SMTP proxies.  Users may use it to interface with the
proxies.
The following functions are currently included:
[From the base class UserInterface]
  onClassify - classify a given message
  onWordquery - query a word from the database
  onTrain - train a message or mbox
  onSave - save the database and possibly shutdown
[Here]
  onHome - a home page with various options
  onUpload - upload a message for later training (used by proxytee.py)
  onReview - show messages in corpii
  onView - view a message from one of the corpii
  onShowclues - show clues for a message
To do:
Web training interface:
 o Review already-trained messages, and purge them.
 o Add a Today button on the Review page.
User interface improvements:
 o Can it cleanly dynamically update its status display while having a POP3
   conversation?  Hammering reload sucks.
 o Suggestions?
"""
__author__ = "Richie Hindle <richie@entrian.com>"
__credits__ = "Tim Peters, Neale Pickett, Tim Stone, all the Spambayes folk."
import cgi
import time
import types
import bisect
from spambayes import UserInterface
from spambayes.Options import options, _
state = None
parm_ini_map = (
    ('POP3 Proxy Options',  None),
    ('pop3proxy',           'remote_servers'),
    ('pop3proxy',           'listen_ports'),
    ('SMTP Proxy Options',  None),
    ('smtpproxy',           'remote_servers'),
    ('smtpproxy',           'listen_ports'),
    ('smtpproxy',           'ham_address'),
    ('smtpproxy',           'spam_address'),
    ('smtpproxy',           'use_cached_message'),
    ('Header Options',      None),
    ('Headers',             'notate_to'),
    ('Headers',             'notate_subject'),
    ('Storage Options',     None),
    ('Storage',             'persistent_storage_file'),
    ('Storage',             'messageinfo_storage_file'),
    ('Storage',             'cache_messages'),
    ('Storage',             'no_cache_bulk_ham'),
    ('Storage',             'no_cache_large_messages'),
    ('Statistics Options',  None),
    ('Categorization',      'ham_cutoff'),
    ('Categorization',      'spam_cutoff'),
)
adv_map = (
    (_('Statistics Options'), None),
    ('Classifier',            'max_discriminators'),
    ('Classifier',            'minimum_prob_strength'),
    ('Classifier',            'unknown_word_prob'),
    ('Classifier',            'unknown_word_strength'),
    ('Classifier',            'use_bigrams'),
    (_('Header Options'),     None),
    ('Headers',               'include_score'),
    ('Headers',               'header_score_digits'),
    ('Headers',               'header_score_logarithm'),
    ('Headers',               'include_thermostat'),
    ('Headers',               'include_evidence'),
    ('Headers',               'clue_mailheader_cutoff'),
    (_('Storage Options'),    None),
    ('Storage',               'persistent_use_database'),
    ('Storage',               'cache_expiry_days'),
    ('Storage',               'cache_use_gzip'),
    ('Storage',               'ham_cache'),
    ('Storage',               'spam_cache'),
    ('Storage',               'unknown_cache'),
    (_('Tokenising Options'), None),
    ('Tokenizer',             'mine_received_headers'),
    ('Tokenizer',             'replace_nonascii_chars'),
    ('Tokenizer',             'summarize_email_prefixes'),
    ('Tokenizer',             'summarize_email_suffixes'),
    (_('Training Options'),   None),
    ('Hammie',                'train_on_filter'),
    (_('Interface Options'),  None),
    ('html_ui',               'display_headers'),
    ('html_ui',               'display_received_time'),
    ('html_ui',               'display_score'),
    ('html_ui',               'display_adv_find'),
    ('html_ui',               'default_ham_action'),
    ('html_ui',               'default_spam_action'),
    ('html_ui',               'default_unsure_action'),
    ('html_ui',               'ham_discard_level'),
    ('html_ui',               'spam_discard_level'),
    ('html_ui',               'allow_remote_connections'),
    ('html_ui',               'http_authentication'),
    ('html_ui',               'http_user_name'),
    ('html_ui',               'http_password'),
    ('pop3proxy',             'allow_remote_connections'),
    ('smtpproxy',             'allow_remote_connections'),
    ('globals',               'language'),
    (_('POP3 Proxy Options'), None),
    ('pop3proxy',             'retrieval_timeout'),
)
class ProxyUserInterface(UserInterface.UserInterface):
    """Serves the HTML user interface for the proxies."""
    def __init__(self, proxy_state, state_recreator):
        global state
        UserInterface.UserInterface.__init__(self, proxy_state.bayes,
                                             parm_ini_map, adv_map,
                                             proxy_state.lang_manager,
                                             proxy_state.stats)
        state = proxy_state
        self.state_recreator = state_recreator # ugly
        self.app_for_version = "SpamBayes Proxy"
        if not proxy_state.can_stop:
            self.html._readonly = False
            self.html.shutdownTableCell = "&nbsp;"
            self.html._readonly = True
    def onHome(self):
        """Serve up the homepage."""
        state.buildStatusStrings()
        stateDict = state.__dict__.copy()
        stateDict.update(state.bayes.__dict__)
        statusTable = self.html.statusTable.clone()
        if not state.servers:
            statusTable.proxyDetails = _("No POP3 proxies running.<br/>")
        findBox = self._buildBox(_('Word query'), 'query.gif',
                                 self.html.wordQuery)
        if not options["html_ui", "display_adv_find"]:
            del findBox.advanced
        content = (self._buildBox(_('Status and Configuration'),
                                  'status.gif', statusTable % stateDict)+
                   self._buildBox(_('Train on proxied messages'),
                                  'train.gif', self.html.reviewText) +
                   self._buildTrainBox() +
                   self._buildClassifyBox() +
                   findBox +
                   self._buildBox(_('Find message'), 'query.gif',
                                  self.html.findMessage)
                   )
        self._writePreamble(_("Home"))
        self.write(content)
        self._writePostamble(help_topic="home_proxy")
    def onUpload(self, file):
        """Save a message for later training - used by Skip's proxytee.py."""
        file = file.replace('\r\n', '\n').replace('\r', '\n')
        messages = self._convertUploadToMessageList(file)
        for m in messages:
            messageName = state.getNewMessageName()
            message = state.unknownCorpus.makeMessage(messageName, m)
            state.unknownCorpus.addMessage(message)
        self.write(_("<p>OK. Return <a href='home'>Home</a>.</p>"))
    def _buildReviewKeys(self, timestamp):
        """Builds an ordered list of untrained message keys, ready for output
        in the Review list.  Returns a 5-tuple: the keys, the formatted date
        for the list (eg. "Friday, November 15, 2002"), the start of the prior
        page or zero if there isn't one, likewise the start of the given page,
        and likewise the start of the next page."""
        allKeys = state.unknownCorpus.keys()
        allKeys.sort()
        if not timestamp:
            if allKeys:
                timestamp = self._keyToTimestamp(allKeys[-1])
            else:
                timestamp = time.time()
        start, end, date = self._getTimeRange(timestamp)
        startKeyIndex = bisect.bisect(allKeys, "%d" % long(start))
        endKeyIndex = bisect.bisect(allKeys, "%d" % long(end))
        keys = allKeys[startKeyIndex:endKeyIndex]
        keys.reverse()
        prior = end = 0
        if startKeyIndex != 0:
            prior = self._keyToTimestamp(allKeys[startKeyIndex-1])
        if endKeyIndex != len(allKeys):
            end = self._keyToTimestamp(allKeys[endKeyIndex])
        return keys, date, prior, start, end
    def onReview(self, **params):
        """Present a list of message for (re)training."""
        self._writePreamble("Review")
        id = ''
        numTrained = 0
        numDeferred = 0
        if params.get('go') != _('Refresh'):
            for key, value in params.items():
                if key.startswith('classify:'):
                    old_class, id = key.split(':')[1:3]
                    if value == _('spam'):
                        targetCorpus = state.spamCorpus
                        stats_as_ham = False
                    elif value == _('ham'):
                        targetCorpus = state.hamCorpus
                        stats_as_ham = True
                    elif value == _('discard'):
                        targetCorpus = None
                        try:
                            state.unknownCorpus.removeMessage(\
                                state.unknownCorpus[id])
                        except KeyError:
                            pass  # Must be a reload.
                    else: # defer
                        targetCorpus = None
                        numDeferred += 1
                    if targetCorpus:
                        sourceCorpus = None
                        if state.unknownCorpus.get(id) is not None:
                            sourceCorpus = state.unknownCorpus
                        elif state.hamCorpus.get(id) is not None:
                            sourceCorpus = state.hamCorpus
                        elif state.spamCorpus.get(id) is not None:
                            sourceCorpus = state.spamCorpus
                        if sourceCorpus is not None:
                            try:
                                targetCorpus.takeMessage(id, sourceCorpus,
                                                         fromCache=True)
                                if numTrained == 0:
                                    self.write(_("<p><b>Training... "))
                                    self.flush()
                                numTrained += 1
                                self.stats.RecordTraining(\
                                  stats_as_ham, old_class=old_class)
                            except KeyError:
                                pass  # Must be a reload.
        if numTrained > 0:
            plural = ''
            if numTrained == 1:
                response = "Trained on one message. "
            else:
                response = "Trained on %d messages. " % (numTrained,)
            self._doSave()
            self.write(response)
            self.write("<br>&nbsp;")
        title = ""
        keys = []
        sourceCorpus = state.unknownCorpus
        if numDeferred > 0:
            start = self._keyToTimestamp(id)
        elif id:
            start = self._keyToTimestamp(id)
            unused, unused, prior, unused, next = self._buildReviewKeys(start)
            if prior:
                start = prior
            else:
                start = next
        elif params.get('go') == _('Next day'):
            start = self._keyToTimestamp(params['next'])
        elif params.get('go') == _('Previous day'):
            start = self._keyToTimestamp(params['prior'])
        elif params.get('find') is not None:
            prior = next = 0
            keys = set()        # so we don't end up with duplicates
            push = keys.add
            try:
                max_results = int(params['max_results'])
            except ValueError:
                max_results = 1
            key = params['find']
            if params.has_key('ignore_case'):
                ic = True
            else:
                ic = False
            error = False
            if key == "":
                error = True
                page = _("<p>You must enter a search string.</p>")
            else:
                if len(keys) < max_results and \
                   params.has_key('id'):
                    if state.unknownCorpus.get(key):
                        push((key, state.unknownCorpus))
                    elif state.hamCorpus.get(key):
                        push((key, state.hamCorpus))
                    elif state.spamCorpus.get(key):
                        push((key, state.spamCorpus))
                if params.has_key('subject') or params.has_key('body') or \
                   params.has_key('headers'):
                    self.write(_('<p>Searching...</p>'))
                    for corp in [state.unknownCorpus, state.hamCorpus,
                                   state.spamCorpus]:
                        for k in corp.keys():
                            if len(keys) >= max_results:
                                break
                            msg = corp[k]
                            msg.load()
                            if params.has_key('subject'):
                                subj = str(msg['Subject'])
                                if self._contains(subj, key, ic):
                                    push((k, corp))
                            if params.has_key('body'):
                                msg_body = msg.as_string()
                                msg_body = msg_body[msg_body.index('\r\n\r\n'):]
                                if self._contains(msg_body, key, ic):
                                    push((k, corp))
                            if params.has_key('headers'):
                                for nm, val in msg.items():
                                    nm = str(nm)
                                    val = str(val)
                                    if self._contains(nm, key, ic) or \
                                       self._contains(val, key, ic):
                                        push((k, corp))
                if len(keys):
                    if len(keys) == 1:
                        title = _("Found message")
                    else:                      
                        title = _("Found messages")
                    keys = list(keys)
                else:
                    page = _("<p>Could not find any matching messages. " \
                             "Maybe they expired?</p>")
                    title = _("Did not find message")
                    box = self._buildBox(title, 'status.gif', page)
                    self.write(box)
                    self.write(self._buildBox(_('Find message'),
                                              'query.gif',
                                              self.html.findMessage))
                    self._writePostamble()
                    return
        else:
            start = 0
        if len(keys) == 0:
            keys, date, prior, this, next = self._buildReviewKeys(start)
        keyedMessageInfo = {options["Headers", "header_unsure_string"]: [],
                            options["Headers", "header_ham_string"]: [],
                            options["Headers", "header_spam_string"]: [],
                            }
        invalid_keys = []
        for key in keys:
            if isinstance(key, types.TupleType):
                key, sourceCorpus = key
            else:
                sourceCorpus = state.unknownCorpus
            message = sourceCorpus[key]
            try:
                message.load()
            except IOError:
                invalid_keys.append(key)
                continue
            judgement = message[options["Headers",
                                        "classification_header_name"]]
            if judgement is None:
                judgement = options["Headers", "header_unsure_string"]
            else:
                judgement = judgement.split(';')[0].strip()
            messageInfo = self._makeMessageInfo(message)
            keyedMessageInfo[judgement].append((key, messageInfo))
        for key in invalid_keys:
            keys.remove(key)
        if keys:
            page = self.html.reviewtable.clone()
            if prior:
                page.prior.value = prior
                del page.priorButton.disabled
            if next:
                page.next.value = next
                del page.nextButton.disabled
            templateRow = page.reviewRow.clone()
            sort_order = params.get('sort')
            if self.previous_sort == sort_order:
                reverse = True
                self.previous_sort = None
            else:
                reverse = False
                self.previous_sort = sort_order
            page.table = ""  # To make way for the real rows.
            for header, label in ((options["Headers",
                                           "header_unsure_string"], 'Unsure'),
                                  (options["Headers",
                                           "header_ham_string"], 'Ham'),
                                  (options["Headers",
                                           "header_spam_string"], 'Spam')):
                messages = keyedMessageInfo[header]
                if messages:
                    sh = self.html.reviewSubHeader.clone()
                    sh.optionalHeaders = ''
                    h = self.html.headerHeader.clone()
                    for disp_header in options["html_ui", "display_headers"]:
                        h.headerLink.href = 'review?sort=%sHeader' % \
                                            (disp_header.lower(),)
                        h.headerName = disp_header.title()
                        sh.optionalHeaders += h
                    if not options["html_ui", "display_score"]:
                        del sh.score_header
                    if not options["html_ui", "display_received_time"]:
                        del sh.received_header
                    subHeader = str(sh)
                    subHeader = subHeader.replace('TYPE', label)
                    page.table += self.html.blankRow
                    page.table += subHeader
                    self._appendMessages(page.table, messages, label,
                                         sort_order, reverse)
            page.table += self.html.trainRow
            if title == "":
                title = _("Untrained messages received on %s") % date
            box = self._buildBox(title, None, page)  # No icon, to save space.
        else:
            page = _("<p>There are no untrained messages to display. " \
                     "Return <a href='home'>Home</a>, or " \
                     "<a href='review'>check again</a>.</p>")
            title = _("No untrained messages")
            box = self._buildBox(title, 'status.gif', page)
        self.write(box)
        self._writePostamble(help_topic="review")
    def onView(self, key, corpus):
        """View a message - linked from the Review page."""
        self._writePreamble(_("View message"),
                            parent=('review', _('Review')))
        sourceCorpus = None
        message = None
        if state.unknownCorpus.get(key) is not None:
            sourceCorpus = state.unknownCorpus
        elif state.hamCorpus.get(key) is not None:
            sourceCorpus = state.hamCorpus
        elif state.spamCorpus.get(key) is not None:
            sourceCorpus = state.spamCorpus
        if sourceCorpus is not None:
            message = sourceCorpus.get(key)
        if message is not None:
            self.write("<pre>%s</pre>" % cgi.escape(message.as_string()))
        else:
            self.write(_("<p>Can't find message %r. Maybe it expired.</p>") % key)
        self._writePostamble()
    def onShowclues(self, key, subject, tokens='0'):
        """Show clues for a message - linked from the Review page."""
        tokens = bool(int(tokens)) # needs the int, as bool('0') is True
        self._writePreamble(_("Message clues"),
                            parent=('review', _('Review')))
        sourceCorpus = None
        message = None
        if state.unknownCorpus.get(key) is not None:
            sourceCorpus = state.unknownCorpus
        elif state.hamCorpus.get(key) is not None:
            sourceCorpus = state.hamCorpus
        elif state.spamCorpus.get(key) is not None:
            sourceCorpus = state.spamCorpus
        if sourceCorpus is not None:
            message = sourceCorpus.get(key).as_string()
        if message is not None:
            message = message.replace('\r\n', '\n').replace('\r', '\n') # For Macs
            results = self._buildCluesTable(message, subject, tokens)
            del results.classifyAnother
            self.write(results)
        else:
            self.write(_("<p>Can't find message %r. Maybe it expired.</p>") % key)
        self._writePostamble()
    def close_database(self):
        state.close()
    def reReadOptions(self):
        """Called by the config page when the user saves some new options, or
        restores the defaults."""
        global state
        import Options
        Options.load_options()
        global options
        from Options import options
        state = self.state_recreator()
        self.classifier = state.bayes
    def verifyInput(self, parms, pmap):
        '''Check that the given input is valid.'''
        errmsg = UserInterface.UserInterface.verifyInput(self, parms, pmap)
        if pmap != parm_ini_map:
            return errmsg
        slist = list(parms['pop3proxy_remote_servers'])
        plist = list(parms['pop3proxy_listen_ports'])
        if len(slist) != len(plist):
            errmsg += _('<li>The number of POP3 proxy ports specified ' \
                        'must match the number of servers specified</li>\n')
        plist.sort()
        for p in range(len(plist)-1):
            try:
                if plist[p] == plist[p+1]:
                    errmsg += _('<li>All POP3 port numbers must be unique</li>')
                    break
            except IndexError:
                pass
        slist = list(parms['smtpproxy_remote_servers'])
        plist = list(parms['smtpproxy_listen_ports'])
        if len(slist) != len(plist):
            errmsg += _('<li>The number of SMTP proxy ports specified ' \
                        'must match the number of servers specified</li>\n')
        plist.sort()
        for p in range(len(plist)-1):
            try:
                if plist[p] == plist[p+1]:
                    errmsg += _('<li>All SMTP port numbers must be unique</li>')
                    break
            except IndexError:
                pass
        return errmsg
