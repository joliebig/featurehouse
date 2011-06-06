"""Web User Interface
Classes:
    UserInterfaceServer - Implements the web server component
                          via a Dibbler plugin.
    BaseUserInterface - Just has utilities for creating boxes and so forth.
                        (Does not include any pages)
    UserInterface - A base class for Spambayes web user interfaces.
Abstract:
This module implements a browser based Spambayes user interface.  Users can
*not* use this class (there is no 'home' page), but developments should
sub-class it to provide an appropriate interface for their application.
Functions deemed appropriate for all application interfaces are included.
These currently include:
  onClassify - classify a given message
  onWordquery - query a word from the database
  onTrain - train a message or mbox
  onSave - save the database and possibly shutdown
  onConfig - present the appropriate configuration page
  onAdvancedconfig - present the appropriate advanced configuration page
  onExperimentalconfig - present the experimental options configuration page
  onHelp - present the help page
  onStats - present statistics information
  onBugreport - help the user fill out a bug report
To Do:
Web training interface:
 o Functional tests.
 o Keyboard navigation (David Ascher).  But aren't Tab and left/right
   arrow enough?
User interface improvements:
 o Once the pieces are on separate pages, make the paste box bigger.
 o Deployment: Windows executable?  atlaxwin and ctypes?  Or just
   webbrowser?
 o Save the stats (num classified, etc.) between sessions.
 o "Reload database" button.
 o Displaying options should be done with the locale format function
   rather than str().
 o Suggestions?
"""
__author__ = """Richie Hindle <richie@entrian.com>,
                Tim Stone <tim@fourstonesExpressions.com>"""
__credits__ = "Tim Peters, Neale Pickett, Tony Meyer, all the Spambayes folk."
try:
    True, False
except NameError:
    True, False = 1, 0
import re
import os
import sys
import time
import email
import smtplib
import binascii
import cgi
import mailbox
import types
import StringIO
from email.Iterators import typed_subpart_iterator
import oe_mailbox
import PyMeldLite
import Dibbler
import tokenizer
from spambayes import Stats
from spambayes import Version
from spambayes import storage
from spambayes import FileCorpus
from Options import options, optionsPathname, defaults, OptionsClass, _
IMAGES = ('helmet', 'status', 'config', 'help',
          'message', 'train', 'classify', 'query')
experimental_ini_map = (
    ('Experimental Options', None),
)
for opt in options.options(True):
    sect, opt = opt[1:].split(']', 1)
    if opt[:2].lower() == "x-" and \
       not options.doc(sect, opt).lower().startswith(_("(deprecated)")):
        experimental_ini_map += ((sect, opt),)
class UserInterfaceServer(Dibbler.HTTPServer):
    """Implements the web server component via a Dibbler plugin."""
    def __init__(self, uiPort):
        Dibbler.HTTPServer.__init__(self, uiPort)
        print _('User interface url is http://localhost:%d/') % (uiPort)
    def requestAuthenticationMode(self):
        return options["html_ui", "http_authentication"]
    def getRealm(self):
        return _("SpamBayes Web Interface")
    def isValidUser(self, name, password):
        return (name == options["html_ui", "http_user_name"] and
                password == options["html_ui", "http_password"])
    def getPasswordForUser(self, name):
        return options["html_ui", "http_password"]
    def getCancelMessage(self):
        return _("You must login to use SpamBayes.")
class BaseUserInterface(Dibbler.HTTPPlugin):
    def __init__(self, lang_manager=None):
        Dibbler.HTTPPlugin.__init__(self)
        self.lang_manager = lang_manager
        htmlSource, self._images = self.readUIResources()
        self.html = PyMeldLite.Meld(htmlSource, readonly=True)
        self.app_for_version = "SpamBayes"
    def onIncomingConnection(self, clientSocket):
        """Checks the security settings."""
        remoteIP = clientSocket.getpeername()[0]
        trustedIPs = options["html_ui", "allow_remote_connections"]
        if trustedIPs == "*" or remoteIP == clientSocket.getsockname()[0]:
            return True
        trustedIPs = trustedIPs.replace('.', '\.').replace('*', '([01]?\d\d?|2[04]\d|25[0-5])')
        for trusted in trustedIPs.split(','):
            if re.search("^" + trusted + "$", remoteIP):
                return True
        return False
    def _getHTMLClone(self, help_topic=None):
        """Gets a clone of the HTML, with the footer timestamped, and
        version information added, ready to be modified and sent to the
        browser."""
        clone = self.html.clone()
        timestamp = time.strftime('%H:%M on %A %B %d %Y', time.localtime())
        clone.footer.timestamp = timestamp
        v = Version.get_current_version()
        clone.footer.version = v.get_long_version(self.app_for_version)
        if help_topic:
            clone.helplink.href = "help?topic=%s" % (help_topic,)
        return clone
    def _writePreamble(self, name, parent=None, showImage=True):
        """Writes the HTML for the beginning of a page - time-consuming
        methlets use this and `_writePostamble` to write the page in
        pieces, including progress messages.  `parent` (if given) should
        be a pair: `(url, label)`, eg. `('review', 'Review')`."""
        html = self._getHTMLClone()
        html.mainContent = " "
        del html.footer
        html.title = name
        if name == _('Home'):
            del html.homelink
            html.pagename = _("Home")
        elif parent:
            html.pagename = "> <a href='%s'>%s</a> > %s" % \
                            (parent[0], parent[1], name)
        else:
            html.pagename = "> " + name
        if not showImage:
            del html.helmet
        self.writeOKHeaders('text/html')
        self.write(re.sub(r'</div>\s*</body>\s*</html>', '', str(html)))
    def _writePostamble(self, help_topic=None):
        """Writes the end of time-consuming pages - see `_writePreamble`."""
        self.write("</div>" + self._getHTMLClone(help_topic).footer)
        self.write("</body></html>")
    def _trimHeader(self, field, limit, quote=False):
        """Trims a string, adding an ellipsis if necessary and HTML-quoting
        on request.  Also pumps it through email.Header.decode_header, which
        understands charset sections in email headers - I suspect this will
        only work for Latin character sets, but hey, it works for Francois
        Granger's name.  8-)"""
        try:
            sections = email.Header.decode_header(field)
        except (binascii.Error, email.Errors.HeaderParseError):
            sections = [(field, None)]
        field = ' '.join([text for text, unused in sections])
        if len(field) > limit:
            field = field[:limit-3] + "..."
        if quote:
            field = cgi.escape(field)
        return field
    def onHome(self):
        """Serve up the homepage."""
        raise NotImplementedError
    def _writeImage(self, image):
        self.writeOKHeaders('image/gif')
        self.write(self._images[image])
    for imageName in IMAGES:
        exec "def %s(self): self._writeImage('%s')" % \
             ("on%sGif" % imageName.capitalize(), imageName)
    def _buildBox(self, heading, icon, content):
        """Builds a yellow-headed HTML box."""
        box = self.html.headedBox.clone()
        box.heading = heading
        if icon:
            box.icon.src = icon
        else:
            del box.iconCell
        box.boxContent = content
        return box
    def readUIResources(self):
        """Returns ui.html and a dictionary of Gifs."""
        if self.lang_manager:
            ui_html = self.lang_manager.import_ui_html()
        else:
            from spambayes.resources import ui_html
        images = {}
        for baseName in IMAGES:
            moduleName = '%s.%s_gif' % ('spambayes.resources', baseName)
            module = __import__(moduleName, {}, {}, ('spambayes', 'resources'))
            images[baseName] = module.data
        return ui_html.data, images
class UserInterface(BaseUserInterface):
    """Serves the HTML user interface."""
    def __init__(self, bayes, config_parms=(), adv_parms=(),
                 lang_manager=None, stats=None):
        """Load up the necessary resources: ui.html and helmet.gif."""
        BaseUserInterface.__init__(self, lang_manager)
        self.classifier = bayes
        self.parm_ini_map = config_parms
        self.advanced_options_map = adv_parms
        self.stats = stats
        self.app_for_version = None # subclasses must fill this in
        self.previous_sort = None
    def onClassify(self, file, text, which):
        """Classify an uploaded or pasted message."""
        self._writePreamble(_("Classify"))
        message = file or text
        message = message.replace('\r\n', '\n').replace('\r', '\n') # For Macs
        results = self._buildCluesTable(message)
        results.classifyAnother = self._buildClassifyBox()
        self.write(results)
        self._writePostamble()
    ev_re = re.compile("%s:(.*?)(?:\n\S|\n\n)" % \
                       re.escape(options["Headers",
                                         "evidence_header_name"]),
                       re.DOTALL)
    sc_re = re.compile("%s:\s*([\d.]+)" % \
                       re.escape(options["Headers", "score_header_name"]))
    def _fillCluesTable(self, clues):
        accuracy = 6
        cluesTable = self.html.cluesTable.clone()
        cluesRow = cluesTable.cluesRow.clone()
        del cluesTable.cluesRow   # Delete dummy row to make way for real ones
        fetchword = self.classifier._wordinfoget
        for word, wordProb in clues:
            record = fetchword(word)
            if record:
                nham = record.hamcount
                nspam = record.spamcount
                if wordProb is None:
                    wordProb = self.classifier.probability(record)
            elif word != "*H*" and word != "*S*":
                nham = nspam = 0
            else:
                nham = nspam = "-"
            if wordProb is None:
                wordProb = "-"
            else:
                wordProb = round(float(wordProb), accuracy)
            cluesTable += cluesRow % (cgi.escape(word), wordProb,
                                      nham, nspam)
        return cluesTable
    def _buildCluesTable(self, message, subject=None, show_tokens=False):
        tokens = list(tokenizer.tokenize(message))
        if show_tokens:
            clues = [(tok, None) for tok in tokens]
            probability = self.classifier.spamprob(tokens)
            cluesTable = self._fillCluesTable(clues)
            head_name = _("Tokens")
        else:
            (probability, clues) = self.classifier.spamprob(tokens, evidence=True)
            cluesTable = self._fillCluesTable(clues)
            head_name = _("Clues")
        results = self.html.classifyResults.clone()
        results.probability = "%.2f%% (%s)" % (probability*100, probability)
        if subject is None:
            heading = "%s: (%s)" % (head_name, len(clues))
        else:
            heading = "%s for: %s (%s)" % (head_name, subject, len(clues))
        results.cluesBox = self._buildBox(heading, 'status.gif', cluesTable)
        if not show_tokens:
            mo = self.sc_re.search(message)
            if mo:
                prob = float(mo.group(1).strip())
                results.orig_prob_num = "%.2f%% (%s)" % (prob*100, prob)
            else:
                del results.orig_prob
            mo = self.ev_re.search(message)
            if mo:
                clues = []
                evidence = re.findall(r"'(.+?)': ([^;]+)(?:;|$)", mo.group(1))
                for word, prob in evidence:
                    clues.append((word, prob))
                cluesTable = self._fillCluesTable(clues)
                if subject is None:
                    heading = _("Original clues: (%s)") % (len(evidence),)
                else:
                    heading = _("Original clues for: %s (%s)") % \
                              (subject, len(evidence),)
                orig_results = self._buildBox(heading, 'status.gif',
                                              cluesTable)
                results.cluesBox += orig_results
        else:
            del results.orig_prob
        return results
    def onWordquery(self, word, query_type=_("basic"), max_results='10',
                    ignore_case=False):
        try:
            max_results = int(max_results)
        except ValueError:
            max_results = 10
        original_word = word
        query = self.html.wordQuery.clone()
        query.word.value = "%s" % (word,)
        for q_type in [query.advanced.basic,
                       query.advanced.wildcard,
                       query.advanced.regex]:
            if query_type == q_type.id:
                q_type.checked = 'checked'
                if query_type != _("basic"):
                    del query.advanced.max_results.disabled
        if ignore_case:
            query.advanced.ignore_case.checked = 'checked'
        query.advanced.max_results.value = str(max_results)
        queryBox = self._buildBox(_("Word query"), 'query.gif', query)
        if not options["html_ui", "display_adv_find"]:
            del queryBox.advanced
        stats = []
        if word == "":
            stats.append(_("You must enter a word."))
        elif query_type == _("basic") and not ignore_case:
            wordinfo = self.classifier._wordinfoget(word)
            if wordinfo:
                stat = (word, wordinfo.spamcount, wordinfo.hamcount,
                        self.classifier.probability(wordinfo))
            else:
                stat = _("%r does not exist in the database.") % \
                       cgi.escape(word)
            stats.append(stat)
        else:
            if query_type != _("regex"):
                word = re.escape(word)
            if query_type == _("wildcard"):
                word = word.replace("\\?", ".")
                word = word.replace("\\*", ".*")
            flags = 0
            if ignore_case:
                flags = re.IGNORECASE
            r = re.compile(word, flags)
            reached_limit = False
            for w in self.classifier._wordinfokeys():
                if not reached_limit and len(stats) >= max_results:
                    reached_limit = True
                    over_limit = 0
                if r.match(w):
                    if reached_limit:
                        over_limit += 1
                    else:
                        wordinfo = self.classifier._wordinfoget(w)
                        stat = (w, wordinfo.spamcount, wordinfo.hamcount,
                                self.classifier.probability(wordinfo))
                        stats.append(stat)
            if len(stats) == 0 and max_results > 0:
                stat = _("There are no words that begin with '%s' " \
                         "in the database.") % (word,)
                stats.append(stat)
            elif reached_limit:
                stat = _("Additional tokens not shown: %d") % (over_limit,)
                stats.append(stat)
        self._writePreamble(_("Word query"))
        if len(stats) == 1:
            if isinstance(stat, types.TupleType):
                stat = self.html.wordStats.clone()
                word = stats[0][0]
                stat.spamcount = stats[0][1]
                stat.hamcount = stats[0][2]
                stat.spamprob = "%.6f" % stats[0][3]
            else:
                stat = stats[0]
                word = original_word
            row = self._buildBox(_("Statistics for '%s'") % \
                                 cgi.escape(word), 'status.gif', stat)
            self.write(row)
        else:
            page = self.html.multiStats.clone()
            page.multiTable = "" # make way for the real rows
            page.multiTable += self.html.multiHeader.clone()
            stripe = 0
            for stat in stats:
                if isinstance(stat, types.TupleType):
                    row = self.html.statsRow.clone()
                    row.word, row.spamcount, row.hamcount = stat[:3]
                    row.spamprob = "%.6f" % stat[3]
                    setattr(row, 'class', ['stripe_on', 'stripe_off'][stripe])
                    stripe = stripe ^ 1
                    page.multiTable += row
                else:
                    self.write(self._buildBox(_("Statistics for '%s'") % \
                                              cgi.escape(original_word),
                                              'status.gif', stat))
            self.write(self._buildBox(_("Statistics for '%s'") % \
                                      cgi.escape(original_word), 'status.gif',
                                      page))
        self.write(queryBox)
        self._writePostamble()
    def onTrain(self, file, text, which):
        """Train on an uploaded or pasted message."""
        self._writePreamble(_("Train"))
        content = file or text
        isSpam = (which == _('Train as Spam'))
        if file:
            content = self._convertToMbox(content)
        content = content.replace('\r\n', '\n').replace('\r', '\n')
        messages = self._convertUploadToMessageList(content)
        if isSpam:
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
                if isSpam:
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
                                          '[0123456789\-]*', cacheSize=20)
                setattr(self, desired_corpus, corpus)
                class UniqueNamer(object):
                    count = -1
                    def generate_name(self):
                        self.count += 1
                        return "%10.10d-%d" % (long(time.time()), self.count)
                Namer = UniqueNamer()
                self.msg_name_func = Namer.generate_name
        self.write("<b>" + _("Training") + "...</b>\n")
        self.flush()
        for message in messages:
            key = self.msg_name_func()
            msg = corpus.makeMessage(key, message)
            msg.setId(key)
            corpus.addMessage(msg)
            msg.RememberTrained(isSpam)
            self.stats.RecordTraining(not isSpam)
        self._doSave()
        self.write(_("%sOK. Return %sHome%s or train again:%s") %
                   ("<p>", "<a href='home'>", "</a", "</p>"))
        self.write(self._buildTrainBox())
        self._writePostamble()
    def _convertToMbox(self, content):
        """Check if the given buffer is in a non-mbox format, and convert it
        into mbox format if so.  If it's already an mbox, return it unchanged.
        Currently, the only supported non-mbox format is Outlook Express DBX.
        In such a case we use the module oe_mailbox to convert the DBX
        content into a standard mbox file.  Testing if the file is a
        DBX one is very quick (just a matter of checking the first few
        bytes), and should not alter the overall performance."""
        content = oe_mailbox.convertToMbox(content)
        return content
    def _convertUploadToMessageList(self, content):
        """Returns a list of raw messages extracted from uploaded content.
        You can upload either a single message or an mbox file."""
        if content.startswith('From '):
            class SimpleMessage:
                def __init__(self, fp):
                    self.guts = fp.read()
            contentFile = StringIO.StringIO(content)
            mbox = mailbox.PortableUnixMailbox(contentFile, SimpleMessage)
            return map(lambda m: m.guts, mbox)
        else:
            return [content]
    def _doSave(self):
        """Saves the database."""
        self.write("<b>" + _("Saving..."))
        self.flush()
        self.classifier.store()
        self.write(_("Done.") + "</b>\n")
    def onSave(self, how):
        """Command handler for "Save" and "Save & shutdown"."""
        isShutdown = how.lower().find('shutdown') >= 0
        self._writePreamble(_("Save"), showImage=(not isShutdown))
        self._doSave()
        if isShutdown:
            self.write("<p>%s</p>" % self.html.shutdownMessage)
            self.write("</div></body></html>")
            self.flush()
            self.close()
            raise SystemExit
        self._writePostamble()
    def _buildClassifyBox(self):
        """Returns a "Classify a message" box.  This is used on both the Home
        page and the classify results page.  The Classify form is based on the
        Upload form."""
        form = self.html.upload.clone()
        del form.or_mbox
        del form.submit_spam
        del form.submit_ham
        form.action = "classify"
        return self._buildBox(_("Classify a message"), 'classify.gif', form)
    def _buildTrainBox(self):
        """Returns a "Train on a given message" box.  This is used on both
        the Home page and the training results page.  The Train form is
        based on the Upload form."""
        form = self.html.upload.clone()
        del form.submit_classify
        return self._buildBox(_("Train on a message, mbox file or dbx file"),
                              'message.gif', form)
    def reReadOptions(self):
        """Called by the config page when the user saves some new options,
        or restores the defaults."""
        pass
    def onExperimentalconfig(self):
        html = self._buildConfigPage(experimental_ini_map)
        html.title = _('Home &gt; Experimental Configuration')
        html.pagename = _('&gt; Experimental Configuration')
        html.adv_button.name.value = _("Back to basic configuration")
        html.adv_button.action = "config"
        html.config_submit.value = _("Save experimental options")
        html.restore.value = _("Restore experimental options defaults (all off)")
        del html.exp_button
        self.writeOKHeaders('text/html')
        self.write(html)
    def onAdvancedconfig(self):
        html = self._buildConfigPage(self.advanced_options_map)
        html.title = _('Home &gt; Advanced Configuration')
        html.pagename = _('&gt; Advanced Configuration')
        html.adv_button.name.value = _("Back to basic configuration")
        html.adv_button.action = "config"
        html.config_submit.value = _("Save advanced options")
        html.restore.value = _("Restore advanced options defaults")
        del html.exp_button
        self.writeOKHeaders('text/html')
        self.write(html)
    def onConfig(self):
        html = self._buildConfigPage(self.parm_ini_map)
        html.title = _('Home &gt; Configure')
        html.pagename = _('&gt; Configure')
        self.writeOKHeaders('text/html')
        self.write(html)
    def _buildConfigPage(self, parm_map):
        html = self._getHTMLClone()
        html.shutdownTableCell = "&nbsp;"
        html.mainContent = self.html.configForm.clone()
        html.mainContent.configFormContent = ""
        html.mainContent.optionsPathname = cgi.escape(optionsPathname)
        return self._buildConfigPageBody(html, parm_map)
    def _buildConfigPageBody(self, html, parm_map):
        configTable = None
        section = None
        for sect, opt in parm_map:
            if opt is None:
                if configTable is not None and section is not None:
                    section.boxContent = configTable
                    html.configFormContent += section
                section = self.html.headedBox.clone()
                configTable = self.html.configTable.clone()
                configTextRow1 = configTable.configTextRow1.clone()
                configTextRow2 = configTable.configTextRow2.clone()
                configCbRow1 = configTable.configCbRow1.clone()
                configRow2 = configTable.configRow2.clone()
                blankRow = configTable.blankRow.clone()
                del configTable.configTextRow1
                del configTable.configTextRow2
                del configTable.configCbRow1
                del configTable.configRow2
                del configTable.blankRow
                del configTable.folderRow
                section.heading = sect
                del section.iconCell
                continue
            html_key = sect + '_' + opt
            if sect == "Headers" and opt in ("notate_to", "notate_subject"):
                valid_input = (options["Headers", "header_ham_string"],
                               options["Headers", "header_spam_string"],
                               options["Headers", "header_unsure_string"])
            else:
                valid_input = options.valid_input(sect, opt)
            if isinstance(valid_input, types.StringTypes):
                newConfigRow1 = configTextRow1.clone()
                newConfigRow1.label = options.display_name(sect, opt)
                newConfigRow1.input.name = html_key
                newConfigRow1.input.value = options.unconvert(sect, opt)
            else:
                newConfigRow1 = configCbRow1.clone()
                newConfigRow1.label = options.display_name(sect, opt)
                blankOption = newConfigRow1.input.clone()
                firstOpt = True
                i = 0
                for val in valid_input:
                    newOption = blankOption.clone()
                    if options.multiple_values_allowed(sect, opt):
                        if val in options[sect, opt]:
                            newOption.input_box.checked = "checked"
                        newOption.input_box.type = "checkbox"
                        newOption.input_box.name = html_key + '-' + str(i)
                        i += 1
                    else:
                        if val == options[sect, opt]:
                            newOption.input_box.checked = "checked"
                        newOption.input_box.type = "radio"
                        newOption.input_box.name = html_key
                    if options.is_boolean(sect, opt):
                        if val is True:
                            val = "Yes"
                        elif val is False:
                            val = "No"
                    newOption.val_label = str(val)
                    newOption.input_box.value = str(val)
                    if firstOpt:
                        newConfigRow1.input = newOption
                        firstOpt = False
                    else:
                        newConfigRow1.input += newOption
            newConfigRow1.helpCell = '<strong>' + \
                                     options.display_name(sect, opt) + \
                                     ':</strong> ' + \
                                     cgi.escape(options.doc(sect, opt))
            newConfigRow2 = configRow2.clone()
            currentValue = options[sect, opt]
            if type(currentValue) in types.StringTypes:
                currentValue = currentValue.replace(',', ', ')
                newConfigRow2 = configTextRow2.clone()
            else:
                currentValue = options.unconvert(sect, opt)
                newConfigRow2 = configRow2.clone()
            if options.is_boolean(sect, opt):
                if currentValue == "False":
                    currentValue = _("No")
                elif currentValue == "True":
                    currentValue = _("Yes")
            newConfigRow2.currentValue = currentValue
            configTable += newConfigRow1 + newConfigRow2 + blankRow
        if section is not None:
            section.boxContent = configTable
            html.configFormContent += section
        return html
    def onChangeopts(self, **parms):
        pmap = self.parm_ini_map
        if parms.has_key("how"):
            if parms["how"] == _("Save advanced options"):
                pmap = self.advanced_options_map
            elif parms["how"] == _("Save experimental options"):
                pmap = experimental_ini_map
            elif parms["how"] == _("Save plugin options"):
                pmap = self.plugin_ini_map
            del parms["how"]
        html = self._getHTMLClone()
        html.shutdownTableCell = "&nbsp;"
        html.mainContent = self.html.headedBox.clone()
        errmsg = self.verifyInput(parms, pmap)
        if errmsg != '':
            html.mainContent.heading = _("Errors Detected")
            html.mainContent.boxContent = errmsg
            html.title = _('Home &gt; Error')
            html.pagename = _('&gt; Error')
            self.writeOKHeaders('text/html')
            self.write(html)
            return
        old_database_type = options["Storage", "persistent_use_database"]
        old_name = options["Storage", "persistent_storage_file"]
        for name, value in parms.items():
            sect, opt = name.split('_', 1)
            if (sect, opt) in pmap:
                options.set(sect, opt, value)
            else:
                sect2, opt = opt.split('_', 1)
                sect += '_' + sect2
                options.set(sect, opt, value)
        options.update_file(optionsPathname)
        if options["Storage", "persistent_use_database"] != \
           old_database_type and os.path.exists(old_name):
            new_name = options["Storage", "persistent_storage_file"]
            new_type = options["Storage", "persistent_use_database"]
            self.close_database()
            try:
                os.remove(new_name + ".tmp")
            except OSError:
                pass
            storage.convert(old_name, old_database_type,
                            new_name + ".tmp", new_type)
            if os.path.exists(new_name):
                try:
                    os.remove(new_name + ".old")
                except OSError:
                    pass
                os.rename(new_name, new_name + ".old")
            os.rename(new_name + ".tmp", new_name)
            if os.path.exists(options["Storage",
                                      "messageinfo_storage_file"]):
                try:
                    os.remove(options["Storage",
                                      "messageinfo_storage_file"] + ".old")
                except OSError:
                    pass
                os.rename(options["Storage", "messageinfo_storage_file"],
                          options["Storage",
                                  "messageinfo_storage_file"] + ".old")
        self.reReadOptions()
        html.mainContent.heading = _("Options Changed")
        html.mainContent.boxContent = _("Options changed.  Return " \
                                        "<a href='home'>Home</a>.")
        html.title = _('Home &gt; Options Changed')
        html.pagename = _('&gt; Options Changed')
        self.writeOKHeaders('text/html')
        self.write(html)
    def onRestoredefaults(self, how):
        if how == _("Restore advanced options defaults"):
            self.restoreConfigDefaults(self.advanced_options_map)
        elif how == _("Restore experimental options defaults (all off)"):
            self.restoreConfigDefaults(experimental_ini_map)
        else:
            self.restoreConfigDefaults(self.parm_ini_map)
        self.reReadOptions()
        html = self._getHTMLClone()
        html.shutdownTableCell = "&nbsp;"
        html.mainContent = self.html.headedBox.clone()
        html.mainContent.heading = _("Option Defaults Restored")
        html.mainContent.boxContent = _("Defaults restored.  Return " \
                                        "<a href='home'>Home</a>.")
        html.title = _('Home &gt; Defaults Restored')
        html.pagename = _('&gt; Defaults Restored')
        self.writeOKHeaders('text/html')
        self.write(html)
        self.reReadOptions()
    def verifyInput(self, parms, pmap):
        '''Check that the given input is valid.'''
        errmsg = ''
        for name, value in parms.items():
            if name[-2:-1] == '-':
                if parms.has_key(name[:-2]):
                    parms[name[:-2]] += (value,)
                else:
                    parms[name[:-2]] = (value,)
                del parms[name]
        for sect, opt in pmap:
            if opt is None:
                nice_section_name = sect
                continue
            if sect == "Headers" and opt in ("notate_to", "notate_subject"):
                valid_input = (options["Headers", "header_ham_string"],
                               options["Headers", "header_spam_string"],
                               options["Headers", "header_unsure_string"])
            else:
                valid_input = options.valid_input(sect, opt)
            html_key = sect + '_' + opt
            if not parms.has_key(html_key):
                value = ()
                entered_value = "None"
            else:
                value = parms[html_key]
                entered_value = value
                if options.is_boolean(sect, opt):
                    if value == _("No"):
                        value = False
                    elif value == _("Yes"):
                        value = True
                if options.multiple_values_allowed(sect, opt) and \
                   value == "":
                    value = ()
                value = options.convert(sect, opt, value)
            if not options.is_valid(sect, opt, value):
                errmsg += _('<li>\'%s\' is not a value valid for [%s] %s') % \
                          (entered_value, nice_section_name,
                           options.display_name(sect, opt))
                if isinstance(valid_input, types.TupleType):
                    errmsg += _('. Valid values are: ')
                    for valid in valid_input:
                        errmsg += str(valid) + ','
                    errmsg = errmsg[:-1] # cut last ','
                errmsg += '</li>'
            parms[html_key] = value
        return errmsg
    def restoreConfigDefaults(self, parm_map):
        d = OptionsClass()
        d.load_defaults(defaults)
        for section, option in parm_map:
            if option is not None:
                if not options.no_restore(section, option):
                    options.set(section, option, d.get(section,option))
        options.update_file(optionsPathname)
    def onHelp(self, topic=None):
        """Provide a help page, either the default if topic is not
        supplied, or specific to the topic given."""
        self._writePreamble(_("Help"))
        helppage = self.html.helppage.clone()
        if topic:
            headerelem_name = "helpheader_" + topic
            textelem_name = "helptext_" + topic
            try:
                helppage.helpheader = self.html[headerelem_name]._content
                helppage.helptext = self.html[textelem_name]._content % \
                    { "cache_expiry_days": options["Storage", "cache_expiry_days"] }
            except KeyError:
                pass
        self.write(helppage)
        self._writePostamble()
    def onStats(self):
        """Provide statistics about previous SpamBayes activity."""
        self._writePreamble(_("Statistics"))
        if self.stats:
            stats = self.stats.GetStats(use_html=True)
            stats = self._buildBox(_("Statistics"), None,
                                   "<br/><br/>".join(stats))
        else:
            stats = self._buildBox(_("Statistics"), None,
                                   _("Statistics not available"))
        self.write(stats)
        self._writePostamble(help_topic="stats")
    def onBugreport(self):
        """Create a message to post to spambayes@python.org that hopefully
        has enough information for us to help this person with their
        problem."""
        self._writePreamble(_("Send Help Message"), ("help", _("Help")))
        report = self.html.bugreport.clone()
        v = Version.get_current_version()
        sb_ver = v.get_long_version(self.app_for_version)
        if hasattr(sys, "frozen"):
            sb_type = "binary"
        else:
            sb_type = "source"
        py_ver = sys.version
        try:
            os_name = "Windows %d.%d.%d.%d (%s)" % sys.getwindowsversion()
        except AttributeError:
            os_name = os.name
        report.message_body = "I am using %s (%s), with version %s of " \
                              "Python; my operating system is %s.  I have " \
                              "trained %d ham and %d spam.\n\nThe problem " \
                              "I am having is [DESCRIBE YOUR PROBLEM HERE] " \
                              % (sb_ver, sb_type, py_ver, os_name,
                                 self.classifier.nham, self.classifier.nspam)
        remote_servers = options["pop3proxy", "remote_servers"]
        if remote_servers:
            domain_guess = remote_servers[0]
            for pre in ["pop.", "pop3.", "mail.",]:
                if domain_guess.startswith(pre):
                    domain_guess = domain_guess[len(pre):]
        else:
            domain_guess = "[YOUR ISP]"
        report.from_addr.value = "[YOUR EMAIL ADDRESS]@%s" % (domain_guess,)
        report.subject.value = "Problem with %s: [PROBLEM SUMMARY]" % \
                               (self.app_for_version,)
        try:
            import win32api
        except ImportError:
            pass
        else:
            if hasattr(sys, "frozen"):
                temp_dir = win32api.GetTempPath()
                for name in ["SpamBayesService", "SpamBayesServer",]:
                    for i in xrange(3):
                        pn = os.path.join(temp_dir, "%s%d.log" % (name,
                                                                  (i+1)))
                        if os.path.exists(pn):
                            report.file.type = "text"
                            report.file.value = pn
                            break
                    if report.file.value:
                        break
            try:
                smtp_server = options["smtpproxy", "remote_servers"][0]
            except IndexError:
                smtp_server = None
            if not smtp_server:
                self.write(self._buildBox(_("Warning"), "status.gif",
                           _("You will be unable to send this message from " \
                           "this page, as you do not have your SMTP " \
                           "server's details entered in your configuration. " \
                           "Please either <a href='config'>enter those " \
                           "details</a>, or copy the text below into your " \
                           "regular mail application.")))
                del report.submitrow
        self.write(report)
        self._writePostamble()
    def onSubmitreport(self, from_addr, message, subject, attach):
        """Send the help message/bug report to the specified address."""
        import mimetypes
        from email import Encoders
        from email.MIMEBase import MIMEBase
        from email.MIMEAudio import MIMEAudio
        from email.MIMEMultipart import MIMEMultipart
        from email.MIMEImage import MIMEImage
        from email.MIMEText import MIMEText
        if not self._verifyEnteredDetails(from_addr, subject, message):
            self._writePreamble(_("Error"), ("help", _("Help")))
            self.write(self._buildBox(_("Error"), "status.gif",
                                      _("You must fill in the details that " \
                                      "describe your specific problem " \
                                      "before you can send the message.")))
        else:
            self._writePreamble(_("Sent"), ("help", _("Help")))
            mailer = smtplib.SMTP(options["smtpproxy", "remote_servers"][0])
            outer = MIMEMultipart()
            outer['Subject'] = subject
            outer['To'] = '"SpamBayes Mailing List" <spambayes@python.org>'
            outer['CC'] = from_addr
            outer['From'] = from_addr
            v = Version.get_current_version()
            outer['X-Mailer'] = v.get_long_version(self.app_for_version)
            outer.preamble = self._wrap(message)
            outer.epilogue = ''
            try:
                ctype, encoding = mimetypes.guess_type(attach)
                if ctype is None or encoding is not None:
                    ctype = 'application/octet-stream'
                maintype, subtype = ctype.split('/', 1)
                if maintype == 'text':
                    fp = open(attach)
                    msg = MIMEText(fp.read(), _subtype=subtype)
                    fp.close()
                elif maintype == 'image':
                    fp = open(attach, 'rb')
                    msg = MIMEImage(fp.read(), _subtype=subtype)
                    fp.close()
                elif maintype == 'audio':
                    fp = open(attach, 'rb')
                    msg = MIMEAudio(fp.read(), _subtype=subtype)
                    fp.close()
                else:
                    fp = open(attach, 'rb')
                    msg = MIMEBase(maintype, subtype)
                    msg.set_payload(fp.read())
                    fp.close()
                    Encoders.encode_base64(msg)
            except IOError:
                pass
            else:
                msg.add_header('Content-Disposition', 'attachment',
                               filename=os.path.basename(attach))
                outer.attach(msg)
            msg = MIMEText(self._wrap(message))
            outer.attach(msg)
            recips = []
            for r in ["spambayes@python.org", from_addr]:
                if r:
                    recips.append(r)
            mailer.sendmail(from_addr, recips, outer.as_string())
            self.write(_("Sent message.  Please do not send again, or " \
                       "refresh this page!"))
        self._writePostamble()
    def _verifyEnteredDetails(self, from_addr, subject, message):
        """Ensure that the user didn't just send the form message, and
        at least changed the fields."""
        if from_addr.startswith(_("[YOUR EMAIL ADDRESS]")):
            return False
        if message.endswith(_("[DESCRIBE YOUR PROBLEM HERE]")):
            return False
        if subject.endswith(_("[PROBLEM SUMMARY]")):
            return False
        return True
    def _wrap(self, text, width=70):
        """Wrap the text into lines no bigger than the specified width."""
        try:
            from textwrap import fill
        except ImportError:
            pass
        else:
            return "\n".join([fill(paragraph, width) \
                              for paragraph in text.split('\n')])
        def fill(text, width):
            if len(text) <= width:
                return text
            wordsep_re = re.compile(r'(-*\w{2,}-(?=\w{2,})|'   # hyphenated words
                                    r'(?<=\S)-{2,}(?=\w))')    # em-dash
            chunks = wordsep_re.split(text)
            chunks = filter(None, chunks)
            return '\n'.join(self._wrap_chunks(chunks, width))
        return "\n".join([fill(paragraph, width) \
                          for paragraph in text.split('\n')])
    def _wrap_chunks(self, chunks, width):
        """Stolen from textwrap; see that module in Python >= 2.3 for
        details."""
        lines = []
        while chunks:
            cur_line = []
            cur_len = 0
            if chunks[0].strip() == '' and lines:
                del chunks[0]
            while chunks:
                l = len(chunks[0])
                if cur_len + l <= width:
                    cur_line.append(chunks.pop(0))
                    cur_len += l
                else:
                    break
            if chunks and len(chunks[0]) > width:
                space_left = width - cur_len
                cur_line.append(chunks[0][0:space_left])
                chunks[0] = chunks[0][space_left:]
            if cur_line and cur_line[-1].strip() == '':
                del cur_line[-1]
            if cur_line:
                lines.append(''.join(cur_line))
        return lines
    def _keyToTimestamp(self, key):
        """Given a message key (as seen in a Corpus), returns the timestamp
        for that message.  This is the time that the message was received,
        not the Date header."""
        return long(key[:10])
    def _getTimeRange(self, timestamp):
        """Given a unix timestamp, returns a 3-tuple: the start timestamp
        of the given day, the end timestamp of the given day, and the
        formatted date of the given day."""
        this = time.localtime(timestamp)
        start = (this[0], this[1], this[2], 0, 0, 0, this[6], this[7], this[8])
        end = time.localtime(time.mktime(start) + 36*60*60)
        end = (end[0], end[1], end[2], 0, 0, 0, end[6], end[7], end[8])
        date = time.strftime("%A, %B %d, %Y", start)
        return time.mktime(start), time.mktime(end), date
    def _sortMessages(self, messages, sort_order, reverse=False):
        """Sorts the message by the appropriate attribute.  If this was the
        previous sort order, then reverse it."""
        if sort_order is None or sort_order == "received":
            messages.sort()
            if self.previous_sort == sort_order:
                messages.reverse()
                self.previous_sort = None
            else:
                self.previous_sort = 'received'
            return messages
        tmplist = [(getattr(x[1], sort_order), x) for x in messages]
        tmplist.sort()
        if reverse:
            tmplist.reverse()
        return [x for (key, x) in tmplist]
    def _appendMessages(self, table, keyedMessageInfo, label, sort_order,
                        reverse=False):
        """Appends the rows of a table of messages to 'table'."""
        stripe = 0
        keyedMessageInfo = self._sortMessages(keyedMessageInfo, sort_order,
                                              reverse)
        nrows = options["html_ui", "rows_per_section"]
        for key, messageInfo in keyedMessageInfo[:nrows]:
            unused, unused, messageInfo.received = \
                    self._getTimeRange(self._keyToTimestamp(key))
            row = self.html.reviewRow.clone()
            try:
                score = messageInfo.score
            except ValueError:
                score = None
            if label == _('Spam'):
                if score is not None \
                   and score > options["html_ui", "spam_discard_level"]:
                    r_att = getattr(row, 'discard')
                else:
                    r_att = getattr(row, options["html_ui",
                                           "default_spam_action"])
            elif label == _('Ham'):
                if score is not None \
                   and score < options["html_ui", "ham_discard_level"]:
                    r_att = getattr(row, 'discard')
                else:
                    r_att = getattr(row, options["html_ui",
                                           "default_ham_action"])
            else:
                r_att = getattr(row, options["html_ui",
                                           "default_unsure_action"])
            setattr(r_att, "checked", 1)
            row.optionalHeadersValues = '' # make way for real list
            for header in options["html_ui", "display_headers"]:
                header = header.lower()
                text = getattr(messageInfo, "%sHeader" % (header,))
                if header == "subject":
                    h = self.html.reviewRow.linkedHeaderValue.clone()
                    h.text.title = messageInfo.bodySummary
                    h.text.href = "view?key=%s&corpus=%s" % (key, label)
                else:
                    h = self.html.reviewRow.headerValue.clone()
                h.text = text
                row.optionalHeadersValues += h
            if options["html_ui", "display_score"]:
                if isinstance(messageInfo.score, types.StringTypes):
                    row.score_ = messageInfo.score
                else:
                    row.score_ = "%.2f%%" % (messageInfo.score,)
            else:
                del row.score_
            if options["html_ui", "display_received_time"]:
                row.received_ = messageInfo.received
            else:
                del row.received_
            subj_list = []
            for c in messageInfo.subjectHeader:
                subj_list.append("%%%s" % (hex(ord(c))[2:],))
            subj = "".join(subj_list)
            row.classify.href = "showclues?key=%s&subject=%s" % (key, subj)
            row.tokens.href = ("showclues?key=%s&subject=%s&tokens=1" %
                               (key, subj))
            setattr(row, 'class', ['stripe_on', 'stripe_off'][stripe]) # Grr!
            setattr(row, 'onMouseOut',
                    ["this.className='stripe_on';",
                     "this.className='stripe_off';"][stripe])
            row = str(row).replace('TYPE', label).replace('KEY', key)
            table += row
            stripe = stripe ^ 1
    def _contains(self, a, b, ignore_case=False):
        """Return true if substring b is part of string a."""
        assert isinstance(a, types.StringTypes)
        assert isinstance(b, types.StringTypes)
        if ignore_case:
            a = a.lower()
            b = b.lower()
        return a.find(b) >= 0
    def _makeMessageInfo(self, message):
        """Given an email.Message, return an object with subjectHeader,
        bodySummary and other header (as needed) attributes.  These objects
        are passed into appendMessages by onReview - passing email.Message
        objects directly uses too much memory.
        """
        message.delNotations()
        subjectHeader = message["Subject"] or "(none)"
        headers = {"subject" : subjectHeader}
        for header in options["html_ui", "display_headers"]:
            headers[header.lower()] = (message[header] or "(none)")
        score = message[options["Headers", "score_header_name"]]
        if score:
            op = score.find('(')
            if op >= 0:
                score = score[:op]
            try:
                score = float(score) * 100
            except ValueError:
                score = "Err"  # Let the user know something is wrong.
        else:
            score = "?"
        try:
            part = typed_subpart_iterator(message, 'text', 'plain').next()
            text = part.get_payload()
        except StopIteration:
            try:
                part = typed_subpart_iterator(message, 'text', 'html').next()
                text = part.get_payload()
                text, unused = tokenizer.crack_html_style(text)
                text, unused = tokenizer.crack_html_comment(text)
                text = tokenizer.html_re.sub(' ', text)
                text = _('(this message only has an HTML body)\n') + text
            except StopIteration:
                text = _('(this message has no text body)')
        if type(text) == type([]):  # gotta be a 'right' way to do this
            text = _("(this message is a digest of %s messages)") % (len(text))
        elif text is None:
            text = _("(this message has no body)")
        else:
            text = text.replace('&nbsp;', ' ')      # Else they'll be quoted
            text = re.sub(r'(\s)\s+', r'\1', text)  # Eg. multiple blank lines
            text = text.strip()
        class _MessageInfo:
            pass
        messageInfo = _MessageInfo()
        for headerName, headerValue in headers.items():
            headerValue = self._trimHeader(headerValue, 45, True)
            setattr(messageInfo, "%sHeader" % (headerName,), headerValue)
        messageInfo.score = score
        messageInfo.bodySummary = self._trimHeader(text, 200)
        return messageInfo
