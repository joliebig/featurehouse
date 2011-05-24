"""IMAPFilter Web Interface
Classes:
    IMAPUserInterface - Interface class for the IMAP filter
Abstract:
This module implements a browser based Spambayes user interface for the
IMAP filter.  Users may use it to interface with the filter - it is
expected that this will primarily be for configuration, although users
may also wish to look up words in the database, or classify a message.
The following functions are currently included:
[From the base class UserInterface]
    onClassify - classify a given message
    onWordquery - query a word from the database
    onTrain - train a message or mbox
    onSave - save the database and possibly shutdown
[Here]
    onHome - a home page with various options
To do:
 o This could have a neat review page, like pop3proxy, built up by
   asking the IMAP server appropriate questions.  I don't know whether
   this is needed, however.  This would then allow viewing a message,
   showing the clues for it, and so on.  Finding a message (via the
   spambayes id) could also be done.
 o Suggestions?
"""
__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>, Tim Stone"
__credits__ = "All the Spambayes folk."
import cgi
from spambayes import UserInterface
from spambayes.Options import options, optionsPathname, _
parm_map = (
    (_('IMAP Options'),       None),
    ('imap',                  'server'),
    ('imap',                  'username'),
    ('imap',                  'password'),
    ('imap',                  'use_ssl'),
    (_('Header Options'),     None),
    ('Headers',               'notate_to'),
    ('Headers',               'notate_subject'),
    (_('Storage Options'),    None),
    ('Storage',               'persistent_storage_file'),
    ('Storage',               'messageinfo_storage_file'),
    (_('Statistics Options'), None),
    ('Categorization',        'ham_cutoff'),
    ('Categorization',        'spam_cutoff'),
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
    (_('Tokenising Options'), None),
    ('Tokenizer',             'mine_received_headers'),
    ('Tokenizer',             'replace_nonascii_chars'),
    ('Tokenizer',             'summarize_email_prefixes'),
    ('Tokenizer',             'summarize_email_suffixes'),
    ('Tokenizer',             'x-pick_apart_urls'),
    (_('Interface Options'),  None),
    ('html_ui',               'display_adv_find'),
    ('html_ui',               'allow_remote_connections'),
    ('html_ui',               'http_authentication'),
    ('html_ui',               'http_user_name'),
    ('html_ui',               'http_password'),
    ('globals',               'language'),
)
class LoginFailure(Exception):
    """Login to the IMAP server failed."""
    def __init__(self, details):
        self.details = details
    def __str__(self):
        return "Login failure: %s" % (self.details,)
class IMAPUserInterface(UserInterface.UserInterface):
    """Serves the HTML user interface for the proxies."""
    def __init__(self, cls, imaps, pwds, imap_session_class,
                 lang_manager=None, stats=None,
                 close_db=None, change_db=None):
        global parm_map
        try:
            from imaplib import IMAP4_SSL
        except ImportError:
            parm_list = list(parm_map)
            parm_list.remove(("imap", "use_ssl"))
            parm_map = tuple(parm_list)
        else:
            del IMAP4_SSL
        UserInterface.UserInterface.__init__(self, cls, parm_map, adv_map,
                                             lang_manager, stats)
        self.classifier = cls
        self.imaps = imaps
        self.imap_pwds = pwds
        self.app_for_version = "SpamBayes IMAP Filter"
        self.imap_session_class = imap_session_class
        self.close_database = close_db
        self.change_db = change_db
    def onHome(self):
        """Serve up the homepage."""
        stateDict = self.classifier.__dict__.copy()
        stateDict["warning"] = ""
        stateDict.update(self.classifier.__dict__)
        statusTable = self.html.statusTable.clone()
        del statusTable.proxyDetails
        statusTable.configurationLink += "<br />&nbsp;&nbsp;&nbsp;&nbsp;" \
            "&nbsp;" + _("You can also <a href='filterfolders'>configure" \
                         " folders to filter</a><br />and " \
                         "<a href='trainingfolders'>Configure folders to" \
                         " train</a>")
        findBox = self._buildBox(_('Word query'), 'query.gif',
                                 self.html.wordQuery)
        if not options["html_ui", "display_adv_find"]:
            del findBox.advanced
        content = (self._buildBox(_('Status and Configuration'),
                                  'status.gif', statusTable % stateDict)+
                   self._buildTrainBox() +
                   self._buildClassifyBox() +
                   findBox
                   )
        self._writePreamble(_("Home"))
        self.write(content)
        self._writePostamble()
    def reReadOptions(self):
        """Called by the config page when the user saves some new options, or
        restores the defaults."""
        import Options
        Options.load_options()
        global options
        from Options import options
        self.change_db()
    def onSave(self, how):
        for imap in self.imaps:
            if imap:
                imap.logout()
        UserInterface.UserInterface.onSave(self, how)
    def onFilterfolders(self):
        self._writePreamble(_("Select Filter Folders"))
        self._login_to_imap()
        available_folders = []
        for imap in self.imaps:
            if imap and imap.logged_in:
                available_folders.extend(imap.folder_list())
        if not available_folders:
            content = self._buildBox(_("Error"), None,
                                     _("No folders available"))
            self.write(content)
            self._writePostamble()
            return
        content = self.html.configForm.clone()
        content.configFormContent = ""
        content.introduction = _("This page allows you to change " \
                                 "which folders are filtered, and " \
                                 "where filtered mail ends up.")
        content.config_submit.value = _("Save Filter Folders")
        content.optionsPathname = optionsPathname
        for opt in ("unsure_folder", "spam_folder",
                    "filter_folders"):
            folderBox = self._buildFolderBox("imap", opt, available_folders)
            content.configFormContent += folderBox
        self.write(content)
        self._writePostamble()
    def _login_to_imap(self):
        new_imaps = []
        for i in xrange(len(self.imaps)):
            imap = self.imaps[i]
            imap_logged_in = self._login_to_imap_server(imap, i)
            if imap_logged_in:
                new_imaps.append(imap_logged_in)
        self.imaps = new_imaps
    def _login_to_imap_server(self, imap, i):
        if imap and imap.logged_in:
            return imap
        if imap is None or not imap.connected:
            try:
                server = options["imap", "server"][i]
            except KeyError:
                content = self._buildBox(_("Error"), None,
                                         _("Please check server/port details."))
                self.write(content)
                return None
            if server.find(':') > -1:
                server, port = server.split(':', 1)
                port = int(port)
            else:
                if options["imap", "use_ssl"]:
                    port = 993
                else:
                    port = 143
            imap = self.imap_session_class(server, port)
            if not imap.connected:
                content = self._buildBox(_("Error"), None,
                                         _("Please check server/port details."))
                self.write(content)
                return None
        usernames = options["imap", "username"]
        if not usernames:
            content = self._buildBox(_("Error"), None,
                                     _("Must specify username first."))
            self.write(content)
            return None
        if not self.imap_pwds:
            self.imap_pwd = options["imap", "password"]
        if not self.imap_pwds:
            content = self._buildBox(_("Error"), None,
                                     _("Must specify password first."))
            self.write(content)
            return None
        try:
            imap.login(usernames[i], self.imap_pwds[i])
        except KeyError:
            content = self._buildBox(_("Error"), None,
                                     _("Please check username/password details."))
            self.write(content)
            return None
        except LoginFailure, e:
            content = self._buildBox(_("Error"), None, str(e))
            self.write(content)
            return None
        return imap
    def onTrainingfolders(self):
        self._writePreamble(_("Select Training Folders"))
        self._login_to_imap()
        available_folders = []
        for imap in self.imaps:
            if imap and imap.logged_in:
                available_folders.extend(imap.folder_list())
        if not available_folders:
            content = self._buildBox(_("Error"), None,
                                     _("No folders available"))
            self.write(content)
            self._writePostamble()
            return
        content = self.html.configForm.clone()
        content.configFormContent = ""
        content.introduction = _("This page allows you to change " \
                                 "which folders contain mail to " \
                                 "train Spambayes.")
        content.config_submit.value = _("Save Training Folders")
        content.optionsPathname = optionsPathname
        for opt in ("ham_train_folders",
                    "spam_train_folders"):
            folderBox = self._buildFolderBox("imap", opt, available_folders)
            content.configFormContent += folderBox
        self.write(content)
        self._writePostamble()
    def onChangeopts(self, **parms):
        backup = self.parm_ini_map
        if parms["how"] == _("Save Training Folders") or \
           parms["how"] == _("Save Filter Folders"):
            del parms["how"]
            self.parm_ini_map = ()
            for opt, value in parms.items():
                del parms[opt]
                if opt[-len(value):] == value:
                    opt = opt[:-len(value)]
                self.parm_ini_map += ("imap", opt),
                key = "imap_" + opt
                if parms.has_key(key):
                    parms[key] += ',' + value
                else:
                    parms[key] = value
        UserInterface.UserInterface.onChangeopts(self, **parms)
        self.parm_ini_map = backup
    def _buildFolderBox(self, section, option, available_folders):
        folderTable = self.html.configTable.clone()
        del folderTable.configTextRow1
        del folderTable.configTextRow2
        del folderTable.configCbRow1
        del folderTable.configRow2
        del folderTable.blankRow
        del folderTable.folderRow
        firstRow = True
        for folder in available_folders:
            folder = cgi.escape(folder)
            folderRow = self.html.configTable.folderRow.clone()
            if firstRow:
                folderRow.helpCell = options.doc(section, option)
                firstRow = False
            else:
                del folderRow.helpCell
            folderRow.folderBox.name = option
            folderRow.folderBox.value = folder
            folderRow.folderName = folder
            if options.multiple_values_allowed(section, option):
                if folder in options[section, option]:
                    folderRow.folderBox.checked = "checked"
                folderRow.folderBox.name += folder
            else:
                if folder == options[section, option]:
                    folderRow.folderBox.checked = "checked"
                folderRow.folderBox.type = "radio"
            folderTable += folderRow
        return self._buildBox(options.display_name(section, option),
                              None, folderTable)
