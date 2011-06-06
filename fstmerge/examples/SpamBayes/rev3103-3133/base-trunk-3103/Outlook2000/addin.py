import sys, os
import types
import warnings
import traceback
import _winreg
from types import UnicodeType
import encodings
try:
    True, False
except NameError:
    True, False = 1, 0
import locale
locale.setlocale(locale.LC_NUMERIC, "C")
from win32com import universal
from win32com.server.exception import COMException
from win32com.client import gencache, DispatchWithEvents, Dispatch
import win32api
import pythoncom
from win32com.client import constants, getevents
import win32gui, win32con, win32clipboard # for button images!
import timer, thread
from dialogs.dlgutils import SetWaitCursor
toolbar_name = "SpamBayes"
ADDIN_DISPLAY_NAME = "SpamBayes Outlook Addin"
try:
    win32api.GetConsoleTitle()
except win32api.error:
    if hasattr(sys, "frozen"):
        temp_dir = win32api.GetTempPath()
        for i in range(3,0,-1):
            try: os.unlink(os.path.join(temp_dir, "spambayes%d.log" % (i+1)))
            except os.error: pass
            try:
                os.rename(
                    os.path.join(temp_dir, "spambayes%d.log" % i),
                    os.path.join(temp_dir, "spambayes%d.log" % (i+1))
                    )
            except os.error: pass
        sys.stdout = open(os.path.join(temp_dir,"spambayes1.log"), "wt", 0)
        sys.stderr = sys.stdout
    else:
        import win32traceutil
bValidateGencache = not hasattr(sys, "frozen")
gencache.EnsureModule('{00062FFF-0000-0000-C000-000000000046}', 0, 9, 0,
                        bForDemand=True, bValidateFile=bValidateGencache) # Outlook 9
gencache.EnsureModule('{2DF8D04C-5BFA-101B-BDE5-00AA0044DE52}', 0, 2, 1,
                        bForDemand=True, bValidateFile=bValidateGencache) # Office 9
gencache.EnsureModule('{AC0714F2-3D04-11D1-AE7D-00A0C90F26F4}', 0, 1, 0,
                        bForDemand=True, bValidateFile=bValidateGencache)
universal.RegisterInterfaces('{AC0714F2-3D04-11D1-AE7D-00A0C90F26F4}', 0, 1, 0,
                             ["_IDTExtensibility2"])
try:
    from win32com.client import CastTo, WithEvents
except ImportError:
    print "*" * 50
    print "You appear to be running a win32all version pre 151, which is pretty old"
    print "I'm afraid it is time to upgrade"
    raise
def HaveSeenMessage(msgstore_message, manager):
    if msgstore_message.GetField(manager.config.general.field_score_name) is not None:
        return True
    import train
    manager.classifier_data.message_db.load_msg(msgstore_message)
    if train.been_trained_as_ham(msgstore_message) or \
       train.been_trained_as_spam(msgstore_message):
        return True
    return msgstore_message.GetReadState()
def TrainAsHam(msgstore_message, manager, rescore = True, save_db = True):
    import train
    subject = msgstore_message.subject
    print "Training on message '%s' in '%s - " % \
            (subject,
             msgstore_message.GetFolder().GetFQName()),
    if train.train_message(msgstore_message, False, manager.classifier_data):
        print "trained as good"
        if rescore:
            import filter
            filter.filter_message(msgstore_message, manager, all_actions = False)
    else:
        print "already was trained as good"
    manager.classifier_data.message_db.load_msg(msgstore_message)
    assert train.been_trained_as_ham(msgstore_message)
    if save_db:
        manager.classifier_data.SavePostIncrementalTrain()
def TrainAsSpam(msgstore_message, manager, rescore = True, save_db = True):
    import train
    subject = msgstore_message.subject
    print "Training on message '%s' in '%s - " % \
            (subject,
             msgstore_message.GetFolder().GetFQName()),
    if train.train_message(msgstore_message, True, manager.classifier_data):
        print "trained as spam"
        if rescore:
            import filter
            filter.filter_message(msgstore_message, manager, all_actions = False)
    else:
        print "already was trained as spam"
    manager.classifier_data.message_db.load_msg(msgstore_message)
    assert train.been_trained_as_spam(msgstore_message)
    if save_db:
        manager.classifier_data.SavePostIncrementalTrain()
def ProcessMessage(msgstore_message, manager):
    manager.LogDebug(2, "ProcessMessage starting for message '%s'" \
                        % msgstore_message.subject)
    try:
        if not msgstore_message.IsFilterCandidate():
            manager.LogDebug(1, "Skipping message '%s' - we don't filter ones like that!" \
                             % msgstore_message.subject)
            return
        if HaveSeenMessage(msgstore_message, manager):
            if manager.config.training.train_recovered_spam:
                import train
                manager.classifier_data.message_db.load_msg(msgstore_message)
                if train.been_trained_as_spam(msgstore_message):
                    need_train = True
                else:
                    prop = msgstore_message.GetField(manager.config.general.field_score_name)
                    if prop is None:
                        prop = manager.score(msgstore_message)
                    need_train = manager.config.filter.unsure_threshold < prop * 100
                if need_train:
                    TrainAsHam(msgstore_message, manager)
                else:
                    subject = msgstore_message.subject
                    manager.LogDebug(1, "Message '%s' was previously seen, but " \
                                     "did not need to be trained as ham" % subject)
            return
        if manager.config.filter.enabled:
            import filter
            folder_name = msgstore_message.GetFolder().GetFQName()
            disposition = filter.filter_message(msgstore_message, manager)
            print "Message '%s' in '%s' had a Spam classification of '%s'" \
                  % (msgstore_message.GetSubject(),
                     folder_name,
                     disposition)
            manager.HandleNotification(disposition)
        else:
            print "Spam filtering is disabled - ignoring new message"
    except manager.message_store.NotFoundException:
        manager.LogDebug(1, "ProcessMessage had the message moved out from underneath us")
    manager.LogDebug(2, "ProcessMessage finished for", msgstore_message.subject)
class ButtonEvent:
    def Init(self, handler, *args):
        self.handler = handler
        self.args = args
    def Close(self):
        self.handler = self.args = None
    def OnClick(self, button, cancel):
        locale.setlocale(locale.LC_NUMERIC, "C") # see locale comments above
        self.handler(*self.args)
class _BaseItemsEvent:
    def Init(self, target, application, manager):
        self.owner_thread_ident = thread.get_ident() # check we arent multi-threaded
        self.application = application
        self.manager = manager
        self.target = target
        self.use_timer = False
    def ReInit(self):
        pass
    def Close(self):
        self.application = self.manager = self.target = None
        self.close() # the events
class HamFolderItemsEvent(_BaseItemsEvent):
    def Init(self, *args):
        _BaseItemsEvent.Init(self, *args)
        timer_enabled = self.manager.config.filter.timer_enabled
        start_delay = self.manager.config.filter.timer_start_delay
        interval = self.manager.config.filter.timer_interval
        use_timer = timer_enabled and start_delay and interval
        if timer_enabled and not use_timer:
            print "*" * 50
            print "The timer is enabled, but one of the timer intervals values is zero"
            print "You must set both intervals before the timer will enable"
        if use_timer and not hasattr(timer, "__version__"):
            print "*" * 50
            print "SORRY: You have tried to enable the timer, but you have a"
            print "leaky version of the 'timer' module.  These leaks prevent"
            print "Outlook from shutting down.  Please update win32all to post 154"
            print "The timer is NOT enabled..."
            print "*" * 50
            use_timer = False
        if use_timer:
            is_inbox = self.target.IsReceiveFolder()
            if not is_inbox and self.manager.config.filter.timer_only_receive_folders:
                use_timer = False
        if use_timer:
            too = None
            if not isinstance(start_delay, types.FloatType) or \
               not isinstance(interval, types.FloatType):
                print "*" * 50
                print "Timer values are garbage!", repr(start_delay), repr(interval)
                use_timer = False
            elif start_delay < 0.4 or interval < 0.4:
                too = "too often"
            elif start_delay > 60 or interval > 60:
                too = "too infrequently"
            if too:
                print "*" * 50
                print "The timer is configured to fire way " + too + \
                  " (delay=%s seconds, interval=%s seconds)" \
                  % (start_delay, interval)
                print "Please adjust your configuration.  The timer is NOT enabled..."
                print "*" * 50
                use_timer = False
        self.use_timer = use_timer
        self.timer_id = None
    def ReInit(self):
        if self.use_timer:
            self._KillTimer()
        self.Init(self, self.target, self.application, self.manager)
    def Close(self, *args):
        self._KillTimer()
        _BaseItemsEvent.Close(self, *args)
    def _DoStartTimer(self, delay):
        assert thread.get_ident() == self.owner_thread_ident
        assert self.timer_id is None, "Shouldn't start a timer when already have one"
        assert isinstance(delay, types.FloatType), "Timer values are float seconds"
        assert delay, "No delay means no timer!"
        delay = int(delay*1000) # convert to ms.
        self.timer_id = timer.set_timer(delay, self._TimerFunc)
        self.manager.LogDebug(1, "New message timer started - id=%d, delay=%d" % (self.timer_id, delay))
    def _StartTimer(self):
        self._KillTimer()
        delay = self.manager.config.filter.timer_start_delay
        field_name = self.manager.config.general.field_score_name
        self.timer_generator = self.target.GetNewUnscoredMessageGenerator(field_name)
        self._DoStartTimer(delay)
    def _KillTimer(self):
        assert thread.get_ident() == self.owner_thread_ident
        if self.timer_id is not None:
            timer.kill_timer(self.timer_id)
            self.manager.LogDebug(2, "The timer with id=%d was stopped" % self.timer_id)
            self.timer_id = None
    def _TimerFunc(self, event, time):
        assert thread.get_ident() == self.owner_thread_ident
        self.manager.LogDebug(1, "The timer with id=%s fired" % self.timer_id)
        self._KillTimer()
        assert self.timer_generator, "Can't have a timer with no generator"
        locale.setlocale(locale.LC_NUMERIC, "C") # see locale comments above
        try:
            while 1:
                item = self.timer_generator.next()
                try:
                    if not HaveSeenMessage(item, self.manager):
                        break
                except self.manager.message_store.NotFoundException:
                    self.manager.LogDebug(1, "The new message is skipping a message that moved underneath us")
        except StopIteration:
            self.timer_generator = None
            self.manager.LogDebug(1, "The new message timer found no new items, so is stopping")
        else:
            try:
                ProcessMessage(item, self.manager)
            finally:
                delay = self.manager.config.filter.timer_interval
                self._DoStartTimer(delay)
    def OnItemAdd(self, item):
        locale.setlocale(locale.LC_NUMERIC, "C") # see locale comments above
        self.manager.LogDebug(2, "OnItemAdd event for folder", self,
                              "with item", item.Subject.encode("mbcs", "ignore"))
        if not self.use_timer or not item.UnRead:
            ms = self.manager.message_store
            msgstore_message = ms.GetMessage(item)
            ProcessMessage(msgstore_message, self.manager)
        else:
            self._StartTimer()
class SpamFolderItemsEvent(_BaseItemsEvent):
    def OnItemAdd(self, item):
        self.manager.LogDebug(2, "OnItemAdd event for SPAM folder", self,
                              "with item", item.Subject.encode("mbcs", "ignore"))
        assert(not self.manager.config.training.train_manual_spam,
               "The folder shouldn't be hooked if this is False")
        msgstore_message = self.manager.message_store.GetMessage(item)
        if not msgstore_message.IsFilterCandidate():
            self.manager.LogDebug(1, "Not training message '%s' - we don't filter ones like that!")
            return
        if HaveSeenMessage(msgstore_message, self.manager):
            import train
            self.manager.classifier_data.message_db.load_msg(msgstore_message)
            if train.been_trained_as_ham(msgstore_message):
                need_train = True
            else:
                prop = msgstore_message.GetField(self.manager.config.general.field_score_name)
                if prop is None:
                    prop = self.manager.score(msgstore_message)
                need_train = self.manager.config.filter.spam_threshold > prop * 100
            if need_train:
                TrainAsSpam(msgstore_message, self.manager)
def GetClues(mgr, msgstore_message):
    from cgi import escape
    mgr.classifier_data.message_db.load_msg(msgstore_message)
    score, clues = mgr.score(msgstore_message, evidence=True)
    body = ["<h2>Combined Score: %d%% (%g)</h2>\n" %
            (round(score*100), score)]
    push = body.append
    push("Internal ham score (<tt>%s</tt>): %g<br>\n" % clues.pop(0))
    push("Internal spam score (<tt>%s</tt>): %g<br>\n" % clues.pop(0))
    c = mgr.GetClassifier()
    push("<br>\n")
    push("# ham trained on: %d<br>\n" % c.nham)
    push("# spam trained on: %d<br>\n" % c.nspam)
    push("<br>\n")
    modified_date = msgstore_message.date_modified
    if modified_date:
        from time import localtime, strftime
        modified_date = localtime(modified_date)
        date_string = strftime("%a, %d %b %Y %I:%M:%S %p", modified_date)
        push("As at %s:<br>\n" % (date_string,))
    else:
        push("The last time this message was classified or trained:<br>\n")
    original_score = msgstore_message.GetField(\
        mgr.config.general.field_score_name)
    if original_score is not None:
        original_score *= 100.0
        if original_score >= mgr.config.filter.spam_threshold:
            original_class = "spam"
        elif original_score >= mgr.config.filter.unsure_threshold:
            original_class = "unsure"
        else:
            original_class = "good"
    if original_score is None:
        push("This message had not been filtered.")
    else:
        original_score = round(original_score)
        push("This message was classified as %s (it scored %d%%)." % \
             (original_class, original_score))
    push("<br>\n")
    push("This message had %sbeen trained%s." % \
         {False : ("", " as ham"), True : ("", " as spam"),
          None : ("not ", "")}[msgstore_message.t])
    push("<h2>%s Significant Tokens</h2>\n<PRE>" % len(clues))
    push("<strong>")
    push("token                               spamprob         #ham  #spam\n")
    push("</strong>")
    format = " %-12g %8s %6s\n"
    fetchword = c.wordinfo.get
    for word, prob in clues:
        record = fetchword(word)
        if record:
            nham = record.hamcount
            nspam = record.spamcount
        else:
            nham = nspam = "-"
        if isinstance(word, UnicodeType):
            word = word.encode('mbcs', 'replace')
        else:
            word = repr(word)
        push(escape(word) + " " * (35-len(word)))
        push(format % (prob, nham, nspam))
    push("</PRE>\n")
    push("<h2>Message Stream</h2>\n")
    msg = msgstore_message.GetEmailPackageObject(strip_mime_headers=False)
    push("<PRE>\n")
    push(escape(msg.as_string(), True))
    push("</PRE>\n")
    from spambayes.tokenizer import tokenize
    from spambayes.classifier import Set # whatever classifier uses
    push("<h2>All Message Tokens</h2>\n")
    toks = Set(tokenize(
        msgstore_message.GetEmailPackageObject(strip_mime_headers=True)))
    toks = list(toks)
    toks.sort()
    push("%d unique tokens<br><br>" % len(toks))
    for token in toks:
        if isinstance(token, UnicodeType):
            token = token.encode('mbcs', 'replace')
        else:
            token = repr(token)
        push("<code>" + token + "</code><br>\n")
    body = ''.join(body)
    return body
def ShowClues(mgr, explorer):
    app = explorer.Application
    msgstore_message = explorer.GetSelectedMessages(False)
    if msgstore_message is None:
        return
    body = GetClues(mgr, msgstore_message)
    item = msgstore_message.GetOutlookItem()
    new_msg = app.CreateItem(0)
    new_msg.Subject = "Spam Clues: " + item.Subject
    new_msg.HTMLBody = """\
<HTML>
<HEAD>
<STYLE>
    h2 {color: green}
</STYLE>
</HEAD>
<BODY>""" + body + "</BODY></HTML>"
    new_msg.Attachments.Add(item, constants.olByValue,
                            DisplayName="Original Message")
    new_msg.Display()
def EmptySpamFolder(mgr):
    config = mgr.config.filter
    ms = mgr.message_store
    spam_folder_id = getattr(config, "spam_folder_id")
    try:
        spam_folder = ms.GetFolder(spam_folder_id)
    except ms.MsgStoreException:
        mgr.LogDebug(0, "ERROR: Unable to open the spam folder for emptying - " \
                        "spam messages were not deleted")
    else:
        try:
            if spam_folder.GetItemCount() > 0:
                message = _("Are you sure you want to permanently delete " \
                            "all items in the \"%s\" folder?") \
                            % spam_folder.name
                if mgr.AskQuestion(message):
                    mgr.LogDebug(2, "Emptying spam from folder '%s'" % \
                                 spam_folder.GetFQName())
                    import manager
                    spam_folder.EmptyFolder(manager._GetParent())
            else:
                mgr.LogDebug(2, "Spam folder '%s' was already empty" % \
                             spam_folder.GetFQName())
                message = _("The \"%s\" folder is already empty.") % \
                          spam_folder.name
                mgr.ReportInformation(message)
        except:
            mgr.LogDebug(0, "Error emptying spam folder '%s'!" % \
                         spam_folder.GetFQName())
            traceback.print_exc()
def CheckLatestVersion(manager):
    from spambayes.Version import get_current_version, get_version, \
            get_download_page, fetch_latest_dict
    app_name = "Outlook"
    ver_current = get_current_version()
    cur_ver_string = ver_current.get_long_version(ADDIN_DISPLAY_NAME)
    try:
        SetWaitCursor(1)
        latest = fetch_latest_dict()
        SetWaitCursor(0)
        ver_latest = get_version(app_name, version_dict=latest)
        latest_ver_string = ver_latest.get_long_version(ADDIN_DISPLAY_NAME)
    except:
        print "Error checking the latest version"
        traceback.print_exc()
        manager.ReportError(
            _("There was an error checking for the latest version\r\n"
              "For specific details on the error, please see the SpamBayes log"
              "\r\n\r\nPlease check your internet connection, or try again later")
        )
        return
    print "Current version is %s, latest is %s." % (str(ver_current), str(ver_latest))
    if ver_latest > ver_current:
        url = get_download_page(app_name, version_dict=latest)
        msg = _("You are running %s\r\n\r\nThe latest available version is %s" \
                "\r\n\r\nThe download page for the latest version is\r\n%s" \
                "\r\n\r\nWould you like to visit this page now?") \
                % (cur_ver_string, latest_ver_string, url)
        if manager.AskQuestion(msg):
            print "Opening browser page", url
            os.startfile(url)
    else:
        msg = _("The latest available version is %s\r\n\r\n" \
                "No later version is available.") % latest_ver_string
        manager.ReportInformation(msg)
def Tester(manager):
    import tester
    reload(tester)
    try:
        print "Executing automated tests..."
        tester.test(manager)
    except:
        traceback.print_exc()
        print "Tests FAILED.  Sorry about that.  If I were you, I would do a full re-train ASAP"
        print "Please delete any test messages from your Spam, Unsure or Inbox/Watch folders first."
class ButtonDeleteAsEventBase:
    def Init(self, manager, explorer):
        self.manager = manager
        self.explorer = explorer
    def Close(self):
        self.manager = self.explorer = None
class ButtonDeleteAsSpamEvent(ButtonDeleteAsEventBase):
    def OnClick(self, button, cancel):
        msgstore = self.manager.message_store
        msgstore_messages = self.explorer.GetSelectedMessages(True)
        if not msgstore_messages:
            return
        if not self.manager.config.filter.enabled:
            self.manager.ReportError(
                _("You must configure and enable SpamBayes before you " \
                  "can mark messages as spam"))
            return
        SetWaitCursor(1)
        spam_folder = None
        spam_folder_id = self.manager.config.filter.spam_folder_id
        if spam_folder_id:
            try:
                spam_folder = msgstore.GetFolder(spam_folder_id)
            except msgstore.MsgStoreException:
                pass
        if spam_folder is None:
            self.manager.ReportError(_("You must configure the Spam folder"),
                                     _("Invalid Configuration"))
            return
        import train
        new_msg_state = self.manager.config.general.delete_as_spam_message_state
        for msgstore_message in msgstore_messages:
            self.manager.stats.RecordTraining(False,
                                self.manager.score(msgstore_message))
            msgstore_message.RememberMessageCurrentFolder()
            msgstore_message.Save()
            subject = msgstore_message.GetSubject()
            print "Moving and spam training message '%s' - " % (subject,),
            TrainAsSpam(msgstore_message, self.manager, save_db = False)
            try:
                if new_msg_state == "Read":
                    msgstore_message.SetReadState(True)
                elif new_msg_state == "Unread":
                    msgstore_message.SetReadState(False)
                else:
                    if new_msg_state not in ["", "None", None]:
                        print "*** Bad new_msg_state value: %r" % (new_msg_state,)
            except pythoncom.com_error:
                print "*** Failed to set the message state to '%s' for message '%s'" % (new_msg_state, subject)
            msgstore_message.MoveToReportingError(self.manager, spam_folder)
        self.manager.classifier_data.SavePostIncrementalTrain()
        SetWaitCursor(0)
class ButtonRecoverFromSpamEvent(ButtonDeleteAsEventBase):
    def OnClick(self, button, cancel):
        msgstore = self.manager.message_store
        msgstore_messages = self.explorer.GetSelectedMessages(True)
        if not msgstore_messages:
            return
        if not self.manager.config.filter.enabled:
            self.manager.ReportError(
                _("You must configure and enable SpamBayes before you " \
                  "can mark messages as not spam"))
            return
        SetWaitCursor(1)
        app = self.explorer.Application
        inbox_folder = msgstore.GetFolder(
                    app.Session.GetDefaultFolder(constants.olFolderInbox))
        new_msg_state = self.manager.config.general.recover_from_spam_message_state
        for msgstore_message in msgstore_messages:
            try:
                subject = msgstore_message.GetSubject()
                self.manager.classifier_data.message_db.load_msg(msgstore_message)
                restore_folder = msgstore_message.GetRememberedFolder()
                if restore_folder is None or \
                   msgstore_message.GetFolder() == restore_folder:
                    print "Unable to determine source folder for message '%s' - restoring to Inbox" % (subject,)
                    restore_folder = inbox_folder
                self.manager.stats.RecordTraining(True,
                                        self.manager.score(msgstore_message))
                print "Recovering to folder '%s' and ham training message '%s' - " % (restore_folder.name, subject),
                TrainAsHam(msgstore_message, self.manager, save_db = False)
                try:
                    if new_msg_state == "Read":
                        msgstore_message.SetReadState(True)
                    elif new_msg_state == "Unread":
                        msgstore_message.SetReadState(False)
                    else:
                        if new_msg_state not in ["", "None", None]:
                            print "*** Bad new_msg_state value: %r" % (new_msg_state,)
                except msgstore.MsgStoreException, details:
                    print "*** Failed to set the message state to '%s' for message '%s'" % (new_msg_state, subject)
                    print details
                msgstore_message.MoveToReportingError(self.manager, restore_folder)
            except msgstore.NotFoundException:
                self.manager.LogDebug(1, "'Not Spam' had message moved from underneath us - ignored")
        self.manager.classifier_data.SavePostIncrementalTrain()
        SetWaitCursor(0)
def SetButtonImage(button, fname, manager):
    if hasattr(sys, "frozen"):
        if fname=="recover_ham.bmp":
            bid = 6000
        elif fname=="delete_as_spam.bmp":
            bid = 6001
        else:
            raise RuntimeError, "What bitmap to use for '%s'?" % fname
        handle = win32gui.LoadImage(sys.frozendllhandle, bid, win32con.IMAGE_BITMAP, 0, 0, win32con.LR_DEFAULTSIZE)
    else:
        if not os.path.isabs(fname):
            fname = os.path.join(manager.application_directory,
                                     "images", fname)
        if not os.path.isfile(fname):
            print "WARNING - Trying to use image '%s', but it doesn't exist" % (fname,)
            return None
        handle = win32gui.LoadImage(0, fname, win32con.IMAGE_BITMAP, 0, 0, win32con.LR_DEFAULTSIZE | win32con.LR_LOADFROMFILE)
    win32clipboard.OpenClipboard()
    win32clipboard.SetClipboardData(win32con.CF_BITMAP, handle)
    win32clipboard.CloseClipboard()
    button.Style = constants.msoButtonIconAndCaption
    button.PasteFace()
class ExplorerWithEvents:
    def Init(self, manager, explorers_collection):
        self.manager = manager
        self.have_setup_ui = False
        self.explorers_collection = explorers_collection
        self.toolbar = None
    def SetupUI(self):
        manager = self.manager
        assert self.toolbar is None, "Should not yet have a toolbar"
        tt_text = _("Move the selected message to the Spam folder,\n" \
                    "and train the system that this is Spam.")
        self.but_delete_as = self._AddControl(
                        None,
                        constants.msoControlButton,
                        ButtonDeleteAsSpamEvent, (self.manager, self),
                        Caption=_("Spam"),
                        TooltipText = tt_text,
                        BeginGroup = False,
                        Tag = "SpamBayesCommand.DeleteAsSpam",
                        image = "delete_as_spam.bmp")
        tt_text = _(\
            "Recovers the selected item back to the folder\n" \
            "it was filtered from (or to the Inbox if this\n" \
            "folder is not known), and trains the system that\n" \
            "this is a good message\n")
        self.but_recover_as = self._AddControl(
                        None,
                        constants.msoControlButton,
                        ButtonRecoverFromSpamEvent, (self.manager, self),
                        Caption=_("Not Spam"),
                        TooltipText = tt_text,
                        Tag = "SpamBayesCommand.RecoverFromSpam",
                        image = "recover_ham.bmp")
        popup = None
        for attempt in range(2):
            popup = self._AddControl(
                            None,
                            constants.msoControlPopup,
                            None, None,
                            Caption=_("SpamBayes"),
                            TooltipText = _("SpamBayes anti-spam filters and functions"),
                            Enabled = True,
                            Tag = "SpamBayesCommand.Popup")
            if popup is None:
                break
            popup = CastTo(popup, "CommandBarPopup")
            child = self._AddControl(popup,
                           constants.msoControlButton,
                           ButtonEvent, (manager.ShowManager,),
                           Caption=_("SpamBayes Manager..."),
                           TooltipText = _("Show the SpamBayes manager dialog."),
                           Enabled = True,
                           Visible=True,
                           Tag = "SpamBayesCommand.Manager")
            if child is None:
                try:
                    item = self.CommandBars.FindControl(
                                    Type = constants.msoControlPopup,
                                    Tag = "SpamBayesCommand.Popup")
                    if item is None:
                        print "ERROR: Could't re-find control to delete"
                        break
                    item.Delete(False)
                    print "The above toolbar message is common - " \
                          "recreating the toolbar..."
                except pythoncom.com_error, e:
                    print "ERROR: Failed to delete our dead toolbar control"
                    break
                continue
            self._AddControl(popup,
                           constants.msoControlButton,
                           ButtonEvent, (ShowClues, self.manager, self),
                           Caption=_("Show spam clues for current message"),
                           Enabled=True,
                           Visible=True,
                           Tag = "SpamBayesCommand.Clues")
            self._AddControl(popup,
                           constants.msoControlButton,
                           ButtonEvent, (manager.ShowFilterNow,),
                           Caption=_("Filter messages..."),
                           Enabled=True,
                           Visible=True,
                           Tag = "SpamBayesCommand.FilterNow")
            self._AddControl(popup,
                           constants.msoControlButton,
                           ButtonEvent, (EmptySpamFolder, self.manager),
                           Caption=_("Empty Spam Folder"),
                           Enabled=True,
                           Visible=True,
                           BeginGroup=True,
                           Tag = "SpamBayesCommand.EmptySpam")
            self._AddControl(popup,
                           constants.msoControlButton,
                           ButtonEvent, (CheckLatestVersion, self.manager,),
                           Caption=_("Check for new version"),
                           Enabled=True,
                           Visible=True,
                           BeginGroup=True,
                           Tag = "SpamBayesCommand.CheckVersion")
            helpPopup = self._AddControl(
                            popup,
                            constants.msoControlPopup,
                            None, None,
                            Caption=_("Help"),
                            TooltipText = _("SpamBayes help documents"),
                            Enabled = True,
                            Tag = "SpamBayesCommand.HelpPopup")
            if helpPopup is not None:
                helpPopup = CastTo(helpPopup, "CommandBarPopup")
                self._AddHelpControl(helpPopup,
                                     _("About SpamBayes"),
                                     "about.html",
                                     "SpamBayesCommand.Help.ShowAbout")
                self._AddHelpControl(helpPopup,
                                     _("Troubleshooting Guide"),
                                     "docs/troubleshooting.html",
                                     "SpamBayesCommand.Help.ShowTroubleshooting")
                self._AddHelpControl(helpPopup,
                                     _("SpamBayes Website"),
                                     "http://spambayes.sourceforge.net/",
                                     "SpamBayesCommand.Help.ShowSpamBayes Website")
                self._AddHelpControl(helpPopup,
                                     _("Frequently Asked Questions"),
                                     "http://spambayes.sourceforge.net/faq.html",
                                     "SpamBayesCommand.Help.ShowFAQ")
                self._AddHelpControl(helpPopup,
                                     _("SpamBayes Bug Tracker"),
                                     "http://sourceforge.net/tracker/?group_id=61702&atid=498103",
                                     "SpamBayesCommand.Help.BugTacker")
        if not hasattr(sys, "frozen"):
            self._AddControl(popup,
                           constants.msoControlButton,
                           ButtonEvent, (Tester, self.manager),
                           Caption=_("Execute test suite"),
                           Enabled=True,
                           Visible=True,
                           BeginGroup=True,
                           Tag = "SpamBayesCommand.TestSuite")
        self.have_setup_ui = True
    def _AddHelpControl(self, parent, caption, url, tag):
        self._AddControl(parent,
                        constants.msoControlButton,
                        ButtonEvent, (self.manager.ShowHtml, url),
                        Caption=caption,
                        Enabled=True,
                        Visible=True,
                        Tag=tag)
    def _AddControl(self,
                    parent, # who the control is added to
                    control_type, # type of control to add.
                    events_class, events_init_args, # class/Init() args
                    **item_attrs): # extra control attributes.
        assert item_attrs.has_key('Tag'), "Need a 'Tag' attribute!"
        image_fname = None
        if 'image' in item_attrs:
            image_fname = item_attrs['image']
            del item_attrs['image']
        tag = item_attrs["Tag"]
        item = self.CommandBars.FindControl(
                        Type = control_type,
                        Tag = tag)
        temporary = parent is not None
        if item is not None and temporary:
            item.Delete(False)
            item = None
        if item is None:
            if parent is None:
                if self.toolbar is None:
                    bars = self.CommandBars
                    for i in range(bars.Count):
                        toolbar = bars.Item(i+1)
                        if toolbar.Name == "SpamBayes":
                            self.toolbar = toolbar
                            break
                    else:
                        print "Creating new SpamBayes toolbar to host our buttons"
                        self.toolbar = bars.Add(toolbar_name,
                                                constants.msoBarTop,
                                                Temporary=False)
                    self.toolbar.Visible = True
                parent = self.toolbar
            try:
                item = parent.Controls.Add(Type=control_type, Temporary=temporary)
            except pythoncom.com_error, e:
                print "FAILED to add the toolbar item '%s' - %s" % (tag,e)
                return
            if image_fname:
                assert control_type == constants.msoControlButton
                but = CastTo(item, "_CommandBarButton")
                SetButtonImage(but, image_fname, self.manager)
            for attr, val in item_attrs.items():
                setattr(item, attr, val)
        item.OnAction = "<!" + OutlookAddin._reg_progid_ + ">"
        if events_class is not None and tag not in self.explorers_collection.button_event_map:
            item = DispatchWithEvents(item, events_class)
            item.Init(*events_init_args)
            self.explorers_collection.button_event_map[tag] = item
        return item
    def GetSelectedMessages(self, allow_multi = True, explorer = None):
        if explorer is None:
            explorer = self.Application.ActiveExplorer()
        sel = explorer.Selection
        if sel.Count > 1 and not allow_multi:
            self.manager.ReportError(_("Please select a single item"),
                                     _("Large selection"))
            return None
        ret = []
        ms = self.manager.message_store
        for i in range(sel.Count):
            item = sel.Item(i+1)
            try:
                msgstore_message = ms.GetMessage(item)
                if msgstore_message.IsFilterCandidate():
                    ret.append(msgstore_message)
            except ms.NotFoundException:
                pass
            except ms.MsgStoreException, details:
                print "Unexpected error fetching message"
                traceback.print_exc()
                print details
        if len(ret) == 0:
            self.manager.ReportError(_("No filterable mail items are selected"),
                                     _("No selection"))
            return None
        if allow_multi:
            return ret
        return ret[0]
    def OnActivate(self):
        self.manager.LogDebug(3, "OnActivate", self)
        pass
    def OnSelectionChange(self):
        self.manager.LogDebug(3, "OnSelectionChange", self)
        if not self.have_setup_ui:
            self.SetupUI()
            self.OnFolderSwitch()
    def OnClose(self):
        self.manager.LogDebug(3, "Explorer window closing", self)
        self.explorers_collection._DoDeadExplorer(self)
        self.explorers_collection = None
        self.toolbar = None
        self.close() # disconnect events.
    def OnBeforeFolderSwitch(self, new_folder, cancel):
        self.manager.LogDebug(3, "OnBeforeFolderSwitch", self)
    def OnFolderSwitch(self):
        self.manager.LogDebug(3, "OnFolderSwitch", self)
        if not self.have_setup_ui:
            self.SetupUI()
        outlook_folder = self.CurrentFolder
        if outlook_folder is None or \
           outlook_folder.DefaultItemType != constants.olMailItem:
            show_delete_as = False
            show_recover_as = False
        else:
            show_delete_as = True
            show_recover_as = False
            try:
                mapi_folder = self.manager.message_store.GetFolder(outlook_folder)
                look_id = self.manager.config.filter.spam_folder_id
                if mapi_folder is not None and look_id:
                    look_folder = self.manager.message_store.GetFolder(look_id)
                    if mapi_folder == look_folder:
                        show_recover_as = True
                        show_delete_as = False
                look_id = self.manager.config.filter.unsure_folder_id
                if mapi_folder is not None and look_id:
                    look_folder = self.manager.message_store.GetFolder(look_id)
                    if mapi_folder == look_folder:
                        show_recover_as = True
                        show_delete_as = True
            except:
                print "Error finding the MAPI folders for a folder switch event"
                self.manager.ReportErrorOnce(_(
                    "There appears to be a problem with the SpamBayes"
                    " configuration\r\n\r\nPlease select the SpamBayes"
                    " manager, and run the\r\nConfiguration Wizard to"
                    " reconfigure the filter."),
                    _("Invalid SpamBayes Configuration"))
                traceback.print_exc()
        if self.but_recover_as is not None:
            self.but_recover_as.Visible = show_recover_as
        if self.but_delete_as is not None:
            self.but_delete_as.Visible = show_delete_as
    def OnBeforeViewSwitch(self, new_view, cancel):
        self.manager.LogDebug(3, "OnBeforeViewSwitch", self)
    def OnViewSwitch(self):
        self.manager.LogDebug(3, "OnViewSwitch", self)
        if not self.have_setup_ui:
            self.SetupUI()
class ExplorersEvent:
    def Init(self, manager):
        assert manager
        self.manager = manager
        self.explorers = []
        self.button_event_map = {}
    def Close(self):
        while self.explorers:
            self._DoDeadExplorer(self.explorers[0])
        self.explorers = None
    def _DoNewExplorer(self, explorer):
        explorer = DispatchWithEvents(explorer, ExplorerWithEvents)
        explorer.Init(self.manager, self)
        self.explorers.append(explorer)
        return explorer
    def _DoDeadExplorer(self, explorer):
        self.explorers.remove(explorer)
        if len(self.explorers)==0:
            for tag, button in self.button_event_map.items():
                closer = getattr(button, "Close", None)
                if closer is not None:
                    closer()
            self.button_event_map = {}
    def OnNewExplorer(self, explorer):
        self._DoNewExplorer(explorer)
class OutlookAddin:
    _com_interfaces_ = ['_IDTExtensibility2']
    _public_methods_ = []
    _reg_clsctx_ = pythoncom.CLSCTX_INPROC_SERVER
    _reg_clsid_ = "{3556EDEE-FC91-4cf2-A0E4-7489747BAB10}"
    _reg_progid_ = "SpamBayes.OutlookAddin"
    _reg_policy_spec_ = "win32com.server.policy.EventHandlerPolicy"
    def __init__(self):
        self.folder_hooks = {}
        self.application = None
    def OnConnection(self, application, connectMode, addin, custom):
        locale.setlocale(locale.LC_NUMERIC, "C") # see locale comments above
        import manager
        try:
            self.application = application
            self.manager = None # if we die while creating it!
            self.manager = manager.GetManager(application)
            assert self.manager.addin is None, "Should not already have an addin"
            self.manager.addin = self
            from spambayes.Version import get_current_version
            v = get_current_version()
            vstring = v.get_long_version(ADDIN_DISPLAY_NAME)
            if not hasattr(sys, "frozen"): vstring += " from source"
            print vstring
            major, minor, spack, platform, ver_str = win32api.GetVersionEx()
            print "on Windows %d.%d.%d (%s)" % \
                  (major, minor, spack, ver_str)
            print "using Python", sys.version
            from time import asctime, localtime
            print "Log created", asctime(localtime())
            self.explorers_events = None # create at OnStartupComplete
            if connectMode == constants.ext_cm_AfterStartup:
                self.OnStartupComplete(None)
        except:
            print "Error connecting to Outlook!"
            traceback.print_exc()
            manager.ReportError(
                "There was an error initializing the SpamBayes addin\r\n\r\n"
                "Please re-start Outlook and try again.")
    def OnStartupComplete(self, custom):
        if self.manager.never_configured:
            import dialogs
            dialogs.ShowWizard(0, self.manager)
        if self.manager.config.filter.enabled:
            if not self.manager.config.filter.spam_folder_id or \
               not self.manager.config.filter.watch_folder_ids:
                msg = _("It appears there was an error loading your configuration\r\n\r\n" \
                        "Please re-configure SpamBayes via the SpamBayes dropdown")
                self.manager.ReportError(msg)
            self.FiltersChanged()
            try:
                self.ProcessMissedMessages()
            except:
                print "Error processing missed messages!"
                traceback.print_exc()
        else:
            self.manager.LogDebug(0, _("*** SpamBayes is NOT enabled, so " \
                                       "will not filter incoming mail. ***"))
        explorers = self.application.Explorers
        if self.manager is not None: # If we successfully started up.
            self.explorers_events = WithEvents(explorers, ExplorersEvent)
            self.explorers_events.Init(self.manager)
            for i in range(explorers.Count):
                explorer = explorers.Item(i+1)
                explorer = self.explorers_events._DoNewExplorer(explorer)
                explorer.OnFolderSwitch()
    def ProcessMissedMessages(self):
        from time import clock
        config = self.manager.config.filter
        manager = self.manager
        field_name = manager.config.general.field_score_name
        for folder in manager.message_store.GetFolderGenerator(
                                    config.watch_folder_ids,
                                    config.watch_include_sub):
            event_hook = self._GetHookForFolder(folder)
            if event_hook is None:
                manager.LogDebug(0,
                    "Skipping processing of missed messages in folder '%s', "
                    "as it is not available" % folder.name)
            elif event_hook.use_timer:
                print "Processing missed spam in folder '%s' by starting a timer" \
                      % (folder.name,)
                event_hook._StartTimer()
            else:
                num = 0
                start = clock()
                for message in folder.GetNewUnscoredMessageGenerator(field_name):
                    ProcessMessage(message, manager)
                    num += 1
                print "Processing %d missed spam in folder '%s' took %gms" \
                      % (num, folder.name, (clock()-start)*1000)
    def FiltersChanged(self):
        try:
            self.UpdateFolderHooks()
        except:
            self.manager.ReportFatalStartupError(
                "Could not watch the specified folders")
        unsure_id = self.manager.config.filter.unsure_folder_id
        if unsure_id:
            try:
                self.manager.EnsureOutlookFieldsForFolder(unsure_id)
            except:
                print "Error adding field to 'Unsure' folder %r" % (unsure_id,)
                etype, value, tb = sys.exc_info()
                tb = None # dont want it, and nuke circular ref
                traceback.print_exception(etype, value, tb)
    def UpdateFolderHooks(self):
        config = self.manager.config.filter
        new_hooks = {}
        new_hooks.update(
            self._HookFolderEvents(config.watch_folder_ids,
                                   config.watch_include_sub,
                                   HamFolderItemsEvent,
                                   "filtering")
            )
        if config.spam_folder_id and \
           self.manager.config.training.train_manual_spam:
            new_hooks.update(
                self._HookFolderEvents([config.spam_folder_id],
                                       False,
                                       SpamFolderItemsEvent,
                                       "incremental training")
                )
        for k in self.folder_hooks.keys():
            if not new_hooks.has_key(k):
                self.folder_hooks[k].Close()
        self.folder_hooks = new_hooks
    def _GetHookForFolder(self, folder):
        ret = self.folder_hooks.get(folder.id)
        if ret is None: # we were unable to hook events for this folder.
            return None
        assert ret.target == folder
        return ret
    def _HookFolderEvents(self, folder_ids, include_sub, HandlerClass, what):
        new_hooks = {}
        for msgstore_folder in self.manager.message_store.GetFolderGenerator(
                    folder_ids, include_sub):
            existing = self.folder_hooks.get(msgstore_folder.id)
            if existing is None or existing.__class__ != HandlerClass:
                name = msgstore_folder.GetFQName()
                try:
                    folder = msgstore_folder.GetOutlookItem()
                except self.manager.message_store.MsgStoreException, details:
                    print "FAILED to open the Outlook folder '%s' " \
                          "to hook events" % name
                    print details
                    continue
                try:
                    self.manager.EnsureOutlookFieldsForFolder(msgstore_folder.GetID())
                except:
                    print "ERROR: Failed to check folder '%s' for " \
                          "Spam field" % name
                    etype, value, tb = sys.exc_info()
                    tb = None # dont want it, and nuke circular ref
                    traceback.print_exception(etype, value, tb)
                try:
                    new_hook = DispatchWithEvents(folder.Items, HandlerClass)
                except ValueError:
                    print "WARNING: Folder '%s' can not hook events" % (name,)
                    new_hook = None
                if new_hook is not None:
                    new_hook.Init(msgstore_folder, self.application, self.manager)
                    new_hooks[msgstore_folder.id] = new_hook
                    print "SpamBayes: Watching (for %s) in '%s'" % (what, name)
            else:
                new_hooks[msgstore_folder.id] = existing
                existing.ReInit()
        return new_hooks
    def OnDisconnection(self, mode, custom):
        print "SpamBayes - Disconnecting from Outlook"
        if self.folder_hooks:
            for hook in self.folder_hooks.values():
                hook.Close()
            self.folder_hooks = None
        if self.explorers_events is not None:
            self.explorers_events.Close()
            self.explorers_events = None
        if self.manager is not None:
            self.manager.Save()
            print "Session:"
            print "\r\n".join(self.manager.stats.GetStats(session_only=True))
            print "Total:"
            print "\r\n".join(self.manager.stats.GetStats())
            self.manager.Close()
            self.manager = None
        if mode==constants.ext_dm_UserClosed:
            print "SpamBayes is being manually disabled - deleting toolbar"
            try:
                explorers = self.application.Explorers
                for i in range(explorers.Count):
                    explorer = explorers.Item(i+1)
                    try:
                        toolbar = explorer.CommandBars.Item(toolbar_name)
                    except pythoncom.com_error:
                        print "Could not find our toolbar to delete!"
                    else:
                        toolbar.Delete()
            except:
                print "ERROR deleting toolbar"
                traceback.print_exc()
        self.application = None
        print "Addin terminating: %d COM client and %d COM servers exist." \
              % (pythoncom._GetInterfaceCount(), pythoncom._GetGatewayCount())
        try:
            total_refs = sys.gettotalrefcount() # debug Python builds only
            print "%d Python references exist" % (total_refs,)
        except AttributeError:
            pass
    def OnAddInsUpdate(self, custom):
        pass
    def OnBeginShutdown(self, custom):
        pass
def _DoRegister(klass, root):
    key = _winreg.CreateKey(root,
                            "Software\\Microsoft\\Office\\Outlook\\Addins")
    subkey = _winreg.CreateKey(key, klass._reg_progid_)
    _winreg.SetValueEx(subkey, "CommandLineSafe", 0, _winreg.REG_DWORD, 0)
    _winreg.SetValueEx(subkey, "LoadBehavior", 0, _winreg.REG_DWORD, 3)
    _winreg.SetValueEx(subkey, "Description", 0, _winreg.REG_SZ, "SpamBayes anti-spam tool")
    _winreg.SetValueEx(subkey, "FriendlyName", 0, _winreg.REG_SZ, "SpamBayes")
def DllInstall(bInstall, cmdline):
    klass = OutlookAddin
    if bInstall and cmdline.lower().find('hkey_local_machine')>=0:
        DllUnregisterServer()
        _DoRegister(klass, _winreg.HKEY_LOCAL_MACHINE)
        print "Registration (in HKEY_LOCAL_MACHINE) complete."
def DllRegisterServer():
    klass = OutlookAddin
    try:
        _winreg.DeleteKey(_winreg.HKEY_LOCAL_MACHINE,
                          "Software\\Microsoft\\Office\\Outlook\\Addins\\" \
                          + klass._reg_progid_)
    except WindowsError:
        pass
    _DoRegister(klass, _winreg.HKEY_CURRENT_USER)
    print "Registration complete."
def DllUnregisterServer():
    klass = OutlookAddin
    try:
        _winreg.DeleteKey(_winreg.HKEY_LOCAL_MACHINE,
                          "Software\\Microsoft\\Office\\Outlook\\Addins\\" \
                          + klass._reg_progid_)
    except WindowsError:
        pass
    try:
        _winreg.DeleteKey(_winreg.HKEY_CURRENT_USER,
                          "Software\\Microsoft\\Office\\Outlook\\Addins\\" \
                          + klass._reg_progid_)
    except WindowsError:
        pass
if __name__ == '__main__':
    if hasattr(sys, "frozen"):
        sys.frozendllhandle = win32api.LoadLibrary("outlook_addin.dll")
        pythoncom.frozen = sys.frozen = "dll"
        OutlookAddin._reg_class_spec_ = "addin.OutlookAddin"
    import win32com.server.register
    win32com.server.register.UseCommandLine(OutlookAddin)
    if "--unregister" in sys.argv:
        DllUnregisterServer()
    else:
        DllRegisterServer()
        for a in sys.argv[1:]:
            if a.lower()=='hkey_local_machine':
                DllInstall(True, 'hkey_local_machine')
