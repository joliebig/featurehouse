import gobject
import Conversation
import ConversationWindow
class ConversationManager(gobject.GObject):
    '''This class handle a collection of conversations'''
    __gsignals__ = {
        'new-conversation-ui' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
        (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT)),
        'close-conversation-ui' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
        (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT)),
        'send-message' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
        (gobject.TYPE_PYOBJECT,gobject.TYPE_STRING)),
        'receive-message' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
        (gobject.TYPE_PYOBJECT,gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING,gobject.TYPE_STRING)),
    }
    def __init__(self, controller):
        '''Contructor'''
        gobject.GObject.__init__(self)
        self.controller = controller
        self.theme = controller.theme
        self.config = controller.config
        self.conversations = []
        self.user = ''
        self.signals = []
        sap = self.signals.append
        sap(self.config.connect('change::showHeader', self.updateUI))
        sap(self.config.connect('change::showToolbar', self.updateUI))
        sap(self.config.connect('change::showAvatars', self.updateUI))
        sap(self.config.connect('change::showAvatarMine', self.updateUI))
        sap(self.config.connect('change::showAvatarOther', self.updateUI))
        sap(self.config.connect('change::showStatusBar', self.updateUI))
        sap(self.config.connect('change::showTabCloseButton', self.updateUI))
        sap(self.config.connect('change::showSendButton', self.updateUI))
        sap(self.config.connect('change::hideNewWindow', self.updateUI))
        self.controller.connect('preferences-changed', self.updateUI)
    def handleLogin(self, user):
        '''handle a new user login (close conversations or reconnect them, etc.)'''
        if self.user == user:
            self.enableAll()
        else:
            self.user = user
            self.closeAll()
    def updateUI(self, *args):
        for window, conversation in self.conversations:
            conversation.ui.update()
    def getOpenConversation(self, mail, switchboard = None):
        '''return (ConversationWindow, conversation) for this contact,
        or return None if there isn't a conversation open yet'''
        self.removeClosedWindows()
        for window, conversation in self.conversations:
            members = conversation.getMembers()
            if len(members) == 1 and members[0] == mail:
                return (window, conversation)
            elif len(members) > 1 and mail in members:
                if switchboard and len(switchboard.members) == len(members) and \
                    sorted(switchboard.members.keys()) == sorted(members):
                        return (window, conversation)
        return None
    def removeClosedWindows(self):
        '''remove conversations for closed windows'''
        for window, conversation in self.conversations[:]:
            if window.closed or conversation.closed or \
               (window.notOpen and conversation.getStatus() == 'closed'):
                self.conversations.remove((window, conversation))
    def openConversation(self, msnp, mail, weStarted, switchboard=None):
        '''opens a new conversation and a new window or tab'''
        if switchboard is None:
            switchboard = msnp.newSwitchboard()
            switchboard.invite(mail)
        conversation = Conversation.Conversation(self.controller, switchboard)
        useTabs = not self.config.user['windows']
        if useTabs and len(self.conversations) > 0:
            window = self.conversations[0][0]
            window.openTab(conversation)
            if weStarted:
                window.present()
        else:
            window = ConversationWindow.ConversationWindow(self.controller, conversation)
            if self.config.user['hideNewWindow']:
                window.iconify()
            window.show()
        conversation.setWindow(window)
        window.set_icon(conversation.getWindowIcon())
        self.conversations.append((window, conversation))
        self.emit('new-conversation-ui', conversation, window)
        return window, conversation
    def newConversation(self, msnp, mail, switchboard, weStarted):
        '''Open a new conversation, or open an existing window or tab
        if switchboard is None we create the switchboard'''
        window = conversation = None
        result = self.getOpenConversation(mail, switchboard)
        if result is not None:
            window, conversation = result
            if switchboard:
                conversation.setSwitchboard(switchboard)
            if weStarted:
                if not self.config.user['windows']:
                    window.showTab(window.tabs.page_num(conversation.ui))
                window.present()
        else:
            window, conversation = self.openConversation(msnp, mail,
                                weStarted, switchboard)
        return window, conversation
    def do_send_message(self, conversation, message):
        '''Send message to conversation'''
        conversation.do_send_message(message)
    def do_receive_message(self, conversation, mail, nick, message, format,
      charset):
        '''Receive a message from'''
        conversation.do_receive_message(mail, nick, message, format, charset)
    def closeAll(self):
        '''close all the conversations'''
        for window, conversation in self.conversations:
            window.hide()
        self.conversations = []
    def disableAll(self):
        '''close conversations and disable text input of conversation windows'''
        for window, conversation in self.conversations:
            conversation.ui.setInputEnabled(False)
            conversation.switchboard.leaveChat()
    def enableAll(self):
        '''reconnect conversations and re-enable text input'''
        for window, conversation in self.conversations:
            conversation.reconnect()
            conversation.ui.setInputEnabled(True)
