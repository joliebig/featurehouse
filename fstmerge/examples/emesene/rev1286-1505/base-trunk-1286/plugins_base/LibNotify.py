import gtk
import gobject
import Plugin
import desktop
from emesenelib.common import escape
from emesenelib.common import unescape
from Parser import PangoDataType
ERROR = ''
try:
    import pynotify
    if not pynotify.init("emesene"):
        raise
except:
    ERROR = _('there was a problem initializing the pynotify module')
class MainClass(Plugin.Plugin):
    '''Main plugin class'''
    def __init__(self, controller, msn):
        '''Contructor'''
        Plugin.Plugin.__init__(self, controller, msn)
        self.description = _('Notify about diferent events using pynotify')
        self.authors = {'Mariano Guerra': 'luismarianoguerra at gmail dot com'}
        self.website = 'http://emesene-msn.blogspot.com'
        self.displayName = _('LibNotify')
        self.name = 'LibNotify'
        self.config = controller.config
        self.config.readPluginConfig(self.name)
        self.notifyOnline = int(self.config.getPluginValue(self.name, 'notifyOnline', '1'))
        self.notifyOffline = int(self.config.getPluginValue(self.name, 'notifyOffline', '1'))
        self.notifyNewMail = int(self.config.getPluginValue(self.name, 'notifyMail', '1'))
        self.notifyTyping = int(self.config.getPluginValue(self.name, 'typing', '0'))
        self.notifyNewMsg = int(self.config.getPluginValue(self.name, 'notifyMessage', '1'))
        self.notifyBusy = int(self.config.getPluginValue(self.name, 'notifyBusy', '0'))
        self.tray = self.controller.trayIcon.getNotifyObject()
        self.pixbuf = self.controller.theme.getImage('userPanel')
        self.controller = controller
        self.parser = controller.unifiedParser
        self.contacts = controller.contacts  # ref..
        self.notifications = []
        self.onlineId = None
        self.offlineId = None
        self.newMsgId = None
        self.offMsgId = None
        self.typingId = None
        self.newMailId = None
        self.initMailId = None
    def start(self):
        '''start the plugin'''
        self.onlineId = self.connect('user-online', self.online)
        self.offlineId = self.connect('user-offline', self.offline)
        self.newMsgId = self.connect('message-received', self.newMsg)
        self.offMsgId = self.connect('offline-message-received', self.offMsg)
        self.typingId = self.connect('switchboard::typing', self.receiveTyping)
        self.newMailId = self.connect('new-mail-notification', self.newMail)
        self.initMailId = self.connect('initial-mail-notification', \
                self.initMail)
        self.enabled = True
    def stop(self):
        ''' stop the plugin '''
        for i in self.notifications:
            i.close()
        self.notifications = []
        self.disconnect(self.onlineId)
        self.disconnect(self.offlineId)
        self.disconnect(self.newMsgId)
        self.disconnect(self.offMsgId)
        self.disconnect(self.typingId)
        self.disconnect(self.newMailId)
        self.disconnect(self.initMailId)
        self.enabled = False
    def check(self):
        '''check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )'''
        global ERROR
        if ERROR != '':
            return (False, ERROR)
        return (True, 'Ok')
    def configure(self):
        '''display a configuration dialog'''
        l = []
        l.append(Plugin.Option('notifyOnline', bool, \
                _('Notify when someone gets online'), '', self.config.\
                getPluginValue(self.name, 'notifyOnline', '1') == '1'))
        l.append(Plugin.Option('notifyOffline', bool, \
                _('Notify when someone gets offline'), '', self.config.\
                getPluginValue(self.name, 'notifyOffline', '1') == '1'))
        l.append(Plugin.Option('notifyMail', bool, \
                _('Notify when receiving an email'), '', self.config.\
                getPluginValue(self.name, 'notifyMail', '1') == '1'))
        l.append(Plugin.Option('typing', bool, \
                _('Notify when someone starts typing'), '', self.config.\
                getPluginValue(self.name, 'typing', '1') == '1'))
        l.append(Plugin.Option('notifyMessage', bool, \
                _('Notify when receiving a message'), '', self.config.\
                getPluginValue(self.name, 'notifyMessage', '1') == '1'))
        l.append(Plugin.Option('notifyBusy', bool, \
                _('Disable notifications when busy'), '', self.config.\
                getPluginValue(self.name, 'notifyBusy', '1') == '1'))
        response = Plugin.ConfigWindow(_('Config LibNotify Plugin'), l).run()
        if response != None:
            def check(event):
                if response.has_key(event):
                    self.config.setPluginValue(self.name, event, \
                            str(int(response[event].value)))
            check('notifyOnline')
            if response.has_key('notifyOffline'):
                self.config.setPluginValue(self.name, 'notifyOffline', \
                    str(int(response['notifyOffline'].value)))
            if response.has_key('notifyMail'):
                self.config.setPluginValue(self.name, 'notifyMail', \
                    str(int(response['notifyMail'].value)))
            if response.has_key('typing'):
                self.config.setPluginValue(self.name, 'typing', \
                    str(int(response['typing'].value)))
            if response.has_key('notifyMessage'):
                self.config.setPluginValue(self.name, 'notifyMessage', \
                    str(int(response['notifyMessage'].value)))
            if response.has_key('notifyBusy'):
                self.config.setPluginValue(self.name, 'notifyBusy', \
                    str(int(response['notifyBusy'].value)))
        self.notifyOnline = (self.config.getPluginValue \
                (self.name, 'notifyOnline', '1') == '1')
        self.notifyOffline = (self.config.getPluginValue \
                (self.name, 'notifyOffline', '1') == '1')
        self.notifyNewMail = (self.config.getPluginValue \
                (self.name, 'notifyMail', '1') == '1')
        self.notifyTyping = (self.config.getPluginValue \
                (self.name, 'typing', '1') == '1')
        self.notifyNewMsg = (self.config.getPluginValue \
                (self.name, 'notifyMessage', '1') == '1')
        self.notifyBusy = (self.config.getPluginValue \
                (self.name, 'notifyBusy', '1') == '1')
        return True
    def notifyEnabled(self, contact = None):
        '''checks if notifications are enabled'''
        if not self.enabled:
            return False
        if self.notifyBusy and self.msn.status == 'BSY':
            return False
        if contact != None:
            if self.contacts.get_blocked(contact):
                return False
        return True
    def notify(self, contact, title, text, execute = '', data = None):
        notification = pynotify.Notification(title, text, attach = self.tray)
        if contact != '':
            if self.controller.theme.hasUserDisplayPicture(contact):
                self.pixbuf = self.controller.theme.getUserDisplayPicture( \
                    contact, 48, 48)
        notification.set_icon_from_pixbuf(self.pixbuf)
        if contact != '':
            self.pixbuf = self.controller.theme.getImage('userPanel')
        if execute == 'conversation':
            def on_notify_action(notification, action):
                self.controller.newConversation(None, data[0], data[1], True)
            notification.add_action('default', 'default', on_notify_action)
        elif execute == 'mail':
            def openMail(notification, action):
                desktop.open(self.controller.hotmail.getLoginPage\
                    (data[0], data[1], data[2]))
            notification.add_action('default', 'Open Mail', openMail)
        if self.controller.trayIcon and \
           isinstance(self.controller.trayIcon.tray, gtk.StatusIcon):
            tray = self.controller.trayIcon.tray
            try:
                notification.attach_to_status_icon(tray)
            except:
                print "cant set geometry hints on libnotify plugin"
        try:
            notification.show()
        except gobject.GError:
            print "can't display notification" # TODO, retry?
            return
        notification.connect('closed', self.on_notify_close)
        self.notifications.append(notification)
    def getNickPM(self, email, name = ''):
        ''' returns the nick and pm of the contact '''
        contact = self.msn.contactManager.getContact(email)  # :(:(
        if not self.contacts.exists(email):
            text = name + ' &lt;' + email + '&gt;'
        else:
            nick = self.parser.getParser(
                self.contacts.get_display_name(email)).get()
            pm = self.parser.getParser(
                self.contacts.get_message(email)).get()
            text = nick + ' <span foreground="#AAAAAA">' + pm + '</span>'
        return contact, text
    def online(self, msnp, email, oldStatus):
        '''called when someone get online'''
        if not (self.notifyOnline and self.notifyEnabled(email)):
            return
        if oldStatus != 'FLN':
            return
        contact, text = self.getNickPM(email)
        text += "\n" + _("is online")
        self.notify(contact, "emesene", text, 'conversation', (email, None))
    def offline(self, msnp, email):
        '''called when someone get offline'''
        if not (self.notifyOffline and self.notifyEnabled(email)):
            return
        contact, text = self.getNickPM(email)
        text += "\n" + _("is offline")
        self.notify(contact, "emesene", text)
    def newMsg(self, msnp, email):
        '''called when someone send a message'''
        if not (self.notifyNewMsg and self.notifyEnabled(email)):
            return
	result = self.controller.conversationManager.getOpenConversation(email)
        if result != None:
            window, conversation = result
            windowFocus = window.is_active()
            tabFocus = (window.conversation == conversation)
            if windowFocus and tabFocus:
                return
        contact, text = self.getNickPM(email)
        text += "\n" + _("has send a message")
        self.notify(contact, "emesene", text, 'conversation', (email, None))
    def offMsg(self, msnp, oim):
        '''called when someone sent an offline message'''
        email = oim[0]['addr']
        if not (self.notifyNewMsg and self.notifyEnabled(email)):
            return
	result = self.controller.conversationManager.getOpenConversation(email)
        if result != None:
            window, conversation = result
            windowFocus = window.is_active()
            tabFocus = (window.conversation == conversation)
            if windowFocus and tabFocus:
                return
        contact, text = self.getNickPM(email)
        text += "\n" + _("sent an offline message")
        self.notify(contact, "emesene", text, 'conversation', (email, None))
    def receiveTyping(self, msn, switchboard, signal, args):
        '''called when someone starts typing'''
        email = args[0]
        if not (self.notifyTyping and self.notifyEnabled(email)):
            return
        if self.controller.conversationManager.getOpenConversation \
                (email, switchboard) != None:
            return
        contact, text = self.getNickPM(email)
        text += "\n" + _("starts typing")
        self.notify(contact, "emesene", text, 'conversation', \
                (email, switchboard))
    def newMail(self, msnp, From, FromAddr, Subject, MessageURL, PostURL, id):
        ''' called when receiving mail '''
        if not (self.notifyNewMail and self.notifyEnabled(FromAddr)):
            return
        contact, text = self.getNickPM(FromAddr, From)
        text = _('From: ') + text + '\n' + _('Subj: ') + escape(Subject) \
                + '\n\n<span foreground="#AAAAAA">emesene</span>'
        self.notify(contact, _('New email'), text, 'mail', \
                (MessageURL, PostURL, id))
    def initMail(self, msnp):
        if self.notifyNewMail:
            unread = self.controller.getUnreadMails()
            try:
                unread = int(unread)
            except ValueError, TypeError:
                unread = 0
            if unread > 0:
                if unread == 1:
                    s = ''
                else:
                    s = 's'
                self.notify('', "emesene", \
                        _('You have %(num)i unread message%(s)s') % \
                        {'num': unread, 's': s}, 'mail', (None, None, '2'))
    def on_notify_close(self, n):
        if n in self.notifications:
            self.notifications.remove(n)
