import os
import gtk
import sys
import time
import thread
import gobject
import gettext
import weakref
import traceback
from emesenecommon import PATH
if os.path.exists("default.mo"):
    gettext.GNUTranslations(open("default.mo")).install()
elif os.path.exists(PATH.LANG_PATH):
    gettext.install('emesene', PATH.LANG_PATH)
else:
    gettext.install('emesene')
import Theme
import Avatar
import Config
import desktop
import TrayIcon
import MainWindow
import StatusMenu
import SlashCommands
import PluginManager
import PluginManagerDialog
import ConversationManager
import ConversationLayoutManager
from Parser import UnifiedParser
from CustomEmoticons import CustomEmoticons
import emesenelib.common
from emesenelib import core
from emesenelib import Socket
from emesenelib import Hotmail
from emesenecommon import *
import stock
import dialog
import abstract.stock as stock
import GroupManager
import ContactManager
SERVER_HOST = 'messenger.hotmail.com'
class Controller(gobject.GObject):
    '''The Controller class concentrate all the logic of the program
    leaving the other classes to only implement the GUI.
    All the classes with GUI should receive a Controller instance to
    be able to do things.'''
    __gsignals__ = {
        'font-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, 
        (gobject.TYPE_STRING, gobject.TYPE_BOOLEAN, gobject.TYPE_BOOLEAN,
            gobject.TYPE_INT)),
        'color-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, 
        (gobject.TYPE_STRING,)),
        'input-format-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, 
        (gobject.TYPE_PYOBJECT,)),
        'avatar-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, 
        ()),
        'preferences-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, 
        ()),
        'usermenu-item-add' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, 
        (gobject.TYPE_PYOBJECT,)),
    }
    def __init__(self, username, minimized, safe, leakdebug):
        '''Constructor'''
        gobject.GObject.__init__(self)
        self.status_ordered = [
            [ 'NLN', 'AWY', 'BSY', 'BRB', 'PHN', 'LUN', 'HDN', 'IDL', 'FLN' ],
            [ 'online', 'away', 'busy', 'brb', 'phone',
             'lunch', 'invisible', 'idle', 'offline' ],
            [ _('Online'), _('Away'), _('Busy'), _('Be right back'),
             _('On the phone'), _('Gone to lunch'), _('Invisible'),
             _('Idle'), _('Offline') ],
            [ _('_Online'), _('_Away'), _('_Busy'), _('Be _right back'),
             _('On the _phone'), _('Gone to _lunch'), _('_Invisible'),
             _('I_dle'), _('O_ffline') ]
        ]
        self.connected = False
        self.userEmail = None
        self.userStatus = None
        self.config = Config.Config()
        self.config.connect('change::debug', self.updateDebug)
        self.config.connect('change::binary', self.updateDebug)
        desktop.override = self.config.glob['overrideDesktop']
        self.theme = Theme.Theme(self.config, 'default', 'default')
        self.mainWindow = MainWindow.MainWindow(self)
        self.gtk_screen = self.mainWindow.get_screen()
        use_rgba_colormap = (gtk.gtk_version >= (2, 10, 0) and
                             self.config.glob['rgbaColormap'])
        if use_rgba_colormap:
            colormap = self.gtk_screen.get_rgba_colormap()
            if colormap:
                gtk.widget_set_default_colormap(colormap)
        self.mainWindow.realize()
        self.widget_style = self.mainWindow.get_style()
        self.unifiedParser = UnifiedParser(self.theme)
        self.conversationLayoutManager = \
            ConversationLayoutManager.ConversationLayoutManager(self)
        self.conversationManager = \
            ConversationManager.ConversationManager(self)
        self.contacts = ContactManager.ContactManager(dialog, None, None)
        self.groups = GroupManager.GroupManager(dialog, None)
        if use_rgba_colormap:
            gtk.widget_push_colormap(self.gtk_screen.get_rgb_colormap())
        if not TrayIcon.disabled:
            self.trayIcon = TrayIcon.TrayIcon(self)
        if use_rgba_colormap:
            gtk.widget_pop_colormap()
        if TrayIcon.disabled or not minimized:
            self.mainWindow.show()
        self.pluginManager = None
        gtk.gdk.set_program_class('emesene')
        gtk.window_set_default_icon_list(\
            self.theme.getImage('icon16'), self.theme.getImage('icon32'),
            self.theme.getImage('icon48'), self.theme.getImage('icon96'))
        self.autoReplyMessage = ''
        self.msn = None
        self.lastILN = 0
        self.pendingAvatars = {}
        self.trayDisconnect = None
        self.traySeparator = None
        self.trayStatusMenu = None
        self.avatar = None
        self.cancel = False
        self.reconnecting = False
        self.preference_open = False
        self.addBuddy = None
        self.safemode = safe
        self.leakdebug = leakdebug
        if username in self.mainWindow.login.users.getUsers():
            user = username
            self.mainWindow.login.getPreferences(user)
            pwd = self.mainWindow.login.getPass()
            status = self.mainWindow.login.getStatus()
        elif self.config.glob['autoLogin']:
            user = self.mainWindow.login.getUser()
            pwd = self.mainWindow.login.getPass()
            status = self.mainWindow.login.getStatus()
        else:
            user = ''
            pwd = ''
        if user != '' and pwd != '':
            self.login(user, pwd, status)
    def login(self, user, password, status):
        '''do the login'''
        self.reconnecting = (self.mainWindow.currentInterface == 'reconnect')
        self.cancel = False
        self.userEmail = user
        self.userStatus = status
        self.mainWindow.buildInterface('loading')
        self.config.setCurrentUser(user)
        if not self.config.glob['debug']:
            print "If you are reading this, you may want to enable debug"
            print "It's the first option in the connection tab in preferences"
        server = SERVER_HOST
        proxy = None
        if self.config.glob['httpMethod']:
            if self.config.glob['useProxy']:
                proxy = Socket.Proxy(self.config.glob['proxyHost'], \
                    self.config.glob['proxyPort'])
            else:
                proxy = Socket.Proxy()
        self.msn = core.Msnp(server, 1863, user, password,\
             self.config.getUserConfigPath(), proxy, self.safemode)
        self.msn.setDebug(self.config.glob['debug'], 
            self.config.glob['binary'])
        self.msn.connect('login-error', self.on_login_error, password)
        self.msn.connect('login-successful', self.on_login_successful)
        gobject.timeout_add(2000, self.trayicon_effect)
        self.msn.login()
    def trayicon_effect(self):
        '''Create an animation effect in the trayicon'''
        if not TrayIcon.disabled and (self.msn and not self.msn.connected):
            if self.trayIcon.status == 'login':
                self.trayIcon.update('disconnected')
            else:
                self.trayIcon.update('login')
        else:
            return False
    def on_login_error(self, msn, message, password):
        '''Callback to login-error signal'''
        if not self.msn:
            return False
        if not self.reconnecting:
            text = _('Error during login, please retry') + \
             '\n(' + message + ')'
            dialog.error(text)
            self.goToLoginWindow()
        else:
            self.autoReconnect(self.userEmail, password, self.userStatus)
    def cancelLogin(self):
        try:
            self.msn.logout()
        except Exception, e:
            print "Exception at logout", e
        self.goToLoginWindow()
    def goToLoginWindow(self):
        self.config.setCurrentUser('')
        self.mainWindow.buildInterface('login')
        self.msn = None
        self.pluginManager = None
    def on_login_successful(self, msn):
        self.connected = True
        self.pendingAvatars = {}
        self.idleTimeout = 0
        self.themeChanged()
        self.msn.changeStatus(self.userStatus)
        self.msn.connect('disconnected', self.disconnected)
        self.msn.connect('connection-closed', self.on_connection_closed)
        self.msn.connect('error', self.on_msnp_error)
        self.msn.connect('exception', self.on_msnp_exception)
        sys.excepthook = self.except_hook_dialog
        self.contacts = ContactManager.ContactManager(
                                dialog, self.msn, self.userEmail)
        self.groups = GroupManager.GroupManager(
                                dialog, self.msn)
        self.mainWindow.buildInterface('userlist')
        self.changeAvatar(self.config.user['avatarPath'])
        self.config.user['autoReply'] = False
        self.autoReplyMessage = self.config.user['autoReplyMessage']
        layout = self.config.user['conversationLayout']
        if not self.conversationLayoutManager.load(layout):
            self.conversationLayoutManager.setDefault()
        if not self.trayStatusMenu and not TrayIcon.disabled:
            statusMenuItem = gtk.MenuItem(_('_Status'))
            statusMenu = StatusMenu.StatusMenu(self)
            statusMenuItem.set_submenu(statusMenu)
            separator = gtk.SeparatorMenuItem()
            disconnect = gtk.ImageMenuItem(gtk.STOCK_DISCONNECT)
            disconnect.connect('activate', self.on_tray_disconnect)
            self.trayDisconnect = disconnect
            self.traySeparator = separator
            self.trayStatusMenu = statusMenuItem
        self.Slash = SlashCommands.SlashCommands(self)
        if not TrayIcon.disabled:
            self.trayIcon.menu.prepend(self.trayDisconnect)
            self.trayIcon.menu.prepend(self.traySeparator)
            self.trayIcon.menu.prepend(self.trayStatusMenu)
            self.trayIcon.menu.show_all()
            self.trayIcon.update(self.msn.status)
        self.pluginManager = PluginManager.PluginManager(self)
        plugins = self.config.user['activePlugins'].split(',')
        self.startPlugins(plugins)
        self.msn.connect('initial-status-change', self.onInitialStatusChange)
        self.msn.connect('display-picture-changed',
            self.on_display_picture_changed)
        self.msn.connect('new-conversation', self.newConversation)
        self.msn.connect('user-attr-changed', self.on_user_attr_changed)
        self.msn.connect('self-status-changed', self.on_self_status_changed)
        self.msn.connect('group-attr-changed', self.on_group_attr_changed)
        self.msn.connect('add-notification', self.addNotification)
        self.msn.connect('user-list-change', self.refreshUserList)
        self.msn.connect('user-disconnected', self.userDisconnected)
        self.msn.connect('connection-problem', self.connectionProblem)
        self.msn.connect('offline-message-waiting', self.offlineMessageWaiting)
        self.msn.connect('offline-message-received', 
            self.offlineMessageReceived)
        self.msn.connect('send-message-error', self.messageError)
        self.msn.connect('msnobj-changed', self.msnobjChanged)
        self.hotmail = Hotmail.Hotmail(self.msn, self.config)
        self.customEmoticons = CustomEmoticons(self.config, self)
        self.conversationManager.handleLogin(self.userEmail)
        gobject.idle_add(self.checkPending)
        self.cancel = False
        self.reconnecting = False
    def disconnected(self, msnp):
        '''called when a error occurs and we cant keep connected.'''
        self.logout(False, True)
    def on_connection_closed(self, msnp):
        '''called when a connection error happens'''
        self.logout(False, True)
    def on_msnp_error(self, msnp, error, description):
        '''called when there is an error during some msnp operation'''
        dialog.error(description)
    def on_msnp_exception(self, msnp, exception):
        '''called when there is an error in msn.process'''
        lines = traceback.format_exception(*exception)
        message = '<span weight="bold" size="larger">'
        message += _('Fatal error') 
        message += '\n</span>'
        message += _('This is a bug. Please report it at:')
        message += ' ' + 'http://www.emesene.org' + '\n'
        for line in lines:
            message += '\n' + emesenelib.common.escape(line)
        dialog.exception(message)
    def except_hook_dialog(self, *exception):
        '''replaces sys.excepthook displaying a dialog
        this is only for unhandled exceptions'''
        traceback.print_exception(*exception)
        if exception[0] != KeyboardInterrupt:
            gobject.idle_add(self.on_msnp_exception, None, exception)
    def on_user_attr_changed(self, userlist, contact):
        self.mainWindow.userList.updateContact(contact)
    def on_display_picture_changed(self, msnp, switchboard, msnobj, email):
        contact = self.getContact(email)
        if contact:
            self.mainWindow.userList.updateContact(contact)
        else:
            debug('contact not found on contact list!')
    def on_group_attr_changed(self, msnp, oldGroup, newGroup):
        self.refreshUserList()
    def autoReconnect(self, user, password, status):
        self.mainWindow.buildInterface('reconnect')
        self.mainWindow.login.setFieldValues(user, password, status)
        self.reconnectAfter = 30
        self.reconnectTimerId = gobject.timeout_add(1000, \
            self.updateReconnectTimer)
        self.updateReconnectTimer()
    def updateReconnectTimer(self):
        self.reconnectAfter -= 1
        self.mainWindow.login.setReconnectCounter(self.reconnectAfter)
        if self.reconnectAfter == 0:
            self.mainWindow.login.login()
            return False
        else:
            return True
    def cancelReconnect(self):
        gobject.source_remove(self.reconnectTimerId)
        self.reconnectAfter = None
        self.mainWindow.buildInterface('login')
    def logout(self, closeConversations = True, autoReconnect = False):
        '''do the logout'''
        traceback.print_stack()
        if closeConversations:
            self.conversationManager.closeAll()
        else:
            self.conversationManager.disableAll()
        if self.pluginManager:
            self.pluginManager.destroy()
        self.pluginManager = None
        self.hotmail = None
        self.customEmoticons = None
        if self.Slash:
            self.Slash.unregister_slash()
            self.Slash = None
        self.mainWindow.buildInterface('login')
        if autoReconnect and self.msn:
            status = emesenelib.common.reverse_status[self.msn.status]
            self.autoReconnect(self.msn.user, self.msn.password, status)
        self.connected = False
        self.contacts = ContactManager.ContactManager(dialog, None, None)
        self.groups = GroupManager.GroupManager(dialog, None)
        self.groups = None
        if not TrayIcon.disabled:
            try:
                self.trayIcon.menu.remove(self.trayDisconnect)
                self.trayIcon.menu.remove(self.traySeparator)
                self.trayIcon.menu.remove(self.trayStatusMenu)
                self.trayIcon.update('disconnected')
            except:
                pass
        if self.msn:
            self.msn.logout()
            weakmsn = weakref.ref(self.msn)
            self.msn = None
        else:
            return
        if weakmsn() != None:
            print "warning: there are still some references to emesenelib"
            print "references to msn:", sys.getrefcount(weakmsn()) -1
        else:
            print "yay msn got gc'd"
        if self.leakdebug:
            print "cheat sheet: r(o) == gc.get_referrers(o)"
            print "  to create a msn reference: weakmsn()"
            print
            import code, readline, gc
            r = gc.get_referrers
            code.InteractiveConsole(locals()).interact(\
                "Entering controller.msn leak debug console")
    def quit(self, status):
        '''close the window, and do all the things needed...'''
        self.mainWindow.hide()
        if not TrayIcon.disabled:
            self.trayIcon.tray.hide()
        try:
            self.mainWindow.saveToQuit()
        except:
            pass
        if self.connected:
            self.msn.logout()
            self.config.writeUserConfig()
        self.config.writeGlobalConfig()
        sys.exit(status)
    def setMediaEnabled(self, enabled=True):
        '''Set if can set media'''
        if enabled:
            self.config.user['mediaEnabled'] = True
        else:
            self.config.user['mediaEnabled'] = False 
        if not enabled:
            self.msn.changeCurrentMedia('')
    def checkPending(self):
        '''Check for users pending to be added'''
        if self.msn is None:
            return False
        if self.addBuddy is None:
            self.addBuddy = dialog.AddBuddy(self)
        users = self.msn.checkPending()
        if len(users) > 0:
            for mail in users:
                nick = self.msn.getUserDisplayName(mail)
                self.addBuddy.append(nick, mail)
        return False
    def startPlugins(self, plugins):
        '''Start the plugins in pluginList'''
        if plugins[ 0 ] != '':
            for i in plugins:
                try:
                    (success, message) = self.pluginManager.checkPlugin(i)
                except:
                    success = False
                    message = _('invalid check() return value')
                if not success:
                    emesenelib.common.debug(_('plugin %s could not be initialized, reason:')%i)
                    emesenelib.common.debug(message)
                else:
                    self.pluginManager.startPlugin(i)
    def set_picture_dialog(self):
        ''' Shows a dialog to change personal avatar '''
        def _on_picture_selected(response, path):
            '''method called on avatar selected'''
            if response == stock.ACCEPT:
                self.changeAvatar(path)
            elif response == stock.CLEAR:
                self.changeAvatar('')
        avatar_cache = self.config.getAvatarsCachePath()
        path_current = self.config.user['avatarPath']
        dialog.set_picture(path_current, avatar_cache, _on_picture_selected)
    def changeAvatar(self, filename):
        if not self.msn:
            return
        if not filename or not os.path.exists(filename):
            filename = ''
        try:
            self.avatar = Avatar.Avatar(filename,
                self.config.getAvatarsCachePath())
        except:  # may be GError
            return
        if filename == '':
            self.msn.setDisplayPicture('')
            self.mainWindow.setAvatar(self.theme.getImage('userPanel'))
            self.config.user['avatarPath'] = ''
        else:
            self.msn.setDisplayPicture(self.avatar.getImagePath())
            self.mainWindow.setAvatar(self.avatar.getThumb())
            self.config.user['avatarPath'] = self.avatar.getImagePath()
        self.emit('avatar-changed')
    def on_self_status_changed(self, msn, status):
        '''called when our status changes'''
        if not TrayIcon.disabled:
            self.trayIcon.update(self.msn.status)
    def addUserDialog(self, toGroup=None):
        '''show a dialog requesting the email of the ser to be added'''
        def add_contact_cb(response, account='', group=''):
            '''callback for the add_contact dialog'''
            if response == stock.ACCEPT:
                if len(account.split('@')) == 2:
                    self.contacts.add(account, group)
                else:
                    dialog.warning(_("Invalid account"))
        dialog.add_contact(self.groups.groups.keys(), None, add_contact_cb)
    def addNotification(self, msnp, command, tid, params, email, nick):
        '''this method is called when a user adds you'''
        if self.addBuddy is None:
            self.addBuddy = dialog.AddBuddy(self)
        self.addBuddy.append(nick, email)
    def themeChanged(self):
        '''called when the theme changes'''
        theme = self.config.user['theme']
        isSmall = self.config.user['smallIcons']
        smilieTheme = self.config.user['smilieTheme']
        error = False
        if not self.theme.setTheme(theme, isSmall):
            error = True
        if not self.theme.smilies.setTheme(smilieTheme):
            error = True
        if error:
            dialog.error(
                _("Error loading theme, falling back to default"))
            self.config.user['theme'] = 'default'
            self.config.user['smilieTheme'] = 'default'
            theme = 'default'
            stheme = 'default'
        self.mainWindow.rebuild()
        if self.trayStatusMenu:
            self.trayStatusMenu.set_submenu(StatusMenu.StatusMenu(self))
            self.trayIcon.menu.show_all()
            self.trayIcon.update(self.msn.status)
        return theme
    def refreshUserList(self, msnp=None, force=False):
        '''call to refreshUserList on MainWindow that calls refresh on userList
        userList get the values of showOffline etc from config, so if you want
        to change something, change it on config.
        if it's loading contacts (ILN), don't do anything'''
        if self.lastILN == 0 or time.time() - self.lastILN > 1:
            self.mainWindow.refreshUserList(force)
            self.lastILN = 0
    def connectionProblem(self, msnp):
        '''called when a PNG is not answered'''
        self.logout(False, True)
    def onInitialStatusChange(self, msnp, command, tid, params):
        '''callback for ILN messages. saves the timestamp to speed up login'''
        self.lastILN = time.time()
    def onStatusChange(self, msnp, command, tid, params):
        '''callback for the status change event'''
        self.refreshUserList()
    def on_tray_disconnect(self, *args):
        self.logout()
    def msnobjChanged(self, msnp, msnobj, wasOffline):
        '''callback called when the user change his msnobj'''
        if msnobj != None:
            creator = msnobj.getCreator()
            contact = self.getContact(creator)
            if (not creator or not contact) or \
               self.theme.hasUserDisplayPicture(contact):
                return
            self.pendingAvatars[msnobj.getCreator()] = msnobj
            if not self.idleTimeout:
                self.idleTimeout = gobject.timeout_add(5000, \
                    self.idleadd, priority=gobject.PRIORITY_LOW)
    def idle(self):
        '''called when the gobject mainloop is idle
        gobjects thinks that it means "all the time" so we add a
        five-second non-blocking delay'''
        stop = True
        if len(self.pendingAvatars.keys()) > 0 and self.msn:
            creator = self.pendingAvatars.keys().pop(0)
            msnobj = self.pendingAvatars[creator]
            contact = self.getContact(creator)
            requested = False
            if contact and not self.theme.hasUserDisplayPicture(contact):
                sb = self.msn.getSwitchboard(creator)
                sb.getDisplayPicture(creator)
                requested = True
            elif contact:
                self.msn.emit("display-picture-changed", None, msnobj, creator)
            del self.pendingAvatars[creator]
            stop = len(self.pendingAvatars.keys()) == 0
            if not stop and not requested:
                return self.idle()
        if not stop:
            self.idleTimeout = gobject.timeout_add(20000, \
                self.idleadd, priority=gobject.PRIORITY_LOW)
        return False
    def idleadd(self, *args):
        '''called 5 seconds after last idle call'''
        self.idleTimeout = 0
        gobject.idle_add(self.idle, priority=gobject.PRIORITY_LOW)
        return False
    def newConversation(self, msnp, mail, switchboard=None, weStarted=False):
        '''This method is called when the user want to initiate a new
        conversation with someone, or when a switchboard has been created
        and we want to give a window to it.
        (callback for 'new conversation' signal emmited by msnp)
        weStarted is a boolean that indicate if we started the conversation or 
        a friend'''
        return self.conversationManager.newConversation(self.msn, mail, \
            switchboard, weStarted)
    def pluginDialog(self):
        '''Show the pluginManagerDialog'''
        p = PluginManagerDialog.PluginManagerDialog(self.pluginManager, \
            self.mainWindow, self.config)
        p.show()
    def seeSpace(self, email):
        '''gets the space url and opens it in a browser'''
        self.msn.getUserProfile(email)
    def seeProfile(self, email):
        '''opens the profile url in a browser'''
        desktop.open('http://members.msn.com/' + email)
    def offlineMessageWaiting(self, msnp, msnOIM):
        '''process the OIM messages'''
        for i in msnOIM.getMessages():
            msnOIM.retrieve(i[ 'I' ])
    def offlineMessageReceived(self, msnp, oim):
        '''process the OIM message'''
        user, date, message = oim
        window, conv = self.newConversation(msnp, user['addr'], None, False)
        conv.receiveOIM(user['name'], message, date)
    def messageError(self, msnp, to, message, error):
        window, conv = self.newConversation(self.msn, to, None, True)
        conv.receiveError(msnp, to, message, error)
    def userDisconnected(self, msnp, tid, params):
        '''method called when the server disconnect us'''
        self.logout(False, False)
        if tid == 'OTH':
            message = _('Logged in from another location.')
        else:
            message = _('The server has disconnected you.')
        dialog.error(message)
    def getMenuData(self):
        return self.mainWindow.userList.getMenuData()
    def getUnreadMails(self):
        if self.msn:
            return self.msn.inboxUnreadMessages
        else:
            return 0
    def getContact(self, mail):
        '''return a contact object'''
        if not self.msn:
            return None
        return self.msn.contactManager.getContact(mail)
    def updateDebug(self, *args):
        if self.msn:
            self.msn.setDebug(self.config.glob['debug'], \
                self.config.glob['binary'])
def parseArgs():
    '''parses sys.argv with getopt
    returns tuple with Controller args'''
    username = ''
    minimized = False
    safe = False
    leakdebug = False
    try:
        import getopt
    except ImportError:
        return ('', False, False, False)
    shortArgs = 'm'
    longArgs = ['minimized', 'user=', 'safe', 'leakdebug']
    try:
        args = getopt.getopt(sys.argv[1:], shortArgs, longArgs)
    except getopt.GetoptError, e:
        print e
        print 'Usage:', sys.argv[0], '[-m|--minimized] [--user=mail@address]'
        print 'Advanced options: --safe, --leakdebug'
        sys.exit(1)
    for key, value in args[0]:
        if key == '--user':
            username = value
        if key == '-m' or key == '--minimized':
            minimized = True
        if key == '--safe':
            safe = True
        if key == '--leakdebug':
            leakdebug = True
    return (username, minimized, safe, leakdebug)
def debug(msg):
    '''print a debug message'''
    print 'Controller: ' + msg
def main():
    if os.name == 'posix' and os.getuid() == 0:
        print "I refuse to run as root"
        return
    try:
        path = os.path.dirname(__file__) or sys.path[0]
    except NameError:
        path = sys.path[0]
    originalPath = os.getcwd()
    os.chdir(path)
    sys.path.append(path)
    controller = Controller(*parseArgs())
    for i in range(5):
        try:
            gtk.main()
        except KeyboardInterrupt:
            print 'Interrupt (%s more times to close)' % str(5 - i)
        else:
            break
    controller.quit(0)
    os.chdir(originalPath)
if __name__ == '__main__':
    main()
