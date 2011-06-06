import os
import gtk
import gobject
import base64 # yes base64, go out and look how gaim does it :P
import paths
import Widgets
import dialog
import emesenelib.common
class Login(gtk.VBox):
    '''This class represents the login widget where you insert the user,
    pass and status'''
    def __init__(self , controller, interface):
        '''Constructor'''
        gtk.VBox.__init__(self)
        self.controller = controller
        self.set_spacing(20)
        self.set_border_width(20)
        self.buildInterface(interface)
    def buildInterface(self, interface):
        self.users = UserManager(os.path.join(paths.CONFIG_DIR, 'users.dat'))
        self.currentInterface = interface
        lUser = gtk.Label(_('_User:'))
        lPass = gtk.Label(_('_Password:'))
        self.lCaps = gtk.Label('')
        lStatus = gtk.Label(_('_Status:'))
        lUser.set_use_underline(True)
        lPass.set_use_underline(True)
        lStatus.set_use_underline(True)
        self.userListStore = gtk.ListStore(gobject.TYPE_STRING, gtk.gdk.Pixbuf)
        self.tUser = gtk.ComboBoxEntry(self.userListStore, 0)
        self.tUser.connect("changed", self.on_comboxEntry_changed)
        self.tUser.connect("key-release-event", self.on_comboxEntry_keyrelease)
        self.userCompletion = gtk.EntryCompletion()
        self.userCompletion.set_model(self.userListStore)
        self.tUser.get_children()[0].set_completion(self.userCompletion)
        self.userCompletion.connect('match-selected', self.matchSelected)
        self.userPixbufCell = gtk.CellRendererPixbuf()
        self.userCompletion.pack_start(self.userPixbufCell)
        self.userCompletion.add_attribute(self.userPixbufCell, 'pixbuf', 1)
        self.userCompletion.set_text_column(0)
        self.statusListStore = gtk.ListStore(gtk.gdk.Pixbuf,
                                              gobject.TYPE_STRING,
                                              gobject.TYPE_STRING)
        self.tStatus = gtk.ComboBox(self.statusListStore)
        self.statusPixbufCell = gtk.CellRendererPixbuf()
        self.statusTextCell = gtk.CellRendererText()
        self.tStatus.pack_start(self.statusPixbufCell, False)
        self.tStatus.pack_start(self.statusTextCell, False)
        self.statusPixbufCell.set_property('xalign', 0.0)
        self.statusPixbufCell.set_property('xpad', 5)
        self.statusTextCell.set_property('xalign', 0.0)
        self.statusTextCell.set_property('xpad', 5)
        self.tStatus.add_attribute(self.statusTextCell, 'text', 2)
        self.tStatus.add_attribute(self.statusPixbufCell, 'pixbuf', 0)
        self.tPass = gtk.Entry(128)
        self.tPass.set_visibility(False)
        self.tPass.connect('activate' , self.bLogin_clicked)
        self.tPass.connect('key-press-event', self.on_password_keypressed)
        lUser.set_mnemonic_widget(self.tUser)
        lPass.set_mnemonic_widget(self.tPass)
        lStatus.set_mnemonic_widget(self.tStatus)
        self.statusList = []
        j = 0
        for i in self.controller.status_ordered[ 1 ]:
            if i != 'offline':
                self.statusListStore\
                        .append([ self.controller.theme.statusToPixbuf(
                               emesenelib.common.status_table[i]), i,
                               _(self.controller.status_ordered[2][ j]) ])
                self.statusList.append(i)
                j += 1
        self.tStatus.set_active(0)
        pixbuf = self.controller.theme.getImage('login', False)
        self.loginImage = Widgets.avatarHolder()
        self.loginImage.set_from_pixbuf(pixbuf)
        self.pack_start(self.loginImage , True , False)
        self.fieldsBox = gtk.VBox(spacing=10)
        userBox = gtk.VBox(spacing=4)
        lUser.set_alignment(0.0, 0.5)
        userBox.pack_start(lUser , False , False)
        userBox.pack_start(self.tUser , False , False)
        self.fieldsBox.pack_start(userBox, False, False)
        passBox = gtk.VBox(spacing=4)
        passLBox = gtk.HBox(spacing=6)
        lPass.set_alignment(0.0, 0.5)
        passLBox.pack_start(lPass , False , False)
        passLBox.pack_start(self.lCaps , False , False)
        passBox.pack_start(passLBox , False , False)
        passBox.pack_start(self.tPass , False , False)
        self.fieldsBox.pack_start(passBox, False, False)
        statusBox = gtk.VBox(spacing=4)
        lStatus.set_alignment(0.0, 0.5)
        statusBox.pack_start(lStatus , False , False)
        statusBox.pack_end(self.tStatus, True, True)
        self.fieldsBox.pack_start(statusBox, False, False)
        fieldsAlig = gtk.Alignment(0.5, 0.5, 0.75, 0.0)
        fieldsAlig.add(self.fieldsBox)
        self.pack_start(fieldsAlig, True, False)
        buttonBox = gtk.HButtonBox()
        if interface == 'login':
            self.bLogin = gtk.Button(_('_Login'), gtk.STOCK_CONNECT)
            self.bLogin.connect('clicked' , self.bLogin_clicked)
            buttonBox.pack_start(self.bLogin, False, False)
            self.cRemember = gtk.CheckButton(_('_Remember me'), True)
            self.cRemember.connect('toggled' , self.on_cRemember_toggled)
            self.forgetMe = gtk.EventBox()
            self.forgetMe.set_events(gtk.gdk.BUTTON_PRESS_MASK)
            self.forgetMeLabel = gtk.Label('<span foreground="#0000AA">(' + \
                                            _('Forget me') + ')</span>')
            self.forgetMeLabel.set_use_markup(True)
            self.forgetMe.add(self.forgetMeLabel)
            self.forgetMe.connect('button_press_event', self.onForgetMe)
            self.forgetMe.set_child_visible(False)
            self.cRememberPass = gtk.CheckButton(_('R_emember password'), True)
            self.cRememberPass.connect('toggled' , self.on_cRememberPass_toggled)
            self.cAutoLogin = gtk.CheckButton(_('Auto-Login'), True)
            self.cAutoLogin.connect('toggled' , self.on_cAutoLogin_toggled)
            if self.controller.config.glob['rememberMe']:
                self.cRemember.set_active(True)
            if self.controller.config.glob['rememberMyPassword']:
                self.cRememberPass.set_active(True)
            self.checkBox = gtk.VBox(spacing=4)
            rememberBox = gtk.HBox(spacing=4)
            rememberBox.pack_start(self.cRemember , False , False)
            rememberBox.pack_start(self.forgetMe , False , False)
            self.checkBox.pack_start(rememberBox, False, False)
            self.checkBox.pack_start(self.cRememberPass , False , False)
            self.checkBox.pack_start(self.cAutoLogin , False , False)
            checkAlig = gtk.Alignment(0.5, 0.5)
            checkAlig.add(self.checkBox)
            self.pack_start(checkAlig , True , False)
        elif interface == 'reconnect':
            self.fieldsBox.set_sensitive(False)
            self.lConnectionError = gtk.Label()
            self.lConnectionError.set_markup('<b>' + _('Connection error') + '</b>')
            self.lReconnectCounter = gtk.Label()
            self.bCancelReconnect = gtk.Button(_('Cancel'), gtk.STOCK_CANCEL)
            self.bCancelReconnect.connect('clicked', self.onCancelReconnect)
            self.pack_start(self.lConnectionError, True, False)
            self.pack_start(self.lReconnectCounter, True, False)
            buttonBox.pack_start(self.bCancelReconnect, False, False)
        elif interface == 'loading':
            self.fieldsBox.set_sensitive(False)
            pixbuf = self.controller.theme.getImage('loading', True)
            self.loading = gtk.image_new_from_animation(pixbuf)
            cancelButton = gtk.Button(_('Cancel'), gtk.STOCK_CANCEL)
            cancelButton.connect('clicked', self.onCancelLogin)
            buttonBox.pack_start(cancelButton, False, False)
            self.pack_start(self.loading, False, False)
        self.refreshUserList()
        if self.users.userExists(self.controller.config.glob['lastLoggedAccount']):
                self.tUser.get_children()[0].set_text(self.controller.config.glob['lastLoggedAccount'])
        self.pack_start(buttonBox, True, False)
        self.show_all()
        self.tUser.grab_focus()
    def setFieldValues(self, user, password, status):
        self.tUser.prepend_text(user)
        self.tUser.set_active(0)
        self.tPass.set_text(password)
        statusOrdered = self.controller.status_ordered[1]
        if status in statusOrdered:
            self.tStatus.set_active(statusOrdered.index(status))
    def onCancelReconnect(self, *args):
        self.controller.cancelReconnect()
    def onCancelLogin(self,*args):
        self.controller.cancelLogin()
    def refreshUserList(self):
        '''fill the user list to the completion and combo'''
        self.userListStore.clear()
        for i in sort(self.users.getUsers()):
            self.userListStore.append([ i , self.controller.theme.getImage('online') ])
    def on_cRemember_toggled(self , *args):
        if not self.cRemember.get_active():
            self.cRememberPass.set_active(False)
    def on_cRememberPass_toggled(self , *args):
        if self.cRememberPass.get_active():
            self.cRemember.set_active(True)
        else:
            self.cAutoLogin.set_active(False)
            if self.users.userExists(self.getUser()):
                if self.users.getPassword(self.getUser()) != '':
                    self.tPass.set_sensitive(True)
                    self.tPass.set_text('')
                    self.users.resetPassword(self.getUser())
                    self.users.save()
    def on_cAutoLogin_toggled(self , *args):
        if self.cAutoLogin.get_active():
            self.controller.config.glob['autoLogin'] = True
            self.cRememberPass.set_active(True)
        else:
            self.controller.config.glob['autoLogin'] = False
    def setReconnectCounter(self, seconds):
        if seconds > 0:
            self.lReconnectCounter.set_text(_('Reconnecting in ') + str(seconds) + _(' seconds'))
        else:
            self.lReconnectCounter.set_text(_('Reconnecting...'))
    def login(self):
        self.controller.login(self.getUser() , self.getPass() , self.getStatus())
    def bLogin_clicked(self , *args):
        if self.getUser() == '' or self.getPass() == '' or self.getStatus() == '':
            dialog.error(_("User or password empty"))
            return
        if self.currentInterface == 'login' and self.cRemember.get_active():
            if self.cRememberPass.get_active():
                self.users.addUser(self.getUser(), base64.b16encode(self.getPass()), self.getStatus())
            else:
                self.users.addUser(self.getUser(),'',self.getStatus())
        else:
            self.users.removeUser(self.getUser())
        self.users.save()
        self.login()
    def getUser(self):
        self.active_user = self.tUser.get_active()
        return self.tUser.get_active_text()
    def getPass(self):
        return self.tPass.get_text()
    def getStatus(self):
        return self.statusListStore.get(self.tStatus.get_active_iter(), 1)[ 0 ]
    def on_comboxEntry_changed(self, *args):
        self.getPreferences(self.getUser())
    def matchSelected(self, completion=None, model=None, iter=None):
        '''this callback is called when a user is selected with autocompletion
        then i check if the password is stored.'''
        userInEntry = model.get(iter, 0)[ 0 ]
        self.getPreferences(userInEntry)
    def getPreferences(self, user):
        if not self.users.userExists(user):
            self.loginImage.set_from_pixbuf(self.controller.theme.getImage('login', False))
            self.tPass.set_text('')
            self.tPass.set_sensitive(True)
            self.tStatus.set_active(self.statusList.index('online'))
            if self.currentInterface == 'login':
                self.cRemember.set_active(False)
                self.forgetMe.set_child_visible(False)
        else:
            password = self.users.getPassword(user)
            status = self.users.getStatus(user)
            avatar = self.getUserAvatarPath(user)
            try:
                self.loginImage.set_from_file(avatar)
            except gobject.GError:
                pass
            except: # everything else, like open() problems
                pass
            if self.currentInterface == 'login':
                self.cRemember.set_active(True)
                self.forgetMe.set_child_visible(True)
            if password != '':
                self.tPass.set_text(base64.b16decode(password))
                if self.currentInterface == 'login':
                    self.cRememberPass.set_active(True)
                    self.tPass.set_sensitive(False)
            else:
                self.tPass.set_text('')
                if self.currentInterface == 'login':
                    self.cRememberPass.set_active(False)
                    self.tPass.set_sensitive(True)
            if status != '':
                try:
                    self.tStatus.set_active(self.statusList.index(status) )
                except:
                    pass
    def getUserAvatarPath(arg, user):
        user = user.replace('@', '_').replace('.', '_')
        try:
            s = open(os.path.join(paths.CONFIG_DIR, user, 'config'), 'r').read()
            s.replace(' ', '')
            path = s.split('avatarPath=')[1].split('\n')[0]
        except Exception, e:
            path = paths.DEFAULT_THEME_PATH + 'login.png'
        if os.path.isfile(path):
            return path
        return paths.DEFAULT_THEME_PATH + 'login.png'
    def onForgetMe(self, *args):
        '''This method is called when the user press the forget me event box'''
        self.users.removeUser(self.getUser())
        self.tUser.get_children()[0].set_text('')
        self.tPass.set_text('')
        self.forgetMe.set_child_visible(False)
        self.refreshUserList()
        self.users.save()
    def on_comboxEntry_keyrelease(self, widget, event):
        if event.keyval == gtk.keysyms.Tab:
            self.tPass.grab_focus()
        if event.keyval == gtk.keysyms.Return:
            self.login()
    def on_password_keypressed(self, widget, event):
        if (event.state & 0x00000002) == 2: # Verify caps lock state
            warning = _('Attention: Your capslock is enabled!')
            self.lCaps.set_markup(
                '<span foreground="#FF0000">%s</span>' % warning)
        elif self.lCaps.get_text():
            self.lCaps.set_text('')
class UserManager(object):
    '''This class manages the file that hold the user:password data
    the format of users.dat is:
    mail:password:status
    the password is encrypted in a magic way :P'''
    def __init__(self, path):
        '''Contructor'''
        self.path = path
        self.users = {}
        self.load()
    def getUsers(self):
        '''return a list of users'''
        return self.users.keys()
    def getPassword(self, user):
        '''return the password of the user'''
        if user in self.users:
            return self.users[user][0]
        else:
            return ''
    def getStatus(self, user):
        '''return the status of the user'''
        if user in self.users:
            return self.users[user][1]
        else:
            return 'NLN'
    def addUser(self, user, password = '', status = 'online'):
        '''add a user to the dictionary'''
        if password == '' and \
           user in self.users and \
           self.users[user][0] != '':
            password = self.users[user][0]
        self.users[user] = (password, status)
    def removeUser(self, user):
        '''remove the user from the dictionary'''
        if user in self.users:
            del self.users[user]
    def userExists(self, user):
        '''checks if the user exists'''
        if user in self.users:
            return True
        return False
    def resetPassword(self, user):
        '''reset the user password'''
        if user in self.users:
            self.users[user] = ('', self.users[user][1])
    def save(self):
        '''save the values to the file'''
        users = file(self.path, 'w')
        for user in self.users.keys():
            users.write(user + ':')
            users.write(self.users[ user ][ 0 ] + ':')
            users.write(self.users[ user ][ 1 ] + '\n')
        users.close()
    def load(self):
        '''load the content of the file'''
        justCreated = False
        try:
            usersList = open(self.path, 'r')
        except:
            usersList = open(self.path , 'w')
            justCreated = True
        if not justCreated:
            for line in usersList.readlines():
                (user,password,status) = line.strip().split(':')
                self.users[ user ] = (password, status)
        usersList.close()
def sort(list):
    '''Sorts a list in alphabetical order
    without regard of the capitalization'''
    list = list[:]
    list.sort(key=lambda x: str.lower(x))
    return list
