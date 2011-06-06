import os
import gtk
import gobject
import base64 # yes base64, go out and look how gaim does it :P
import Widgets
import dialog
import emesenelib.common
from emesenecommon import PATH, sort
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
        self.currentInterface = interface
        lUser = gtk.Label(_('_User:'))
        lPass = gtk.Label(_('_Password:'))
        lStatus = gtk.Label(_('_Status:'))
        lUser.set_use_underline(True)
        lPass.set_use_underline(True)
        lStatus.set_use_underline(True)
        self.tUser = gtk.combo_box_entry_new_text()
        self.userListStore = gtk.ListStore(gobject.TYPE_STRING, gtk.gdk.Pixbuf)
        self.userCompletion = gtk.EntryCompletion()
        self.tUser.get_children()[0].set_completion(self.userCompletion)
        self.userCompletion.set_model(self.userListStore)
        self.userPixbufCell = gtk.CellRendererPixbuf()
        self.userCompletion.pack_start(self.userPixbufCell)
        self.userCompletion.add_attribute(self.userPixbufCell, 'pixbuf', 1)
        self.userCompletion.set_text_column(0)
        self.userCompletion.connect('match-selected', self.matchSelected)
        self.tUser.connect("changed", self.on_comboxEntry_changed)
        self.tUser.connect("key-release-event", self.on_comboxEntry_keyrelease)
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
        lPass.set_alignment(0.0, 0.5)
        passBox.pack_start(lPass , False , False)
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
        self.pack_start(buttonBox, True, False)            
        self.fillUserList()
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
    def fillUserList(self):
        '''fill the user list to the completion'''
        self.users = UserManager(os.path.join(PATH.CONFIG_DIR, 'users.dat'))
        j=0
        for i in sort(self.users.getUsers()):
            self.tUser.append_text(i)
            self.userListStore.append([ i , self.controller.theme.getImage('online') ])    
            if self.controller.config.glob['lastLoggedAccount'] == i:
                self.getPreferences(i)
                self.tUser.set_active(j)
            j+=1
    def on_cRemember_toggled(self , *args):
        self.controller.config.glob['rememberMe'] = self.cRemember.get_active()
    def on_cRememberPass_toggled(self , *args):
        if self.cRememberPass.get_active():
            self.controller.config.glob['rememberMyPassword'] = True
            self.cRemember.set_active(True)
            self.cRemember.set_sensitive(False)
        else:
            self.controller.config.glob['rememberMyPassword'] = False
            self.cRemember.set_sensitive(True)
        self.on_cRemember_toggled()
    def on_cAutoLogin_toggled(self , *args):
        if self.cAutoLogin.get_active():
            self.controller.config.glob['autoLogin'] = True
            self.cRememberPass.set_active(True)
            self.cRememberPass.set_sensitive(False)
        else:
            self.controller.config.glob['autoLogin'] = False
            self.cRememberPass.set_sensitive(True)
        self.on_cRememberPass_toggled()
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
        password = self.users.getPassword(user)
        status = self.users.getStatus(user)
        avatar = self.getUserAvatarPath(user)      
        try:
            self.loginImage.set_from_file(avatar)
        except gobject.GError:
            pass
        except: # everything else, like open() problems
            pass
        if password != '':
            self.tPass.set_text(base64.b16decode(password))
        else:
            self.tPass.set_text('')
            if self.currentInterface == 'login': 
                self.cRememberPass.set_active(False)
        if status != '':
            try:
                self.tStatus.set_active(self.statusList.index(status) )
            except:
                pass
    def getUserAvatarPath(arg, user):
        user = user.replace('@', '_').replace('.', '_')
        try:
            s = open(os.path.join(PATH.CONFIG_DIR, user, 'config'), 'r').read()
            s.replace(' ', '')
            path = s.split('avatarPath=')[1].split('\n')[0]
        except Exception, e:
            path = PATH.DEFAULT_THEME_PATH + 'login.png'
        if os.path.isfile(path):
            return path
        return PATH.DEFAULT_THEME_PATH + 'login.png'
    def onForgetMe(self, *args):
        '''This method is called when the user press the forget me event box'''
        self.tUser.remove_text(self.active_user)
        self.users.removeUser(self.getUser())
        self.tUser.get_children()[0].set_text('')
        self.tPass.set_text('')
        self.users.save()
    def on_comboxEntry_keyrelease(self, widget, event):
        if event.keyval == gtk.keysyms.Tab:
            self.tPass.grab_focus()
        if event.keyval == gtk.keysyms.Return:
            self.login()
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
        if self.users.has_key(user):
            return self.users[ user ][ 0 ]
        else:
            return ''
    def getStatus(self, user):
        '''return the status of the user'''
        if self.users.has_key(user):
            return self.users[ user ][ 1 ]
        else:
            return 'NLN'
    def addUser(self, user, password = '', status = 'online'):
        '''add a user to the dictionary'''
        if password == '' and \
           self.users.has_key(user) and \
           self.users[ user ][ 0 ] != '':
            password = self.users[ user ][ 0 ]
        self.users[ user ] = (password, status)
    def removeUser(self, user):
        '''remove the user from the dictionary'''
        if self.users.has_key(user):
            del self.users[ user ]
    def save(self):
        '''save the values to the file'''
        users = file(self.path, 'w')
        for user in self.users.keys():
            users.write(user + ':')
            users.write(self.users[ user ][ 0 ] + ':')
            users.write(self.users[ user ][ 1 ] + '\n')
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
