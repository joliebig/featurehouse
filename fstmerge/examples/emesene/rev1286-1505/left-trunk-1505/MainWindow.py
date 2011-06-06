import sys
import Login
import TrayIcon
import MainMenu
import UserList
import UserPanel
import FilterEntry
import emesenelib.common
try:
    import gtk
    import gobject
except:
    print 'you need pyGTK to run emesene'
    sys.exit(-1)
class MainWindow(gtk.Window):
    '''
    This class represent the main window of emesene,
    it inherit from gtk.Window
    and basically it is a container for other classes.
    '''
    __gsignals__ = {
        'gui-build-done' :
            (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
                (gobject.TYPE_STRING,)),
    }
    def __init__(self, controller):
        '''Constructor'''
        gtk.Window.__init__(self)
        self.controller = controller
        self.config = controller.config
        accelGroup = gtk.AccelGroup()
        self.add_accel_group(accelGroup)
        self.accelGroup = accelGroup
        accelGroup.connect_group(ord('M'), gtk.gdk.CONTROL_MASK, \
            gtk.ACCEL_LOCKED, self.on_toggle_menu)
        self.set_title('emesene')
        self.set_role('main')
        self.x = self.config.glob['mainWindowX']
        self.y = self.config.glob['mainWindowY']
        self.set_geometry_hints(self, -1, -1)
        self.width = self.config.glob['mainWindowWidth']
        self.height = self.config.glob['mainWindowHeight']
        self.set_default_size(self.width , self.height)
        self.connect('size-allocate', self.on_size_alloc)
        theme = controller.theme
        gtk.window_set_default_icon_list(theme.getImage('icon16'),
                                          theme.getImage('icon32'),
                                          theme.getImage('icon48'),
                                          theme.getImage('icon96'))
        self.vbox = None
        self.login = None
        self.userList = None
        self.currentInterface = 'login'
        self.buildInterface('login')
        self.itemSelectedId = 0
        self.signals = []
        sap = self.signals.append
        sap(self.config.connect('change::showUserPanel',
            self.updateConfig))
        sap(self.config.connect('change::showSearchEntry',
            self.updateConfig))
        sap(self.config.connect('change::showStatusCombo',
            self.updateConfig))
        sap(self.config.connect('change::showMenu',
            self.updateConfig))
        sap(self.config.connect('change::userListAvatarSize',
            self.updateSize))
        sap(self.config.connect('change::smallIcons', self.updateSize))
    def on_size_alloc(self, widget, allocation):
        self.width = allocation.width
        self.height = allocation.height
    def on_toggle_menu(self, *args):
        self.config.user['showMenu'] = not self.config.user['showMenu']
    def saveToQuit(self):
        '''Saves configuration parametes and everything needed ir order
        to safely quit'''
        if self.userList:
            try:
                self.userList.saveGroupState()
            except:
                pass
        self.controller.config.glob['mainWindowWidth'] = self.width
        self.controller.config.glob['mainWindowHeight'] = self.height
        self.controller.config.glob['mainWindowX'] = self.x
        self.controller.config.glob['mainWindowY'] = self.y
    def quit(self, status = 0):
        '''close the window, and do all the things needed...'''
        self.controller.quit(status)
    def show(self):
        '''Shows itself'''
        if not (self.x <= 0 or self.y <= 0):
            self.move(self.x, self.y)
        gtk.Window.show(self)
    def hide(self):
        '''Hides itself and any other window'''
        if self.get_property('visible'):
            self.x, self.y = self.get_position()
        if self.userList:
            self.userList.tooltips.hideTooltip()
        gtk.Window.hide(self)
    def hideOrClose(self, *args):
        '''hide or close depending if we have trayicon'''
        if TrayIcon.disabled:
            self.quit(0)
        else:
            self.hide()
        return True
    def buildInterface(self , guiType = 'login'):
        '''build the interface depending on the guiType parameter, by
        default build the login window.'''
        if self.login:
            user = self.login.getUser()
            passwd = self.login.getPass()
            status = self.login.getStatus()
        else:
            user = passwd = status = ''
        if self.get_child():
            self.remove(self.vbox)
        if self.userList:
            self.userList.tooltips.hideTooltip()
            self.userList.disconnect(self.itemSelectedId)
        if guiType == 'userlist' and self.login:
            try:
                self.login.remove(self.login.loginImage)
            except:
                emesenelib.common.debug('Error when removing loginImage')
        self.currentInterface = guiType
        self.vbox = gtk.VBox(spacing=2)
        self.scroll = gtk.ScrolledWindow()
        self.scroll.set_policy(gtk.POLICY_NEVER , gtk.POLICY_AUTOMATIC)
        self.scroll.set_shadow_type(gtk.SHADOW_IN)
        self.menu = MainMenu.MainMenu(self.controller, \
                                       guiType, self.accelGroup)
        if guiType == 'login':
            self.vbox.pack_start(self.menu, False, False)
            self.login = Login.Login(self.controller, 'login')
            self.vbox.pack_start(self.login)
        elif guiType == 'userlist':
            self.login = None
            self.vbox.pack_start(self.menu, False, False)
            self.userList = UserList.UserList(self.controller, \
                self.controller.theme, self.controller.config)
            self.itemSelectedId = self.userList.connect('item-selected',
                self.onItemSelected)
            self.userPanel = UserPanel.UserPanel(self.controller)
            self.vbox.pack_start(self.userPanel, False, False)
            self.filterEntry = FilterEntry.FilterEntry(
                self.userList.setFilterText)
            self.vbox.pack_start(self.filterEntry, False, False)
            self.scroll.add(self.userList)
            vbox2 = gtk.VBox()
            vbox2.pack_start(self.scroll)
            self.statusCombo = StatusCombo(self.controller)
            vbox2.pack_start(self.statusCombo, False, False)
            vbox2.set_border_width(2)
            self.vbox.pack_start(vbox2)
            vbox2.show_all()
            self.controller.connect('preferences-changed',
                self.updateConfig)
        elif guiType == 'loading':
            self.menu.set_sensitive(False)
            self.login = Login.Login(self.controller, 'loading')
            self.login.setFieldValues(user, passwd, status)
            self.vbox.pack_start(self.menu, False, False)
            self.vbox.pack_start(self.login)
            self.menu.show_all()
        elif guiType == 'reconnect':
            self.menu.set_sensitive(False)
            self.login = Login.Login(self.controller, 'reconnect')
            self.vbox.pack_start(self.menu, False, False)
            self.vbox.pack_start(self.login)
            self.menu.show_all()
        self.menu.show_all()
        self.add(self.vbox)
        self.vbox.show()
        self.update(self.controller)
        self.connect('delete-event' , self.hideOrClose)
        self.emit('gui-build-done', guiType)
        self.connect('key-press-event', self.on_key_press)
    def on_key_press(self, widget, event):
        if gtk.keysyms.Escape == event.keyval:
            self.hideOrClose()
    def updateConfig(self, *args):
        self.update(self.controller, False)
    def updateSize(self, config, value, oldvalue):
        if value != oldvalue and self.userList:
            self.userList.fill()
    def update(self, controller, refresUserList=True):
        if not controller or self.currentInterface != 'userlist':
            return
        if not self.config.user['showUserPanel']:
            self.userPanel.hide()
        else:
            self.userPanel.show()
        if not self.config.user['showSearchEntry']:
            self.filterEntry.hide()
        else:
            self.filterEntry.show()
        if not self.config.user['showStatusCombo']:
            self.statusCombo.hide()
        else:
            self.statusCombo.show()
        if not self.config.user['showMenu']:
            self.menu.hide();
        else:
            self.menu.show();
        if self.config.user['showUserPanel']:
            self.userPanel.personalMessageRefresh()
            self.userPanel.nickRefresh()
        if refresUserList:
            self.refreshUserList()
    def rebuild(self):
        '''repaint the currentinteface'''
        self.buildInterface(self.currentInterface)
    def refreshUserList(self, force=False):
        '''refresh the userlist :D
        (if we are in userlist mode)'''
        if self.currentInterface == 'userlist':
            groups = self.controller.msn.contactManager.groups
            groups[_('no group')] = \
                self.controller.msn.contactManager.noGroup
            self.userList.fill(groups, force)
    def setAvatar(self, pixbuf):
        if self.currentInterface == 'userlist':
            self.userPanel.setAvatar(pixbuf)
    def onItemSelected(self, userlist, objType, obj, path):
        if objType == 'user':
            self.controller.newConversation(self.controller.msn,
                                            obj.email, None, True)
        elif objType == 'group':
            if self.userList.row_expanded(path):
                self.userList.collapse_row(path)
            else:
                self.userList.expand_row(path, False)
class StatusCombo(gtk.ComboBox):
    '''this class represent the combo where you set the status'''
    def __init__(self, controller):
        '''Constructor'''
        self.statusListStore = gtk.ListStore(gtk.gdk.Pixbuf, \
                      gobject.TYPE_STRING, gobject.TYPE_STRING)
        gtk.ComboBox.__init__(self, self.statusListStore)
        self.controller = controller
        self.statusPixbufCell = gtk.CellRendererPixbuf()
        self.statusTextCell = gtk.CellRendererText()
        self.pack_start(self.statusPixbufCell, False)
        self.pack_start(self.statusTextCell, False)
        self.statusPixbufCell.set_property('xalign', 0.0)
        self.statusPixbufCell.set_property('xpad', 5)
        self.statusTextCell.set_property('xalign', 0.0)
        self.statusTextCell.set_property('xpad', 5)
        self.statusTextCell.set_property('width', 195)
        self.add_attribute(self.statusPixbufCell, 'pixbuf', 0)
        self.add_attribute(self.statusTextCell, 'text', 2)
        self.set_resize_mode(0)
        self.set_wrap_width(1)
        counter = 0
        flag = False
        j = 0
        for i in controller.status_ordered[0]:
            if self.controller.contacts.get_status() == i:
                self.set_active(j)
            if i != 'FLN':
                self.statusListStore.append([
                    self.controller.theme.statusToPixbuf(i), i,
                    _(self.controller.status_ordered[2][j])]) # re-gettext-it
            j += 1
        self.changeStatusFlag = True
        self.connect('changed', self.on_status_changed, self.changeStatusFlag)
        self.controller.msn.connect('self-status-changed',
            self.selfStatusChanged)
    def selfStatusChanged(self, msnp, status):
        self.changeStatusFlag = False
        statusOrdered = self.controller.status_ordered[1]
        if emesenelib.common.reverse_status[status] in statusOrdered:
            self.set_active(statusOrdered.index(\
                emesenelib.common.reverse_status[status]))
        self.changeStatusFlag = True
    def on_status_changed(self , *args):
        if self.changeStatusFlag:
            asd = self.statusListStore.get(self.get_active_iter(), 1)
            print "on_status_changed", asd
            self.controller.contacts.set_status(asd[0])
