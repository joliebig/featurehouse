import gtk
import desktop
import StatusMenu
import PreferenceWindow
import stock
import dialog
class MainMenu(gtk.MenuBar):
    '''this class represent the main menu in the main window'''
    def __init__(self, controller, window_type, accelGroup):
        '''constructor'''
        gtk.MenuBar.__init__(self)
        self.controller = controller
        self.config = self.controller.config
        self.theme = self.controller.theme
        fileMenu = gtk.Menu()
        fileMenuItem = self.newImageMenuItem(_('_File'))
        fileMenuItem.set_submenu(fileMenu)
        if window_type == 'userlist':
            viewMenu = gtk.Menu()
            viewMenuItem = self.newImageMenuItem(_('_View'))
            viewMenuItem.set_submenu(viewMenu)
            actionsMenu = gtk.Menu()
            actionsMenuItem = self.newImageMenuItem(_('_Actions'))
            actionsMenuItem.connect('activate', self.on_actions_activate)
            actionsMenuItem.set_submenu(actionsMenu)
        optionsMenu = gtk.Menu()
        optionsMenuItem = self.newImageMenuItem(_('_Options'))
        optionsMenuItem.set_submenu(optionsMenu)
        helpMenu = gtk.Menu()
        helpMenuItem = self.newImageMenuItem(_('_Help'))
        helpMenuItem.set_submenu(helpMenu)
        if window_type == 'userlist':
            statusMenuItem = self.newImageMenuItem(_('_Status'))
            statusMenu = StatusMenu.StatusMenu(self.controller)
            statusMenuItem.set_submenu(statusMenu)
            fileMenu.add(statusMenuItem)
            fileMenu.add(gtk.SeparatorMenuItem())
            disconnect = self.newStockImageMenuItem(gtk.STOCK_DISCONNECT)
            fileMenu.add(disconnect)
        else:
            getliveMenuItem = self.newImageMenuItem(_('_New account'), \
                                                    gtk.STOCK_NEW)
            fileMenu.add(getliveMenuItem)
            fileMenu.add(gtk.SeparatorMenuItem())
        quitMenuItem = self.newStockImageMenuItem(gtk.STOCK_QUIT)
        quitMenuItem.add_accelerator('activate', accelGroup, ord('Q'), \
                                 gtk.gdk.CONTROL_MASK, gtk.ACCEL_VISIBLE)
        fileMenu.add(quitMenuItem)
        accelGroup.connect_group(ord('Q'), gtk.gdk.CONTROL_MASK, \
            gtk.ACCEL_LOCKED, self.on_quit_activate)
        if window_type == 'userlist':
            self.orderByGroup = gtk.RadioMenuItem(None, \
                                                      _('Order by _group'))
            self.orderByStatus = gtk.RadioMenuItem(self.orderByGroup, \
                                                      _('Order by _status'))
            if not self.config.user['orderByStatus']:
                self.orderByGroup.set_active(True)
            else:
                self.orderByStatus.set_active(True)
            viewMenu.add(self.orderByGroup)
            viewMenu.add(self.orderByStatus)
            viewMenu.add(gtk.SeparatorMenuItem())
            self.showByNick = self.newCheckMenuItem(_('Show by _nick'), \
                    self.config.user['showByNick'])
            self.showOffline = self.newCheckMenuItem(_('Show _offline'), \
                    self.config.user['showOffline'])
            self.showEmptyGroups = self.newCheckMenuItem(
             _('Show _empty groups'), self.config.user['showEmptyGroups'])
            self.showCountContact = self.newCheckMenuItem(
                _('Show _contact count'), 
                self.config.user['showCountContact'])
            viewMenu.add(self.showByNick)
            viewMenu.add(self.showOffline)
            viewMenu.add(self.showEmptyGroups)
            viewMenu.add(self.showCountContact)
        if window_type == 'userlist':
            addUserMenuItem = self.newImageMenuItem(_('_Add contact...'), \
                                                     gtk.STOCK_ADD)
            actionsMenu.add(addUserMenuItem)
            addGroupMenuItem = self.newImageMenuItem(_('Add _group...'),
                gtk.STOCK_ADD)
            actionsMenu.add(addGroupMenuItem)
            actionsMenu.add(gtk.SeparatorMenuItem())
            contactsActionGroup = gtk.ActionGroup('contacts')
            self.contactsActionGroup = contactsActionGroup
            setAliasAction = gtk.Action('setAlias', 
                _('_Set contact alias...'), None, gtk.STOCK_EDIT)
            blockAction = gtk.Action('block', _('_Block'), \
                                        None, gtk.STOCK_STOP)
            unblockAction = gtk.Action('unblock', _('_Unblock'), \
                                        None, gtk.STOCK_APPLY)
            moveAction = gtk.Action('move', _('M_ove to group'), \
                                     None, gtk.STOCK_REDO)
            deleteAction = gtk.Action('delete', _('_Remove contact'), \
                                        None, gtk.STOCK_DELETE)
            self.blockAction = blockAction
            self.unblockAction = unblockAction
            contactsActionGroup.add_action(setAliasAction)
            actionsMenu.add(setAliasAction.create_menu_item())
            contactsActionGroup.add_action(blockAction)
            actionsMenu.add(blockAction.create_menu_item())
            contactsActionGroup.add_action(unblockAction)
            actionsMenu.add(unblockAction.create_menu_item())
            contactsActionGroup.add_action(moveAction)
            moveMenuItem = moveAction.create_menu_item()
            actionsMenu.add(moveMenuItem)
            contactsActionGroup.add_action(deleteAction)
            actionsMenu.add(deleteAction.create_menu_item())
            moveMenu = gtk.Menu()
            for i in self.controller.msn.getGroupNames():
                menuItem = gtk.MenuItem (i)
                moveMenu.add(menuItem)
                menuItem.connect('activate', self.on_move_to_activate, i)
            moveMenuItem.set_submenu(moveMenu)
            moveMenuItem.show_all()
            moveMenu.show_all()
            actionsMenu.add(gtk.SeparatorMenuItem())
            groupsActionGroup = gtk.ActionGroup('groups')
            self.groupsActionGroup = groupsActionGroup
            deleteGAction = gtk.Action('remove', _('Re_move group'), \
                                        None, gtk.STOCK_DELETE)
            renameAction = gtk.Action('rename', _('Re_name group...'), \
                                        None, gtk.STOCK_EDIT)
            groupsActionGroup.add_action(renameAction)
            actionsMenu.add(renameAction.create_menu_item())
            groupsActionGroup.add_action(deleteGAction)
            actionsMenu.add(deleteGAction.create_menu_item())
        if window_type == 'userlist':            
            changeNickMenuItem = self.newImageMenuItem(\
                _('_Change nick...'), gtk.STOCK_EDIT)
            changeAvatarMenuItem = self.newImageMenuItem(\
                _('Change _display picture...'), gtk.STOCK_EDIT)
            optionsMenu.add(changeNickMenuItem)
            optionsMenu.add(changeAvatarMenuItem)
            optionsMenu.add(gtk.SeparatorMenuItem())
            setAutoReplyMenuItem = self.newImageMenuItem(\
                _('Edit a_utoreply...'), gtk.STOCK_EDIT)
            self.activateAutoReply = self.newCheckMenuItem(\
                _('Activate au_toreply'), self.config.user['autoReply'])
            optionsMenu.add(self.activateAutoReply)
            optionsMenu.add(setAutoReplyMenuItem)
            optionsMenu.add(gtk.SeparatorMenuItem())
            pluginMenuItem = self.newImageMenuItem(_('P_lugins'), \
                gtk.STOCK_DISCONNECT)
            optionsMenu.add(pluginMenuItem)
        preferencesMenuItem = self.newStockImageMenuItem(
            gtk.STOCK_PREFERENCES)
        preferencesMenuItem.add_accelerator('activate', accelGroup, \
            ord('P'), gtk.gdk.CONTROL_MASK, gtk.ACCEL_VISIBLE)
        optionsMenu.add(preferencesMenuItem)
        accelGroup.connect_group(ord('P'), gtk.gdk.CONTROL_MASK, \
            gtk.ACCEL_LOCKED, self.on_preferences_activate)
        aboutMenuItem = self.newStockImageMenuItem(gtk.STOCK_ABOUT)
        helpMenu.add(aboutMenuItem)
        self.add(fileMenuItem)
        if window_type == 'userlist':
            self.add(viewMenuItem)
            self.add(actionsMenuItem)
        self.add(optionsMenuItem)
        self.add(helpMenuItem)
        quitMenuItem.connect('activate', self.on_quit_activate)
        aboutMenuItem.connect('activate', self.on_about_activate)
        if window_type == 'userlist':
            disconnect.connect('activate', self.on_logout_activate)
            self.orderByGroup.connect('activate', self.on_order_changed)
            self.orderByStatus.connect('activate', self.on_order_changed)
            self.showByNick.connect('activate', 
                self.on_show_by_nick_activate)
            self.showOffline.connect('activate', 
                self.on_show_offline_activate)
            self.showEmptyGroups.connect('activate', 
                self.on_show_empty_groups_activate)
            self.showCountContact.connect('activate', 
                self.on_show_count_contact_activate)
            addUserMenuItem.connect('activate', self.on_add_user_activate)
            setAliasAction.connect('activate', self.on_rename_user_activate)
            deleteAction.connect('activate', self.on_delete_user_activate)
            blockAction.connect('activate', self.on_block_user_activate)
            unblockAction.connect('activate', self.on_unblock_user_activate)
            addGroupMenuItem.connect('activate', self.on_add_group_activate)
            deleteGAction.connect('activate', self.on_delete_group_activate)
            renameAction.connect('activate', self.on_rename_group_activate)
            setAutoReplyMenuItem.connect('activate', 
                self.on_set_auto_reply_activate)
            self.activateAutoReply.connect('activate', 
                self.on_auto_reply_activate)
            changeNickMenuItem.connect('activate', 
                self.on_change_nick_activate)
            changeAvatarMenuItem.connect('activate', 
                self.on_change_avatar_activate)
            pluginMenuItem.connect('activate', self.on_plugin_activate)
        else:
            getliveMenuItem.connect('activate', self.on_get_live_activate)
        preferencesMenuItem.connect('activate', 
            self.on_preferences_activate)
    def newStockImageMenuItem (self, stock):
        '''create a new image menu item from gtk's stock and retrun it'''
        mi = gtk.ImageMenuItem(stock)
        return mi
    def newImageMenuItem(self, label, stock = None, img = None):
        '''create a new Imege menu item and return it, it could have a 
            stock image or a custom image'''
        mi = gtk.ImageMenuItem(_(label))
        if stock != None:
            mi.set_image(gtk.image_new_from_stock(stock, 
                gtk.ICON_SIZE_MENU))
        elif img != None:
            image = gtk.Image()
            image.set_from_pixbuf(img)
            mi.set_image(image)
        return mi
    def newCheckMenuItem(self, label, checked):
        '''create a new checkbox and return it, if checked is true, 
            the check box will be checked (d'uh!)'''
        mi = gtk.CheckMenuItem(_(label))
        mi.set_active(checked)
        return mi
    def on_move_to_activate(self, menuItem, group):
        self.controller.contacts.move_to_group(self.userName, 
            self.userGroup, group)
    def on_rename_user_activate(self, *args):
        self.controller.contacts.set_alias_dialog(self.userName)
    def on_actions_activate(self, *args):
        data = self.controller.getMenuData()
        typeSelected = data[0]
        self.contactsActionGroup.set_sensitive(typeSelected == 'user')
        self.groupsActionGroup.set_sensitive\
                    (typeSelected == 'group' and data[2] != 'nogroup')
        if typeSelected == 'user':
            self.userName = data[1]
            self.userGroup = data[2]
            blocked = self.controller.contacts.get_blocked(data[1])
            self.blockAction.set_sensitive(not blocked)
            self.unblockAction.set_sensitive(blocked)
        if typeSelected == 'group':
            self.groupName = data[1]
    def on_quit_activate(self, *args):
        self.controller.quit(0)
    def on_preferences_activate(self, *args):
        if not self.controller.preference_open:
            PreferenceWindow.PreferenceWindow(self.controller, 
                self.config, self.controller.mainWindow).show()
            self.controller.preference_open = True
    def on_about_activate(self, *args):
        try:
            f = file('COPYING', 'r')
        except:
            f = None
        def closeAbout(widget, response_id):
            if response_id == gtk.RESPONSE_CANCEL:
                widget.destroy()
        gtk.about_dialog_set_url_hook(self.on_click_url, None)
        about = gtk.AboutDialog()
        about.set_name('emesene')
        about.set_version('1.0.1')
        about.set_copyright('Luis Mariano Guerra')
        about.set_comments(_('A client for the WLM%s network') %'\xe2\x84\xa2')
        about.connect('response', closeAbout)
        if f == None:
            about.set_license('GNU General Public License')
        else:
            about.set_license(f.read())
        about.set_website('http://www.emesene.org')
        about.set_authors([
            'Luis Mariano Guerra (emesene and emesenelib)', \
            'Horacio Duran (emesene and emesenelib)', \
            'Alberto Talavera (emesene)', \
            'Linan Wang (MsnOIM)', \
            'Roberto Salas & Jakub Steiner (tango theme)', \
            'Vinicius Depizzol (default theme)', \
            'Yguaratã C. Cavalcanti (emesene)', \
            'Roger Duran (emesene)', \
            'dx (emesene & emesenelib)', \
            'alencool (emesene & cairo wizzard :P)', \
            'Mattia \'MaTz\' Pini (emesene)', \
            'mg (emesene)', \
            'Jan de Mooij (emesene)', \
            'j0hn (emesene)', \
            'Luis \'JoinTheHell\' Nell (emesene)', \
            'nopersona (in the margins theme)'
            ])
        about.set_translator_credits(_('translator-credits'))
        icon = self.controller.theme.getImage('login')
        about.set_icon(icon)
        about.set_logo(icon)
        about.run()
    def on_click_url(self, dialog, link, user_data):
        desktop.open(link)
    def on_logout_activate(self, *args):
        self.controller.logout()
    def on_order_changed(self, menuitem):
        if menuitem == self.orderByGroup:
            self.controller.config.user['orderByStatus'] = False
        elif menuitem == self.orderByStatus:
            self.controller.config.user['orderByStatus'] = True
        self.controller.refreshUserList(force=True)
    def on_show_by_nick_activate(self, *args):
        self.controller.config.user['showByNick'] = \
            self.showByNick.get_active()
        self.controller.refreshUserList()
    def on_show_offline_activate(self, *args):
        self.controller.config.user['showOffline'] = \
            self.showOffline.get_active()
        self.controller.mainWindow.userList.refilter()
    def on_show_empty_groups_activate(self, *args):
        self.controller.config.user['showEmptyGroups'] = \
            self.showEmptyGroups.get_active()
        self.controller.mainWindow.userList.refilter()
    def on_show_count_contact_activate(self, *args):
        self.controller.config.user['showCountContact'] = \
            self.showCountContact.get_active()
        self.controller.refreshUserList(force=True)
    def on_add_user_activate(self, *args):
        self.controller.addUserDialog()
    def on_delete_user_activate(self, *args):
        self.controller.contacts.remove(self.userName)
    def on_block_user_activate(self, *args):
        self.controller.contacts.block(self.userName)
    def on_unblock_user_activate(self, *args):
        self.controller.contacts.unblock(self.userName)
    def on_add_group_activate(self, *args):
        self.controller.groups.add_dialog()
    def on_delete_group_activate(self, *args):
        self.controller.groups.remove(self.groupName)
    def on_rename_group_activate(self, *args):
        self.controller.groups.rename_dialog(self.groupName)
    def on_change_avatar_activate(self, *args):
        self.controller.set_picture_dialog()
    def on_change_nick_activate(self, *args):
        self.controller.contacts.set_nick_dialog()
    def on_set_auto_reply_activate(self, *args):
        def response_cb(response, message=''):
            '''callback for the set autoreply dialog'''
            if response == stock.ACCEPT:
                if message == '':
                    dialog.error(_("Empty autoreply"))
                else:
                    self.controller.autoReplyMessage = message 
                    self.config.user['autoReplyMessage'] = message
        window = dialog.entry_window(_("Autoreply message:"), 
            self.config.user['autoReplyMessage'], response_cb, 
            _("Change autoreply"))
        window.show()
    def on_auto_reply_activate(self, *args):
        self.config.user['autoReply'] = self.activateAutoReply.get_active()
    def on_plugin_activate(self, *args):
        self.controller.pluginDialog()
    def on_get_live_activate(self, *args):
        link = 'https://accountservices.passport.net/reg.srf?sl=1'
        desktop.open(link)
