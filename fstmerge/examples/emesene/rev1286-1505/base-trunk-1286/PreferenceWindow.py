import gtk
import os
import Theme
import htmltextview
import desktop
from Widgets import WidgetToggleBox
from emesenecommon import * # no paths here
class PreferenceWindow(gtk.Window):
    '''This class display a windows where the user
    can modify the behaviour of emesene'''
    def __init__(self, controller, config, parent):
        '''Contructor'''
        gtk.Window.__init__(self)
        self.controller = controller
        self.set_default_size(150, 150)
        self.set_title(_('Preferences'))
        self.set_role(_('Preferences'))
        self.config = config
        self.set_transient_for(parent)
        self.set_type_hint(gtk.gdk.WINDOW_TYPE_HINT_DIALOG)
        self.set_border_width(6)
        vbox = gtk.VBox()
        vbox.set_spacing(4)
        hButBox = gtk.HButtonBox()
        hButBox.set_spacing(4)
        hButBox.set_layout(gtk.BUTTONBOX_END)
        notebook = gtk.Notebook()
        notebook.set_scrollable(True)
        notebook.set_border_width(3)
        bClose = gtk.Button(stock=gtk.STOCK_CLOSE)
        vbox.pack_start(notebook, True, True)
        hButBox.pack_start(bClose)
        vbox.pack_start(hButBox, False, False)
        if self.controller.connected:
            self.interface_tab = InterfaceTab(self.controller)
            self.themesTab = ThemesTab(self.controller)
            self.colorSchemeTab = ColorSchemeTab(self.controller)
            self.ftTab = FTSettings(self.controller)
            notebook.append_page(self.themesTab, gtk.Label(_('Theme')))
            notebook.append_page(self.interface_tab, gtk.Label(_('Interface')))
            notebook.append_page(self.colorSchemeTab,
                gtk.Label(_('Color scheme')))
            notebook.append_page(self.ftTab,
                gtk.Label(_('Filetransfer')))
        self.desktopTab = DesktopTab(self.controller)
        self.connectionTab = ConnectionTab(self.config)
        if self.controller.connected:
            notebook.append_page(self.desktopTab, gtk.Label(_('Desktop')))
            notebook.append_page(self.connectionTab, gtk.Label(_('Connection')))
        else:
            notebook.append_page(self.connectionTab, gtk.Label(_('Connection')))
            notebook.append_page(self.desktopTab, gtk.Label(_('Desktop')))
        vbox.show_all()
        self.add(vbox)
        self.connect('delete-event', self.close)
        bClose.connect('clicked', self.close)
    def close(self, *args):
        '''Close the window'''
        self.hide()
        self.save()
        self.destroy()
        self.controller.preference_open = False
    def save(self):
        '''save the actual setting'''
        self.connectionTab.save()
        if self.controller.connected:
            self.interface_tab.save()
            self.themesTab.save()
            self.colorSchemeTab.save()
            self.desktopTab.save()
            self.ftTab.save()
            self.controller.emit('preferences-changed')
class ColorSchemeItem:
    def __init__(self, tab, configColor,text):
        self.configColor=configColor
        self.button = gtk.ColorButton(gtk.gdk.color_parse(tab.config.user[configColor]))
        self.button.connect("color-set", tab.save)
        alignment = gtk.Alignment(0, 0.5, 0, 0)
        alignment.add(gtk.Label(text))
        self.hbox = gtk.HBox()
        self.hbox.pack_start(alignment, True, True, 3)
        self.hbox.pack_start(self.button, False, False, 3)
class ColorSchemeTab(gtk.VBox):
    '''This class represent the tab that contain all the configuration about the colors'''
    def __init__(self, controller):
        gtk.VBox.__init__(self)
        self.config = controller.config
        self.controller = controller
        self.items={}
        self.items["typingColor"] = ColorSchemeItem(self, 'typingColor', \
            _("Contact typing"))
        self.items["messageWaitingColor"] = ColorSchemeItem(self, \
            'messageWaitingColor', _("Message waiting"))
        self.items["personalMessageColor"] = ColorSchemeItem(self, \
            'personalMessageColor', _("Personal msn message"))
        vbox = gtk.VBox()
        for i in self.items.keys():
            vbox.pack_start(self.items[i].hbox, False, False, 3)
        self.pack_start(vbox, True, True, 0)
        self.pack_start(gtk.HSeparator(), False, True, 2)
        restoreButton = gtk.Button(stock = gtk.STOCK_CLEAR)
        restoreButton.connect("clicked", self.restoreColors)
        alignment = gtk.Alignment(0.5, 0.5, 0.5, 0.5)
        alignment.add(restoreButton)
        self.pack_start(alignment, False, True, 2)
        self.show_all()
    def restoreColors(self, *args):
        for i in self.items.keys():
            color = gtk.gdk.color_parse(self.config.user.getDefault(self.items[i].configColor))
            self.items[i].button.set_color(color) 
    def save(self, *args):
        for i in self.items.keys():
            self.config.user[self.items[i].configColor] = rgbToHexa(self.items[i].button.get_color())
class DesktopTab(gtk.VBox):
    '''A tab that lets the user override the detected desktop settings'''
    def __init__(self, controller):
        gtk.VBox.__init__(self)
        self.config = controller.config
        self.urlSettings = UrlSettings(controller.config)
        frame = gtk.Frame(_('Links and files'))
        frame.set_border_width(4)
        frame.add(self.urlSettings)
        self.set_spacing(8)
        self.set_border_width(8)
        self.rgba = gtk.CheckButton(_('Enable rgba colormap (requires restart)'))
        self.rgba.set_active(self.config.glob['rgbaColormap'])
        self.pack_start(self.rgba, False)
        self.pack_start(frame, False)
    def save(self, widget=None):
        self.config.glob['rgbaColormap'] = self.rgba.get_active()
        self.urlSettings.save()
class UrlSettings(gtk.VBox):
    def __init__(self, config):
        gtk.VBox.__init__(self)
        self.set_spacing(8)
        self.set_border_width(8)
        self.config = config
        detected = desktop.get_desktop(True)
        if detected:
            commandline = ' '.join(desktop.get_command(detected, '')).strip()
            tmp = {
                'detected': detected,
                'commandline': commandline,
            }
            markup = _('The detected desktop environment is '
                '<b>"%(detected)s"</b>.\n'
                '<span face="Monospace">%(commandline)s</span> '
                'will be used to open links and files') % tmp
        else:
            markup = _('<b>No desktop environment detected.</b> '
                'The first browser found will be used to open links')
        self.infolabel = gtk.Label()
        self.infolabel.set_markup(markup)
        self.infolabel.set_line_wrap(True)
        self.infolabel.set_alignment(0.0, 0.0)
        self.hboxentry = gtk.HBox()
        self.entry = gtk.Entry()
        self.entry.connect('activate', self.save)
        self.hboxentry.pack_start(gtk.Label(_("Command line:")), False)
        self.hboxentry.pack_start(self.entry)
        self.override = gtk.CheckButton(_('Override detected settings'))
        self.override.set_active(self.config.glob['overrideDesktop'] != '')
        self.override.connect('toggled', self.toggleOverride)
        self.helplabel = gtk.Label()
        self.helplabel.set_markup(_("<i>Note:</i> %s is replaced by "
            "the actual url to be opened") % "%url%")
        self.helplabel.set_alignment(0.5, 1.0)
        self.hboxtest = gtk.HBox()
        self.testbutton = gtk.Button(_('Click to test'))
        self.testbutton.connect('clicked', self.testDesktop)
        self.hboxtest.pack_start(gtk.Label())
        self.hboxtest.pack_start(self.testbutton, False, True, 6)
        self.pack_start(self.infolabel, False)
        self.pack_start(self.override, False)
        self.pack_start(self.hboxentry, False)
        self.pack_start(self.hboxtest, False)
        self.pack_start(self.helplabel, False)
        self.toggleOverride()
    def toggleOverride(self, override=None):
        active = self.override.get_active()
        self.hboxentry.set_sensitive(active)
        self.hboxtest.set_sensitive(active)
        if active:
            self.entry.set_text(self.config.glob['overrideDesktop'])
        else:
            self.entry.set_text('')
        self.save()
    def save(self, widget=None):
        desktop.override = self.entry.get_text()
        self.config.glob['overrideDesktop'] = self.entry.get_text()        
    def testDesktop(self, button):
        self.save()
        try:
            desktop.open('http://www.emesene.org')
        except OSError:
            pass               
class ConnectionTab(gtk.VBox):
    '''This class represent the tab that contains the connection options'''
    def __init__(self, config):
        '''Constructor'''
        gtk.VBox.__init__(self)
        self.config = config
        self.set_spacing(8)
        self.set_border_width(8) 
        self.debug = gtk.CheckButton(_('_Show debug in console'))
        self.debug.set_active(self.config.glob['debug'])
        self.binary = gtk.CheckButton(_('_Show binary codes in debug'))
        self.binary.set_active(self.config.glob['binary'])
        self.httpMethod =  gtk.CheckButton(_('_Use HTTP method'))
        self.httpMethod.set_active(self.config.glob['httpMethod'])
        self.proxySettings = ProxySettings(self.config)
        frame = gtk.Frame(_('Proxy settings'))
        frame.set_border_width(4)
        frame.add(self.proxySettings)
        self.pack_start(self.debug, False, False)
        self.pack_start(self.binary, False, False)
        self.pack_start(self.httpMethod, False, False)
        proxyBox = gtk.VBox(spacing=2)
        proxyBox.set_border_width(4)
        proxyBox.pack_start(frame, False, False)
        self.pack_start(proxyBox, True, True)
        self.show_all()
    def save(self):
        '''save the actual setting'''
        self.config.glob['debug'] = self.debug.get_active()
        self.config.glob['binary'] = self.binary.get_active()
        self.config.glob['httpMethod'] = self.httpMethod.get_active()
        self.proxySettings.save()
class ThemesTab(gtk.VBox):
    '''This class represent the tab that ...'''
    def __init__(self, controller):
        '''Constructor'''
        gtk.VBox.__init__(self)
        self.controller = controller
        self.config = controller.config
        self.theme = controller.theme
        self.basetheme = Theme.BaseTheme()
        self.basetheme.pixbufs = {}
        self.basetheme.defaults = {}
        self.basetheme.HOME_PATH = self.theme.HOME_PATH
        self.basetheme.SYSTEM_WIDE_PATH = self.theme.SYSTEM_WIDE_PATH
        self.basetheme.defaultPath = self.theme.defaultPath
        self.set_spacing(8)
        self.set_border_width(8)
        self.smallIcons = gtk.CheckButton(_('_Use small icons'))
        self.smallIcons.set_active(self.config.user['smallIcons'])
        self.parseSmilies = gtk.CheckButton(_('Display _smiles'))
        self.parseSmilies.set_active(self.config.user['parseSmilies'])
        self.disableFormat = gtk.CheckButton(_('Disable text formatting'))
        self.disableFormat.set_active(self.config.user['disableFormat'])
        self.pack_start(self.smallIcons, False, False)
        self.pack_start(self.parseSmilies, False, False)
        self.pack_start(self.disableFormat, False, False)
        fields = []
        fields.append({'name': 'theme',
                       'label': _('Theme'),
                       'values': self.config.getThemes(),
                       'config': None,
                       'sample': self.getThemeSample,
                       })
        fields.append({'name': 'smilieTheme',
                       'label': _('Smilie theme'),
                       'values': self.config.getSmilieThemes(),
                       'config': None,
                       'sample': self.getSmilieSample,
                       })
        fields.append({'name': 'conversationLayout',
                       'label': _('Conversation Layout'),
                       'values': self.controller.conversationLayoutManager.listAvailableThemes(),
                       'config': self.configConversationLayout,
                       'sample': None,
                       })
        self.fields = fields
        table = gtk.Table(len(fields), 2, False)
        table.set_row_spacings(2)
        table.set_col_spacings(2)
        row = 0
        for field in fields:
            label = gtk.Label(field['label'])
            label.set_alignment(0.0, 0.5)
            label.set_use_underline(True)
            table.attach(label, 0,1, row,row+1)
            comboModel = gtk.ListStore(str, gtk.gdk.Pixbuf)
            combo = gtk.ComboBox(comboModel)
            cellRenderer = gtk.CellRendererText()
            sampleImage = gtk.CellRendererPixbuf()
            sampleImage.set_property('xalign', 1.0)
            combo.pack_start(cellRenderer, False)
            combo.add_attribute(cellRenderer, 'markup', 0)
            combo.pack_start(sampleImage, True)
            combo.add_attribute(sampleImage, 'pixbuf', 1)
            self.fields[row]['combo'] = combo
            label.set_mnemonic_widget(combo)
            combo.connect('changed', self.save, field['name'])
            default = self.config.user[field['name']]
            done = False
            count = 0
            for current in field['values']:
                sample = None
                if field['sample']:
                    sample = field['sample'](field['name'], current)
                if not done:
                    if current == default: done = True
                    else: count += 1
                comboModel.append([ current, sample ])
            combo.set_active(count)
            table.attach(combo, 1,2, row,row+1)
            row += 1
        self.pack_start(table, False, False, 3)
        self.show_all()
    def configConversationLayout(self, *args):
        dialog = gtk.Dialog(_("Conversation Layout"),
                             None,
                             gtk.DIALOG_MODAL,
                             (gtk.STOCK_OK, gtk.RESPONSE_ACCEPT))
        conversationLayoutTab = ConversationLayoutTab(self.controller)
        dialog.vbox.add(conversationLayoutTab)
        dialog.show_all()
        response = dialog.run()
        dialog.destroy()
    def getThemeSample(self, label, item):
        self.basetheme.setTheme(item)
        return self.basetheme.getImage("icon")
    def getSmilieSample(self, label, item):
        tempTheme = Theme.SmilieTheme(None, item)
        try:
            icon = tempTheme.getSmiley(":D").get_static_image()
            del tempTheme
            return icon
        except:
            pass
    def save(self, widget = None, name = 'all'):
        self.config.user['parseSmilies'] = self.parseSmilies.get_active()
        self.config.user['smallIcons'] = self.smallIcons.get_active()
        self.config.user['disableFormat'] = self.disableFormat.get_active()
        try:
            for field in self.fields:
                self.config.user[field['name']] = field['combo'].get_active_text()
        except:
            pass
        if name in ('all','conversationLayout'): 
            layout = self.config.user['conversationLayout']
            self.controller.conversationLayoutManager.load(layout)
class ConversationLayoutTab(gtk.VBox):
    '''This class represents the conversation layout config'''
    def __init__(self, controller):
        '''Constructor'''
        gtk.VBox.__init__(self)
        self.controller = controller
        self.config = controller.config
        self.set_spacing(8)
        self.set_border_width(8) 
        self.labelName = gtk.Label()
        self.labelName.set_alignment(0.0, 0.5)
        self.labelDescription = gtk.Label()
        self.labelDescription.set_alignment(0.0, 0.5)
        self.labelAuthor = gtk.Label()
        self.labelAuthor.set_alignment(0.0, 0.5)
        self.labelWebsite = gtk.Label()
        self.labelWebsite.set_alignment(0.0, 0.5)
        self.previewScroll = gtk.ScrolledWindow()
        self.previewScroll.set_policy(gtk.POLICY_NEVER, gtk.POLICY_AUTOMATIC)
        self.preview = htmltextview.HtmlTextView(self.controller)
        self.previewScroll.add(self.preview)
        self.pack_start(self.labelName, False, False, 3)
        self.pack_start(self.labelDescription, False, False, 3)
        self.pack_start(self.labelAuthor, False, False, 3)
        self.pack_start(self.labelWebsite, False, False, 3)
        self.pack_start(self.previewScroll, True, True, 3)
        self.updateInfosFrame()
        self.show_all()
    def updateInfosFrame(self):
        '''Updates the htmltextview preview.'''
        mgr = self.controller.conversationLayoutManager
        self.labelName.set_markup('<b>' + _('Name:') + '</b> ' + \
            mgr.getName())
        self.labelDescription.set_markup('<b>' + _('Description:') + '</b> ' +\
            mgr.getDescription())
        self.labelAuthor.set_markup('<b>' + _('Author:') + '</b> ' + \
            mgr.getAuthor())
        self.labelWebsite.set_markup('<b>' + _('Website:') + '</b> ' + \
            mgr.getWebsite())
        textbuff = self.preview.get_buffer()
        start, end = textbuff.get_bounds()
        textbuff.delete(start,end)
        html = mgr.getPreview()  
        BODY = "<body>%s</body>"
        self.preview.display_html(BODY % html)
class InterfaceTab(gtk.VBox):
    '''This class represents the [...]'''
    def __init__(self, controller):
        '''Constructor'''
        gtk.VBox.__init__(self)
        self.controller = controller
        self.config = controller.config
        self.set_spacing(2)
        self.set_border_width(8)
        self.label = gtk.Label()
        def add(box, key, desc, size, packing=False, align=None, defsize=-1):
            widget = WidgetToggleBox(self.config, key, desc, self.label)
            widget.show()
            if type(box) == gtk.HBox:
                widget.set_size_request(size, defsize)
            else:
                widget.set_size_request(defsize, size)
            if align:
                align.add(widget)
                widget = align
            box.pack_start(widget, packing)
        self.windowshbox = gtk.HBox()
        self.maindescvbox = gtk.VBox()
        self.mainalign = gtk.Alignment(0.5, 0.5, 0.0, 0.0)
        self.mainvbox = gtk.VBox()
        self.mainvbox.set_size_request(60, -1)
        add(self.mainvbox, 'showMenu', _('Show _menu bar'), 10)
        add(self.mainvbox, 'showUserPanel', _('Show _user panel'), 20)
        add(self.mainvbox, 'showSearchEntry', _('Show _search entry'), 10)
        add(self.mainvbox, None, None, 80)
        add(self.mainvbox, 'showStatusCombo', _('Show status _combo'), 10)
        mainwindowlabel = gtk.Label()
        mainwindowlabel.set_markup('<b>' + _('Main window') + '</b>')
        mainwindowlabel.set_alignment(0.5, 0.0)
        self.mainalign.add(self.mainvbox)
        self.maindescvbox.pack_start(mainwindowlabel)
        self.maindescvbox.pack_start(self.mainalign, False)
        self.windowshbox.pack_start(self.maindescvbox)
        self.convdescvbox = gtk.VBox()
        self.convalign = gtk.Alignment(0.5, 0.5, 0.0, 0.0)
        self.convbox1 = gtk.VBox()
        self.convbox1.set_size_request(100, -1)
        add(self.convbox1, 'showMenubar', _('Show _menu bar'), 10)
        add(self.convbox1, 'showHeader', _('Show conversation _header'), 20)
        self.convbox2 = gtk.HBox()
        self.convbox3 = gtk.VBox()
        self.convbox4 = gtk.HBox()
        self.convbox5 = gtk.VBox()
        self.convbox1.pack_start(self.convbox2)
        self.convbox2.pack_start(self.convbox3)
        add(self.convbox3, None, None, 50)
        self.convbox3.pack_start(self.convbox4)
        self.convbox4.pack_start(self.convbox5)
        self.convbox4.set_size_request(-1, 30)
        add(self.convbox5, 'showToolbar', _('Show conversation _toolbar'), 10)
        add(self.convbox5, None, None, -1, packing=True)
        add(self.convbox4, 'showSendButton', _('S_how a Send button'), 15,
            align=gtk.Alignment(0.0, 0.5, 0.0, 0.0), defsize=10)
        add(self.convbox3, 'showStatusBar', _('Show st_atusbar'), 10)
        add(self.convbox2, 'showAvatars', _('Show a_vatars'), 30)
        convwindowlabel = gtk.Label()
        convwindowlabel.set_markup('<b>' + _('Conversation Window') + '</b>')
        convwindowlabel.set_alignment(0.5, 0.0)
        self.convalign.add(self.convbox1)
        self.convdescvbox.pack_start(convwindowlabel)
        self.convdescvbox.pack_start(self.convalign, True)
        self.windowshbox.pack_start(self.convdescvbox)
        self.pack_start(self.windowshbox)
        self.pack_start(self.label, True)
        self.checks = []
        mkcheck(self, 'windows', _('Use multiple _windows'))
        mkcheck(self, 'showTabCloseButton', _('Show close button on _each tab'))
        mkcheck(self, 'avatarsInUserList', _('Show _avatars'))
        mkcheck(self, 'avatarsInTaskbar', _('Show avatars in task_bar'))
        self.show_all()
    def onToggled(self, radio, option):
        self.config.user[option] = radio.get_active()
    def save(self):
        pass
class ProxySettings(gtk.VBox):
    '''This class represents the panel with the proxy variables
    in the config file'''
    def __init__(self, config):
        '''Constructor'''
        gtk.VBox.__init__(self)   
        self.config = config
        self.set_spacing(8)
        self.set_border_width(8)
        self.useProxy = gtk.CheckButton(_('_Use proxy'))
        self.useProxy.set_active(self.config.glob['useProxy'])
        self.host = gtk.Entry()
        self.host.set_text(self.config.glob['proxyHost'])
        self.port = gtk.Entry()
        self.port.set_text(str(self.config.glob['proxyPort']))
        table = gtk.Table(2, 2)
        table.set_row_spacings(2)
        table.set_col_spacings(2)
        host = gtk.Label(_('_Host:'))
        host.set_alignment(0.0, 0.5)
        host.set_use_underline(True)
        host.set_mnemonic_widget(self.host)
        port = gtk.Label(_('_Port:'))
        port.set_alignment(0.0, 0.5)
        port.set_use_underline(True)
        port.set_mnemonic_widget(self.port)
        table.attach(host, 0, 1, 0, 1)
        table.attach(port , 0, 1, 1, 2)
        table.attach(self.host, 1, 2, 0, 1)
        table.attach(self.port, 1, 2, 1, 2)
        self.useProxyToggled(self.useProxy)
        self.useProxy.connect('toggled', self.useProxyToggled)
        self.pack_start(self.useProxy)
        self.pack_start(table)
        self.show_all()
    def save(self):
        '''save the actual setting'''
        host = self.host.get_text()
        if host.startswith("http://"):
            host = host.split("http://")[1]
        if host.find("/") != -1:
            host = host.split("/")[0]
        self.config.glob['useProxy'] = self.useProxy.get_active()
        self.config.glob['proxyHost'] = host
        self.config.glob['proxyPort'] = self.port.get_text()
    def useProxyToggled(self, check):
        '''callback for the toggled signal'''
        if self.useProxy.get_active():
            self.host.set_sensitive(True)
            self.port.set_sensitive(True)
        else:
            self.host.set_sensitive(False)
            self.port.set_sensitive(False)
class FTSettings(gtk.VBox):
    '''This class represents the panel with the filetransfer settings'''
    def __init__(self, controller):
        '''Constructor'''
        gtk.VBox.__init__(self)
        self.config = controller.config
        self.pathChooser = gtk.FileChooserDialog(
            title=_('Choose a Directory'),
            action=gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER,
            buttons=(
                gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, 
                gtk.STOCK_OPEN, gtk.RESPONSE_OK)
            )
        self.set_spacing(8)
        self.set_border_width(8)
        self.checks = []
        mkcheck(self, 'receivedFilesSortedByUser',
            _('Sort received _files by sender'))
        targetbutton = gtk.FileChooserButton(self.pathChooser)
        targetbutton.set_current_folder(os.path.expanduser(
            self.config.user['receivedFilesDir']))
        target = gtk.Label(_('_Save files to:'))
        target.set_alignment(0.0, 0.5)
        target.set_use_underline(True)
        self.pack_start(target, False)
        self.pack_start(targetbutton, False)
    def onToggled(self, radio, option):
        self.config.user[option] = radio.get_active()
    def save(self):
        self.config.user['receivedFilesDir'] = self.pathChooser.get_filename()
def mkcheck(self, id, label):
    '''little helper function to add checkbuttons'''
    check = gtk.CheckButton(label)
    check.set_active(self.config.user[id])
    check.connect('toggled', self.onToggled, id)
    self.pack_start(check, False, False, padding=0)
    setattr(self, id, check)
    self.checks.append(id)
