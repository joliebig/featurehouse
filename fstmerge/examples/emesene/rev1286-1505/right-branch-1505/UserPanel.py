import gc
import re
import gtk
import pango
import desktop
import Widgets
import FancyLabel
import StatusMenu
from SmileyRenderer import SmileyLabel
from Parser import PangoDataType
import dialog
DOMAIN_REGEXP = re.compile('^live(\.[a-zA-Z]{2,3})?\.[a-zA-Z]{2,3}$|'
                           '^msn(\.[a-zA-Z]{2,3})?\.[a-zA-Z]{2,3}|'
                           '^hotmail(\.[a-zA-Z]{2,3})?\.[a-zA-Z]{2,3}')
class UserPanel(gtk.HBox):
    '''this class is the panel that will contain the user photo, his nick and his status'''
    def __init__(self, controller):
        '''Constructor'''        
        self.tooltip = gtk.Tooltips()
        self.controller = controller
        self.theme = controller.theme
        self.config = controller.config
        gtk.HBox.__init__(self)
        self.set_border_width(4)
        self.set_spacing(2)
        self.smileysCache = {}
        self.hasSmilies = True
        self.tNick = gtk.Entry(max=129)
        self.tNick.set_text(self.controller.msn.nick)
        self.tNick.set_no_show_all(True)
        if self.config.user['useFancyLabel']:
            self.lNick = FancyLabel.FancyLabel(self.controller)
            self.lNick.set_wrap(False) 
        else:
            self.lNick = SmileyLabel('', self.controller.widget_style)
            self.lNick.set_ellipsize(pango.ELLIPSIZE_END)
        self.bNick = gtk.Button()
        self.bNick.set_relief(gtk.RELIEF_NONE)
        self.bNick.set_alignment(0, 0)
        self.bNick.add(self.lNick)
        nick = self.controller.msn.nick
        if not self.config.user['useFancyLabel']:
            parser = self.controller.unifiedParser.getParser(nick, \
                PangoDataType)
            nick = parser.get(self.hasSmilies, self.smileysCache)
        self.lNick.set_markup(nick)
        iconMail = self.theme.getSmiley("(e)") #email icon
        self.mailButton = ImageButton(iconMail, '(0)')
        self.mailButton.set_relief(gtk.RELIEF_NONE)
        self.tPersonalMessage = gtk.Entry()
        self.tPersonalMessage.set_text(self.controller.contacts.get_message())
        self.tPersonalMessage.set_no_show_all(True)
        self.lPersonalMessage = SmileyLabel('', 
            self.controller.widget_style)
        self.lPersonalMessage.set_ellipsize(pango.ELLIPSIZE_END)
        self.bPersonalMessage = gtk.Button()
        self.bPersonalMessage.set_relief(gtk.RELIEF_NONE)
        self.bPersonalMessage.set_alignment(0, 0)
        self.bPersonalMessage.add(self.lPersonalMessage)
        pm = self.controller.contacts.get_message()
        if pm != '':
            parser = self.controller.unifiedParser.getParser(pm, PangoDataType)
            pm = parser.get(self.hasSmilies, self.smileysCache)
            self.lPersonalMessage.set_markup(pm)
        else:
            self.lPersonalMessage.set_text('<i>&lt;' + _('Click here to set your personal message') + '&gt;</i>')
        mediaIcon = self.theme.getSmiley("(8)") #media icon
        self.mediaButton = ImageToggleButton(mediaIcon)
        self.mediaButton.set_relief(gtk.RELIEF_NONE)
        self.lMedia = gtk.Label(_("No media playing"))
        self.lMedia.set_ellipsize(pango.ELLIPSIZE_END)
        self.bMedia = gtk.Button()
        self.bMedia.set_relief(gtk.RELIEF_NONE)
        self.bMedia.set_alignment(0, 0)
        self.bMedia.add(self.lMedia)
        self.bMedia.connect('clicked', self.onMediaClicked)
        self.lMedia.set_no_show_all(True)
        self.bMedia.set_no_show_all(True)
        self.tooltip.set_tip(self.bNick, _('Click here to set your nick name'))
        self.tooltip.set_tip(self.bPersonalMessage, _('Click here to set your personal message'))
        self.tooltip.set_tip(self.bMedia, _('Your current media'))
        self.image = Widgets.avatarHolder(cellDimention = 48)
        self.imageEventBox = gtk.EventBox()
        self.imageEventBox.set_events(gtk.gdk.BUTTON_PRESS_MASK)
        self.imageEventBox.connect('button-press-event', self.avatarClicked)
        self.imageEventBox.add(self.image)
        if self.controller.avatar != None:
            self.image.set_from_pixbuf(self.controller.avatar.getThumb())
        else:
            self.pixbuf = self.controller.theme.getImage('userPanel')
            self.image.set_from_pixbuf(self.pixbuf)
        self.tNick.connect('activate', self.on_nick_changed)
        self.tNick.connect('focus-out-event', self.on_nick_changed)
        self.bNick.connect('clicked', self.on_nick_clicked)
        self.tPersonalMessage.connect('activate', self.on_pm_changed)
        self.tPersonalMessage.connect('focus-out-event', self.on_pm_changed)
        self.bPersonalMessage.connect('clicked', self.on_pm_clicked)
        self.mediaButton.connect("toggled", self.onToggleMedia)
        self.mailButton.connect('clicked', self.onMaiButtonClicked)
        self.controller.msn.connect('self-personal-message-changed', self.personalMessageChanged)
        self.controller.msn.connect('self-nick-changed', self.selfNickChanged)
        self.controller.msn.connect('self-current-media-changed', self.currentMediaChanged)
        self.mailButton.setText('('+str(self.controller.getUnreadMails()) +')')
        self.controller.msn.connect('initial-mail-notification', self.updateMailCount)
        self.controller.msn.connect('new-mail-notification', self.updateMailCount)
        self.controller.msn.connect('mail-movement-notification', self.updateMailCount)
        self.hbox = gtk.HBox()
        self.pack_start(self.imageEventBox, False, False)
        self.vbox1 = gtk.VBox()
        self.vbox2 = gtk.VBox(True)
        self.hbox1 = gtk.HBox()
        self.hbox2 = gtk.HBox()
        self.hbox1.pack_start(self.tNick, True, True)
        self.hbox1.pack_start(self.bNick, True, True)
        self.hbox2.pack_start(self.tPersonalMessage, True, True)
        self.hbox2.pack_start(self.bPersonalMessage, True, True)
        self.hbox2.pack_start(self.bMedia, True, True)
        self.vbox2h1 = gtk.HBox()
        self.vbox2h1.pack_start(self.mailButton, True, True)
        self.vbox2h2 = gtk.HBox()
        self.vbox2h2.pack_start(self.mediaButton, True, True)
        self.vbox2.pack_start(self.vbox2h1, True, False)
        self.vbox2.pack_start(self.vbox2h2, True, False)
        self.vbox1.pack_start(self.hbox1, True, False, 1)
        self.vbox1.pack_start(self.hbox2, True, False, 1)
        self.hbox.pack_start(self.vbox1, True, True)
        self.hbox.pack_start(self.vbox2, False, False)
        self.pack_start(self.hbox)
        self.show_all()
        if controller.config.user['mediaEnabled']:
            self.mediaButton.set_active(True)
        self.mediaButton.hide()
        try:
            if not DOMAIN_REGEXP.match(self.controller.userEmail.split("@")[1]):
                self.mailButton.hide()
        except Exception, e:
            print "error! " + str(e)
    def onMediaClicked(self, *args):
        self.controller.pluginManager.getPlugin('CurrentSong').configure()
    def avatarClicked(self, widget, event):
        if event.button == 1:
            self.controller.set_picture_dialog()
        elif event.button == 3:
            menu = AvatarMenu(self.controller)
            menu.popup(None, None, None, event.button, event.time)
    def updateMailCount(self, *args):
        self.mailButton.setText('('+str(self.controller.getUnreadMails()) +')')
    def selfNickChanged(self, msnp, oldNick, nick):
        '''method called when the user change his nick in other part'''
        self.tNick.set_text(nick)
        if not self.config.user['useFancyLabel']:
            parser = self.controller.unifiedParser.getParser(nick, \
                PangoDataType)
            nick = parser.get(self.hasSmilies, self.smileysCache)
        self.lNick.set_markup(nick)
        self.nickRefresh()
    def on_nick_activate(self, *args):
        self.controller.contacts.set_nick(self.tNick.get_text())
    def on_nick_changed(self, *args):
        self.controller.contacts.set_nick(self.tNick.get_text())
        self.tNick.hide()
        self.bNick.show()
        self.lNick.show()
    def on_nick_clicked(self, *args):
        self.tNick.show()
        self.bNick.hide()
        self.lNick.hide()
        self.tNick.grab_focus()
    def nickRefresh(self):
        self.on_nick_changed()
        self.bNick.grab_focus()
    def on_pm_changed(self, *args) : 
        self.controller.contacts.set_message(\
            self.tPersonalMessage.get_text())
        self.bMedia.hide()
        self.lMedia.hide()
        self.tPersonalMessage.hide()
        self.bPersonalMessage.show()
        self.lPersonalMessage.show()
    def on_pm_clicked(self, *args):
        self.bMedia.hide()
        self.lMedia.hide()
        self.tPersonalMessage.show()
        self.bPersonalMessage.hide()
        self.lPersonalMessage.hide()
        self.tPersonalMessage.grab_focus()
    def personalMessageRefresh(self):
        self.controller.contacts.set_message(\
            self.tPersonalMessage.get_text())
        if not self.mediaButton.get_active():
            self.bMedia.hide()
            self.lMedia.hide()
            self.tPersonalMessage.hide()
            self.bPersonalMessage.show()
            self.lPersonalMessage.show()
            self.bPersonalMessage.grab_focus()
    def personalMessageChanged(self, msnp, user, pm):
        '''method called when the pm is changed in other place'''
        self.tPersonalMessage.set_text(pm)
        if pm == '':
            self.lPersonalMessage.set_text('<i>&lt;' + _('Click here to set your personal message') + '&gt;</i>')
        else:
                parser = self.controller.unifiedParser.getParser(pm, PangoDataType)
                pm = parser.get(self.hasSmilies, self.smileysCache)
                self.lPersonalMessage.set_markup(pm)
    def currentMediaChanged(self, msnp, user, cm, dict):
        '''method called when the current media is changed in other place'''
        if cm != '':
            cm = cm[cm.find('\\0Music\\01\\0')+12:]
            cmargs = cm.split('\\0')
            cm = cmargs[0]
            for args in range(1, len(cmargs)):
                cm = cm.replace('{%s}' %str(args-1), cmargs[args])
            self.lMedia.set_text('¿ ' + cm)
        else:
            self.lMedia.set_text(_("No media playing"))
    def on_personal_activate(self, *args):
        self.controller.contacts.set_message(\
            self.tPersonalMessage.get_text())
    def nickChanged(self, nick):
        self.tNick.set_text(nick)
    def setAvatar(self, pixbuf):
        if pixbuf:
            self.image.set_from_pixbuf(pixbuf)
    def onToggleMedia(self, widget, *args):
        if widget.get_active():
           self.bPersonalMessage.hide()
           self.lPersonalMessage.hide()
           self.tPersonalMessage.hide()
           self.bMedia.show()
           self.lMedia.show()
           self.controller.setMediaEnabled(True)
        else:
            self.bMedia.hide()
            self.lMedia.show()
            self.tPersonalMessage.hide()
            self.bPersonalMessage.show()
            self.lPersonalMessage.show()
            self.controller.setMediaEnabled(False)
    def onMaiButtonClicked(self, *args):
        try:
            desktop.open(self.controller.hotmail.getLoginPage())
        except OSError:
            dialog.error(_('Couldn\'t launch the default browser'))
class BaseImageButton:
    def __init__(self, icon, string=None):
        self.icon = icon
        self.image = gtk.Image()
        hbox = gtk.HBox()
        self.setIcon(icon)
        hbox.pack_start(self.image, True, True, 3)
        if string != None:
            self.label = gtk.Label(string)
            hbox.pack_start(self.label, False, False, 3)
        self.add(hbox)
    def setText(self, string):
        self.label.set_text(string)
    def getText(self):
        return self.label.get_text()
    def setIcon(self, icon):
        if type(icon) == gtk.gdk.PixbufAnimation:
            self.image.set_from_pixbuf(self.scaleImage(icon.get_static_image()))
        elif type(icon) == gtk.gdk.Pixbuf:
            self.image.set_from_pixbuf(self.scaleImage(icon))
        else:
            self.image.set_from_stock(gtk.STOCK_MISSING_IMAGE ,gtk.ICON_SIZE_SMALL_TOOLBAR)
    def getIcon(self):
        return self.icon
    def scaleImage(self, image):
        h,w = image.get_height(), image.get_width()
        width_max, height_max = 18, 16
        width=float(image.get_width())
        height=float(image.get_height())
        if (width/width_max) > (height/height_max):
            height=int((height/width)*width_max)
            width=width_max
        else:
            width=int((width/height)*height_max)
            height=height_max
        image = image.scale_simple(width, height, gtk.gdk.INTERP_BILINEAR)
        gc.collect() # Tell Python to clean up the memory
        return image
class ImageButton(gtk.Button, BaseImageButton):
    def __init__(self, icon, string=None):
        gtk.Button.__init__(self)
        BaseImageButton.__init__(self, icon, string)
class ImageToggleButton(gtk.ToggleButton, BaseImageButton):
   def __init__(self, icon, string=None):
        gtk.ToggleButton.__init__(self)
        BaseImageButton.__init__(self, icon, string)
class AvatarMenu(gtk.Menu):
    '''This class represents the avatar menu, where you can change your
    status or avatar'''
    def __init__(self, controller):
        '''Contructor'''
        gtk.Menu.__init__(self)
        self.controller = controller
        statusMenuItem = gtk.MenuItem(_('_Status'))
        statusMenu = StatusMenu.StatusMenu(self.controller)
        statusMenuItem.set_submenu(statusMenu)
        changeAvatarMenuItem = gtk.MenuItem(_('Change _display picture...'))
        changeAvatarMenuItem.connect('activate', self.changeAvatar)
        self.add(statusMenuItem)
        self.add(gtk.SeparatorMenuItem())
        self.add(changeAvatarMenuItem)
        self.show_all()
    def changeAvatar(self, *args):
        '''Open avatar dialog'''
        self.controller.set_picture_dialog()
