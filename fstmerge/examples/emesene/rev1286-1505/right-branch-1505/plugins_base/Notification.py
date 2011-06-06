import os
import gtk
import time
import gobject
import pango
import Plugin
import stock
import dialog
import desktop
import webbrowser
from emesenelib.common import escape
from emesenelib.common import unescape
from emesenecommon import PATH
from emesenecommon import RESPONSE_NO_AVATAR
from Theme import resizePixbuf
growFactor = 20 # the number of pixels to grow every iteration
class PixmapDialog(gtk.Window):
    '''a dialog to set Notification Pixmap'''
    def __init__(self, response, filename, font, color, online, offline, \
                newMsg, typing, newMail, started, idle, position, scroll):
        gtk.Window.__init__(self)
        self.set_title(_('Notification Config'))
        self.set_border_width(4)
        self.set_position(gtk.WIN_POS_CENTER)
        self.response_ok = response
        self.filename = filename
        self.fontdesc = font
        self.FColor = color
        self.hbox = gtk.HBox()
        self.lbox = gtk.HBox()
        self.image = gtk.Image()
        self.sample = gtk.Label()
        self.sample.set_label('<span foreground="%s">%s</span>' % \
            (self.FColor, _('Sample Text')))
        self.sample.set_use_markup(True)
        try:
            self.sample.modify_font(pango.FontDescription(self.fontdesc))
        except:
            print 'Font Error'
        self.fonttype = gtk.ToolButton()
        self.fonttype.set_stock_id(gtk.STOCK_SELECT_FONT)
        self.fontColor = gtk.ToolButton()
        self.fontColor.set_stock_id(gtk.STOCK_SELECT_COLOR)
        try:
            pixbuf = gtk.gdk.pixbuf_new_from_file(self.filename)
            pixbuf = resizePixbuf(pixbuf, 24,24)
            self.image.set_from_pixbuf(pixbuf)
        except:
            self.filename = None
        self.button = gtk.Button(_('Image'))
        self.hbox.pack_start(self.image)
        self.hbox.pack_start(self.button)
        self.lbox.pack_start(self.fonttype)
        self.lbox.pack_start(self.fontColor)
        self.vbox = gtk.VBox(spacing=4)
        self.vbox.pack_start(self.hbox)
        self.vbox.pack_start(self.sample)
        self.vbox.pack_start(self.lbox)
        self.button.connect('clicked', self.clickPixmap)
        self.fonttype.connect('clicked', self.clickFont)
        self.fontColor.connect('clicked', self.clickColor)
        self.chbuOnline = gtk.CheckButton(_('Notify when someone gets online'))
        self.chbuOnline.set_active(online)
        self.chbuOffline = gtk.CheckButton(_('Notify when someone gets offline'))
        self.chbuOffline.set_active(offline)
        self.chbuNewMail = gtk.CheckButton(_('Notify when receiving an email'))
        self.chbuNewMail.set_active(newMail)
        self.chbuTyping = gtk.CheckButton(_('Notify when someone starts typing'))
        self.chbuTyping.set_active(typing)
        self.chbuNewMsg = gtk.CheckButton(_('Notify when receiving a message'))
        self.chbuNewMsg.set_active(newMsg)
        self.chbuStarted = gtk.CheckButton(_('Don`t notify if conversation is started'))
        self.chbuStarted.set_active(started)
        self.chbuIdle = gtk.CheckButton(_('Disable notifications when busy'))
        self.chbuIdle.set_active(idle)
        self.lblPos = gtk.Label()
        self.lblPos.set_label(_('Position'))
        self.coboPosition = gtk.combo_box_new_text()
        self.coboPosition.append_text(_('Top Left'))
        self.coboPosition.append_text(_('Top Right'))
        self.coboPosition.append_text(_('Bottom Left'))
        self.coboPosition.append_text(_('Bottom Right'))
        self.coboPosition.set_active(position)
        self.pbox = gtk.HBox()
        self.pbox.pack_start(self.lblPos)
        self.pbox.pack_start(self.coboPosition)
        self.lblScr = gtk.Label()
        self.lblScr.set_label(_('Scroll'))
        self.coboScroll = gtk.combo_box_new_text()
        self.coboScroll.append_text(_('Horizontal'))
        self.coboScroll.append_text(_('Vertical'))
        self.coboScroll.set_active(scroll)
        self.sbox = gtk.HBox()
        self.sbox.pack_start(self.lblScr)
        self.sbox.pack_start(self.coboScroll)
        self.vbox.pack_start(self.chbuOnline)
        self.vbox.pack_start(self.chbuOffline)
        self.vbox.pack_start(self.chbuNewMail)
        self.vbox.pack_start(self.chbuTyping)
        self.vbox.pack_start(self.chbuNewMsg)
        self.vbox.pack_start(self.chbuStarted)
        self.vbox.pack_start(self.chbuIdle)
        self.vbox.pack_start(self.pbox)
        self.vbox.pack_start(self.sbox)
        b_accept = gtk.Button(stock=gtk.STOCK_OK)
        b_cancel = gtk.Button(stock=gtk.STOCK_CANCEL)
        b_accept.connect('clicked', self._on_response_ok)
        b_cancel.connect('clicked', self._on_response_cancel)
        hbbox = gtk.HButtonBox()
        hbbox.set_spacing(4)
        hbbox.set_layout(gtk.BUTTONBOX_END)
        hbbox.pack_start(b_cancel, False)
        hbbox.pack_start(b_accept, False)
        self.vbox.pack_start(hbbox)
        self.add(self.vbox)
        self.show_all()
    def clickPixmap(self, arg):
        def _on_image_selected(response, path):
            if response == stock.ACCEPT:
                self.filename = path
            try:
                pixbuf = gtk.gdk.pixbuf_new_from_file(self.filename)
                pixbuf = resizePixbuf(pixbuf, 24, 24)
                self.image.set_from_pixbuf(pixbuf)
                self.image.show()
            except:
                self.filename = None
                self.image.hide()
        dialog.ImageChooser(os.path.expanduser('~'), _on_image_selected).show()
    def clickFont(self, arg):
        fontDialog = gtk.FontSelectionDialog(_('Choose a font'))
        if self.fontdesc != None:
            fontDialog.set_font_name(self.fontdesc)
        response = fontDialog.run()
        if response == gtk.RESPONSE_OK:
            pangoDesc = pango.FontDescription(fontDialog.get_font_name())
            self.sample.modify_font(pangoDesc)
            self.fontdesc = pangoDesc.to_string()
        fontDialog.destroy()
    def clickColor(self, arg):
        colorDialog = gtk.ColorSelectionDialog(_('Choose a color'))
        colorDialog.colorsel.set_has_palette(True)
        response = colorDialog.run()
        if response == gtk.RESPONSE_OK:
            color = colorDialog.colorsel.get_current_color()
            red = color.red >> 8
            green = color.green >> 8
            blue = color.blue >> 8
            self.FColor = '#%02X%02X%02X' % (red, green, blue)
            self.sample.set_label('<span foreground="%s">%s</span>' % \
                    (self.FColor, _('Sample Text')))
            self.sample.set_use_markup(True)
        colorDialog.destroy()
    def _on_response_ok(self, button):
         self.destroy()
         self.response_ok(self.filename, self.fontdesc, self.FColor, \
                 int(self.chbuOnline.get_active()), \
                 int(self.chbuOffline.get_active()), \
                 int(self.chbuNewMsg.get_active()), \
                 int(self.chbuTyping.get_active()), \
                 int(self.chbuNewMail.get_active()), \
                 int(self.chbuStarted.get_active()), \
                 int(self.chbuIdle.get_active()), \
                 self.coboPosition.get_active(), \
                 self.coboScroll.get_active())
    def _on_response_cancel(self, button):
         self.destroy()
class NotificationManager:
    ''' This class manages the creation display and destruction of the notifications. '''
    def __init__(self, defaultHeight = 128, defaultWidth = 200):
        ''' Contructor '''
        self.defaultHeight = defaultHeight
        self.defaultWidth = defaultWidth
        self.offset = 0
        self.list = []
        self.animate = None
    def newNotification(self, string, pos, scroll, pixmap = None, \
                closePixmap = None, callback = None, params = None, \
                userPixbuf = None, font = None, color = None):
        '''
        create a new notification, pixmap is the background image (as a pixbuf),
        closepixmap is a pixbuf for the close button.
        callback is the method that will be called when the message in the Notification
        is clicked
        '''
        if pixmap != None:
            width, height = pixmap.get_size()
        else:
            width = self.defaultWidth
            height = self.defaultHeight
        rgb = gtk.gdk.screen_get_default().get_rgb_colormap()
        gtk.widget_push_colormap(rgb)
        g = Notification(pos, scroll, self.offset, string, height, width, \
                pixmap, closePixmap, callback, params, userPixbuf, font, color)
        g.show()
        gtk.widget_pop_colormap()
        self.offset = g.getOffset()
        self.list.append([g, int(time.time())])
        if len(self.list) <= 1:
            self.animate = gobject.timeout_add(100, self.refresh)
    def refresh(self):
        '''
        check which notifications should be closed
        resize and move notifications
        '''
        self.offset = 0
        if self.list == []:
            return False
        else:
            timestamp = int(time.time())
            count = 0
            for i in self.list:
                if not i[0].get_property('visible'):
                    del self.list[count]
                elif i[1] + 7 <= timestamp:
                    i[0].hide()
                    del self.list[count]
                else:
                    self.list[count][0].grow(self.offset)
                    self.offset = self.list[count][0].getOffset()
                count += 1
            return True
    def closeAll(self):
        ''' close all the notifications '''
        if self.animate:
            gobject.source_remove(self.animate)
        for i in range(len(self.list)):
            self.list[i][0].hide()
        self.offset = 0
        self.list = []
class Notification(gtk.Window):
    def __init__(self, corner, scroll, offset, string, height = 128, \
                width = 200, pixmap = None, closePixmap = None, \
                callback = None, params = None, userPixbuf = None, \
                font = None, color = None):
        gtk.Window.__init__(self)
        if corner == 0:
            self.set_gravity(gtk.gdk.GRAVITY_NORTH_WEST)
        elif corner == 1:
            self.set_gravity(gtk.gdk.GRAVITY_NORTH_EAST)
        elif corner == 2:
            self.set_gravity(gtk.gdk.GRAVITY_SOUTH_WEST)
        else:
            self.set_gravity(gtk.gdk.GRAVITY_SOUTH_EAST)
        self.set_property('can-focus', False)
        self.set_property('accept-focus', False)
        self.corner = corner
        self.scroll = scroll
        if scroll == 0:
            self.height = height
            self.max = width
            self.width = 1
        else:
            self.width = width
            self.max = height
            self.height = 1
        self.callback = callback
        self.set_geometry_hints(None, min_width=-1, min_height=-1, \
                max_width=width, max_height=height)
        self.set_accept_focus(False)
        self.set_decorated(False)
        self.set_keep_above(True)
        self.set_skip_taskbar_hint(True)
        self.set_skip_pager_hint(True)
        if pixmap != None:
            self.set_app_paintable(True)
            self.realize()
            self.window.set_back_pixmap(pixmap, False)
        messageLabel = gtk.Label('<span foreground="' + color +'">' \
                + escape(str(string)) + '</span>')
        messageLabel.set_use_markup(True)
        messageLabel.set_justify(gtk.JUSTIFY_CENTER)
        messageLabel.set_ellipsize(pango.ELLIPSIZE_END)
        try:
            messageLabel.modify_font(pango.FontDescription(font))
        except e:
            print e
        if closePixmap == None:
            close = gtk.Label()
            close.set_label("<span background=\"#cc0000\" foreground=" \
                    + color + "\"> X </span>")
            close.set_use_markup(True)
        else:
            close = gtk.Image()
            close.set_from_pixbuf(closePixmap)
        closeEventBox = gtk.EventBox()
        closeEventBox.set_visible_window(False)
        closeEventBox.set_events(gtk.gdk.BUTTON_PRESS_MASK)
        closeEventBox.connect("button_press_event", self.close)
        closeEventBox.add(close)
        hbox = gtk.HBox()
        vbox = gtk.VBox()
        lbox = gtk.HBox()
        title = gtk.Label("")
        title.set_use_markup(True)
        avatarImage = gtk.Image()
        if userPixbuf != None:
            userPixbuf = resizePixbuf(userPixbuf, 48, 48)
            avatarImage.set_from_pixbuf(userPixbuf)
        lboxEventBox = gtk.EventBox()
        lboxEventBox.set_visible_window(False)
        lboxEventBox.set_events(gtk.gdk.BUTTON_PRESS_MASK)
        lboxEventBox.connect("button_press_event", self.onClick, params)
        lboxEventBox.add(lbox)
        hbox.pack_start(title, True, True)
        hbox.pack_end(closeEventBox, False, False)
        lbox.pack_start(avatarImage, False, False, 10)
        lbox.pack_start(messageLabel, True, True, 5)
        vbox.pack_start(hbox, False, False)
        vbox.pack_start(lboxEventBox, True, True)
        self.grow(offset, False)
        self.add(vbox)
        vbox.show_all()
    def onClick(self, widget, event, params):
        if event.button == 1 and self.callback != None:
            self.callback(params)
        self.close()
    def resize(self):
        ''' change the size and position '''
        if self.scroll == 0:
            if self.corner == 0 or self.corner == 2:
                l = self.offset
            else:
                l = gtk.gdk.screen_width() - self.offset - self.width
            if self.corner == 0 or self.corner == 1:
                t = 0
            else:
                t = gtk.gdk.screen_height() - self.height
        else:
            if self.corner == 0 or self.corner == 2:
                l = 0
            else:
                l = gtk.gdk.screen_width() - self.width
            if self.corner == 0 or self.corner == 1:
                t = self.offset
            else:
                t = gtk.gdk.screen_height() - self.offset - self.height
        gtk.Window.move(self, l, t)
        gtk.Window.resize(self, self.width, self.height)
    def show(self):
        ''' show it '''
        gtk.Window.show(self)
    def close(self , *args):
        ''' hide the Notification '''
        self.hide()
    def grow(self, offset, animate=True):
        ''' increase the size of the notification and position '''
        if animate and offset < self.offset:
            self.offset -= growFactor
            if offset > offset:
                self.offset = offset
        else:
            self.offset = offset
        if self.scroll == 0:
            if self.width < self.max:
                if self.width > self.max:
                    self.width = self.max
                else:
                    self.width += growFactor
        else:
            if self.height < self.max:
                if self.height + growFactor > self.max:
                    self.height = self.max
                else:
                    self.height += growFactor
        self.resize()
    def getOffset(self):
        ''' returns next notifications offset '''
        if self.scroll == 0:
            if self.corner == 0 or self.corner == 2:
                return self.get_position()[0] + self.width
            else:
                return gtk.gdk.screen_width() - self.get_position()[0]
        else:
            if self.corner == 0 or self.corner == 1:
                return self.get_position()[1] + self.height
            else:
                return gtk.gdk.screen_height() - self.get_position()[1]
class MainClass(Plugin.Plugin):
    '''
    The notification plugin
    '''
    def __init__(self, controller, msn):
        ''' Constructor '''
        Plugin.Plugin.__init__(self, controller, msn, 1000)
        self.theme = controller.theme
        self.config = controller.config
        self.description = _('Show a little window in the bottom-right corner of the screen when someone gets online etc.')
        self.authors = { 'Mariano Guerra' : 'luismarianoguerra at gmail dot com' }
        self.website = 'http://emesene-msn.blogspot.com'
        self.displayName = _('Notification')
        self.name = 'Notification'
        self.config.readPluginConfig(self.name)
        self.controller = controller
        self.filename = self.config.getPluginValue(self.name, 'filename', PATH.DEFAULT_THEME_PATH + 'guif.png')
        self.fontname  = self.config.getPluginValue(self.name, 'fontname', 'Sans')
        self.fontcolor = self.config.getPluginValue(self.name, 'fontcolor', '#000000')
        self.notifyOnline  = int(self.config.getPluginValue(self.name, 'online', '1'))
        self.notifyOffline = int(self.config.getPluginValue(self.name, 'offline', '1'))
        self.notifyNewMail = int(self.config.getPluginValue(self.name, 'newMail', '1'))
        self.notifyNewMsg = int(self.config.getPluginValue(self.name, 'newMsg', '1'))
        self.notifyTyping = int(self.config.getPluginValue(self.name, 'typing', '0'))
        self.notifyStarted = int(self.config.getPluginValue(self.name, 'started', '0'))
        self.notifyIdle = int(self.config.getPluginValue(self.name, 'idle', '1'))
        self.position = int(self.config.getPluginValue(self.name, 'position', '3'))
        self.scroll = int(self.config.getPluginValue(self.name, 'scroll', '1'))
        self.onlineId = None
        self.offlineId = None
        self.newMsgId = None
        self.offMsgId = None
        self.typingId = None
        self.newMailId = None
        self.initMailId = None
    def notifyEnabled(self, contact = None):
        '''checks if notifications are enabled'''
        if self.notifyIdle and self.msn.status == 'BSY':
            return False
        if contact != None:
            if self.controller.contacts.get_blocked(contact):
                return False
        return True
    def online(self, msnp, email, oldStatus):
        ''' called when someone get online '''
        if not (self.notifyOnline and self.notifyEnabled(email)):
            return
        if oldStatus != 'FLN':
            return
        nick = unescape(self.controller.unifiedParser.getParser\
                (self.msn.getUserDisplayName(email)).get())
        contact = self.msn.contactManager.getContact(email)
        userPixbuf = self.theme.getUserDisplayPicture(contact)
        self.notificationManager.newNotification(unicode(nick)[:20] \
                + "\n" + _("is online"), self.position, self.scroll, \
                self.pixmap, self.close, self.startConversation, \
                (email, None), userPixbuf, self.fontname, self.fontcolor)
    def offline(self, msnp, email):
        ''' called when someone get offline '''
        if not (self.notifyOffline and self.notifyEnabled(email)):
            return
        nick = unescape(self.controller.unifiedParser.getParser\
                (self.msn.getUserDisplayName(email)).get())
        contact = self.msn.contactManager.getContact(email)
        userPixbuf = self.theme.getUserDisplayPicture(contact)
        self.notificationManager.newNotification(unicode(nick)[:20] \
                + "\n" + _("is offline"), self.position, self.scroll, \
                self.pixmap, self.close, None, None, userPixbuf, \
                self.fontname, self.fontcolor)
    def newMsg(self, msnp, email):
        '''called when someone sent a message'''
        if not (self.notifyNewMsg and self.notifyEnabled(email)):
            return
        result = self.controller.conversationManager.getOpenConversation(email)
        if result != None:
            if self.notifyStarted:
                return
            window, conversation = result
            windowFocus = window.is_active()
            tabFocus = (window.conversation == conversation)
            if windowFocus and tabFocus:
                return
        nick = unescape(self.controller.unifiedParser.getParser\
                (self.msn.getUserDisplayName(email)).get())
        contact = self.msn.contactManager.getContact(email)
        userPixbuf = self.theme.getUserDisplayPicture(contact)
        self.notificationManager.newNotification(unicode(nick)[:20] \
                + "\n" + _('has sent a message'), self.position, self.scroll, \
                self.pixmap, self.close, self.startConversation, \
                (email, None), userPixbuf, self.fontname, self.fontcolor)
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
        nick = unescape(self.controller.unifiedParser.getParser\
                (self.msn.getUserDisplayName(email)).get())
        contact = self.msn.contactManager.getContact(email)
        userPixbuf = self.theme.getUserDisplayPicture(contact)
        self.notificationManager.newNotification(unicode(nick)[:20] \
                + "\n" + _('sent an offline message'), self.position, \
                self.scroll, self.pixmap, self.close, self.startConversation, \
                (email, None), userPixbuf, self.fontname, self.fontcolor)
    def receiveTyping(self, msn, switchboard, signal, args):
        '''called when someone starts typing'''
        email = args[0]
        if not (self.notifyTyping and self.notifyEnabled(email)):
            return
        if self.controller.conversationManager.getOpenConversation\
                (email, switchboard) != None:
            return
        nick = unescape(self.controller.unifiedParser.getParser\
                (self.msn.getUserDisplayName(email)).get())
        contact = self.msn.contactManager.getContact(email)
        userPixbuf = self.theme.getUserDisplayPicture(contact)
        self.notificationManager.newNotification(unicode(nick)[:20] \
                + "\n" + _('starts typing...'), self.position, self.scroll, \
                self.pixmap, self.close, self.startConversation, \
                (email, switchboard), userPixbuf, self.fontname, self.fontcolor)
    def newMail(self, msnp, From, FromAddr, Subject, MessageURL, PostURL, id):
        ''' called when receiving mail '''
        if not (self.notifyNewMail and self.notifyEnabled(FromAddr)):
            return
        contact = self.msn.contactManager.getContact(FromAddr)
        if contact == None:
            text = _('From: ') + From +' &lt;' + FromAddr + '&gt;'
            userPixbuf = None
        else:
            text = _('From: ') + unescape(self.controller.unifiedParser.\
                getParser(self.msn.getUserDisplayName(FromAddr)).get())
            userPixbuf = self.theme.getUserDisplayPicture(contact)
        text += '\n' + _('Subj: ') + escape(Subject)
        self.notificationManager.newNotification(_('New email') \
                + "\n" + text, self.position, self.scroll, self.pixmap, \
                self.close, self.openMail, (MessageURL, PostURL, id), \
                userPixbuf, self.fontname, self.fontcolor)
    def initMail(self, msnp):
        ''' called when receiving initial mail count '''
        if self.notifyNewMail:
            unread = self.controller.getUnreadMails()
            if unread > 0:
                if unread == 1:
                    s = ''
                else:
                    s = 's'
                params = {'num': unread, 's': s}
                self.notificationManager.newNotification(
                        _('You have %(num)i unread message%(s)s') % params, \
                        self.position, self.scroll, self.pixmap, self.close, \
                        self.openMail, (None, None, '2'), None, \
                        self.fontname, self.fontcolor)
    def startConversation(self, params):
        self.controller.newConversation(None, params[0], params[1], True)
    def openMail(self, params):
        desktop.open(self.controller.hotmail.getLoginPage\
                (params[0], params[1], params[2]))
    def start(self):
        ''' start the plugin '''
        self.notificationManager = NotificationManager(128, 200)
        self.onlineId = self.connect('user-online', self.online)
        self.offlineId = self.connect('user-offline', self.offline)
        self.newMsgId = self.connect('message-received', self.newMsg)
        self.offMsgId = self.connect('offline-message-received', self.offMsg)
        self.typingId = self.connect('switchboard::typing', self.receiveTyping)
        self.newMailId = self.connect('new-mail-notification', self.newMail)
        self.initMailId = self.connect\
                ('initial-mail-notification', self.initMail)
        try:
            self.pixmap, mask = gtk.gdk.pixbuf_new_from_file\
                    (self.filename).render_pixmap_and_mask()
        except:
            self.pixmap = None
        try:
            self.close = self.theme.getImage('close')
        except:
            self.close = None
        self.enabled = True
    def stop(self):
        ''' stop the plugin '''
        self.disconnect(self.onlineId)
        self.disconnect(self.offlineId)
        self.disconnect(self.newMsgId)
        self.disconnect(self.offMsgId)
        self.disconnect(self.typingId)
        self.disconnect(self.newMailId)
        self.disconnect(self.initMailId)
        self.enabled = False
        self.notificationManager.closeAll()
    def check(self):
        '''
        check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> (True , 'some message')
        else -> (False , 'error message')
        '''
        return (True, 'Ok')
    def configure(self):
        def _on_configure_ok(filename, font, color, online, offline, \
                newMsg, typing, newMail, started, idle, position, scroll):
            self.filename = filename
            self.fontname = font
            self.fontcolor = color
            self.notifyOnline = online
            self.notifyOffline = offline
            self.notifyNewMsg = newMsg
            self.notifyTyping = typing
            self.notifyNewMail = newMail
            self.notifyStarted = started
            self.notifyIdle = idle
            self.position = position
            self.scroll = scroll
            self.config.setPluginValue(self.name, 'filename', filename)
            self.config.setPluginValue(self.name, 'fontname', font)
            self.config.setPluginValue(self.name, 'fontcolor', color)
            self.config.setPluginValue(self.name, 'online', online)
            self.config.setPluginValue(self.name, 'offline', offline)
            self.config.setPluginValue(self.name, 'newMsg', newMsg)
            self.config.setPluginValue(self.name, 'typing', typing)
            self.config.setPluginValue(self.name, 'newMail', newMail)
            self.config.setPluginValue(self.name, 'started', started)
            self.config.setPluginValue(self.name, 'idle', idle)
            self.config.setPluginValue(self.name, 'position', position)
            self.config.setPluginValue(self.name, 'scroll', scroll)
            try:
                self.pixmap, mask = gtk.gdk.pixbuf_new_from_file\
                        (filename).render_pixmap_and_mask()
            except:
                self.pixmap = None
        PixmapDialog(_on_configure_ok, self.filename, \
                self.fontname, self.fontcolor, self.notifyOnline, \
                self.notifyOffline, self.notifyNewMsg, self.notifyTyping, \
                self.notifyNewMail, self.notifyStarted, self.notifyIdle, \
                self.position, self.scroll).show()
        return True
