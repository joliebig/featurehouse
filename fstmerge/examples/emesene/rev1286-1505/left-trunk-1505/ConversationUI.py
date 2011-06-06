import gtk
import time
import pango
import gobject
import os
import urllib
import Webcam
import UserList
import SmilieWindow
import FontStyleWindow
import FileTransferUI
from Theme import resizePixbuf
from Parser import PangoDataType
import emesenelib.common
import emesenelib.ContactData
import Widgets
from htmltextview import HtmlTextView
class ConversationUI(gtk.VBox):
    '''this class represent all the widgets that are inside a tab
    also hold the tab widget because there is no better place than
    this to hold it...'''
    def __init__(self, controller, parentConversation):
        gtk.VBox.__init__(self, spacing=3)
        self.set_border_width(0)
        self.parentConversation = parentConversation
        self.controller = controller
        self.config = self.controller.config
        self.parser = controller.unifiedParser
        self.header = Header(self, controller)
        self.tabWidget = TabWidget(self, controller)
        self.tabWidget.show()
        self.input = InputWidget(self, controller, \
            parentConversation.isCurrent)
        self.input.show()
        self.status = gtk.Statusbar()
        self.toolbarinput = gtk.HBox()
        self.toolbarinput.show()
        self.listOfUsers = UserList.UserList(self.controller, \
            self.controller.theme, self.config, False)
        self.scrollList = gtk.ScrolledWindow()
        self.scrollList.set_shadow_type(gtk.SHADOW_IN)
        self.scrollList.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.scrollList.set_size_request(111, 0)
        self.scrollList.add(self.listOfUsers)
        self.scroll = gtk.ScrolledWindow()
        self.scroll.set_shadow_type(gtk.SHADOW_IN)
        self.scroll.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.textview = HtmlTextView(controller, \
            self.parentConversation.textBuffer, self.scroll)
        self.textview.set_wrap_mode(gtk.WRAP_WORD_CHAR)
        self.textview.set_left_margin(6)
        self.textview.set_right_margin(6)
        self.textview.set_editable(False)
        self.textview.set_cursor_visible(False)
        self.textview.connect('key-press-event', self.onTextviewKeyPress)
        self.scroll.add(self.textview)
        self.scroll.show_all()
        self.remoteAvatar = AvatarHBox(self, controller,
            self.parentConversation.switchboard.firstUser)
        self.controller.msn.connect('user-attr-changed', self.onUserAttrChanged)
        self.vboxside = gtk.VBox()
        self.vboxside.pack_start(self.remoteAvatar, False, False)
        self.vboxside.pack_start(self.scrollList, True, True)
        self.vboxside.show()
        self.hbox = gtk.HBox(spacing=2)
        self.hbox.set_border_width(2)
        self.hbox.pack_start(self.scroll, True, True)
        self.hbox.pack_start(self.vboxside, False, False)
        self.hbox.show()
        self.toolbarinput.pack_start(self.input, True, True)
        self.toolbarinput.connect('size-allocate', self.onToolbarinputResize)
        vpaned = gtk.VPaned()
        vpaned.pack1(self.hbox, True, True)
        vpaned.pack2(self.toolbarinput, False)
        vpaned.show()
        self.transfers = FileTransferUI.FtBarWidget(self.controller, self,
                                        parentConversation)
        self.transfers.set_no_show_all(True)
        self.pack_start(self.header, False, False)
        self.pack_start(vpaned, True, True)
        self.pack_start(self.transfers, False, False)
        self.pack_start(self.status, False, False)
        self.messageWaiting = {}
        self.contactTyping = {}
        self.typingTimeoutID = 0
        self.closed = False
        self.last_mark = None
        self.show()
        self.update()
    def onToolbarinputResize(self, *args):
        alloc = self.toolbarinput.get_allocation()
        self.config.user['convInputHeight'] = alloc.height
        self.scrollToBottom()
    def onTextviewKeyPress(self, widget, event):
        if event.keyval == gtk.keysyms.Control_L or \
                event.keyval == gtk.keysyms.Control_R or \
                event.state & gtk.gdk.CONTROL_MASK:
            return
        self.input.input.emit('key-press-event', event)
        self.input.grabFocus()
    def close(self):
        self.closed = True
    def setInputEnabled(self, enable):
        self.input.input.set_sensitive(enable)
    def onUserAttrChanged(self, msn, contact):
        if contact.email in self.parentConversation.getMembers():
            self.update()
    def noneIsTyping(self):
        try:
            for contact in self.email_list:
                if self.contactTyping[contact] == True:
                    return False
            return True
        except:
            return False
    def update(self):
        self.header.update()
        self.tabWidget.update()
        self.input.update()
        self.email_list = [mail.email for mail in self.parentConversation.getMembersDict().values()]
        for email in self.email_list:
            if email not in self.contactTyping.keys():
                self.contactTyping[email] = False
            if email not in self.messageWaiting.keys():
                self.messageWaiting[email] = False
        for email in self.contactTyping.keys():
            if self.contactTyping[email]:
                self.setTyping(email)
            else:
                self.setDefault(email)
        isGroupChat = len(self.parentConversation.getMembers()) > 1
        if isGroupChat:
            members = self.parentConversation.getMembersDict()
            d = {'members' : emesenelib.ContactData.Group('members')}
            for member in members.values():
                d['members'].setUser(member.email, member)
            self.listOfUsers.fill(d)
        if isGroupChat:
            self.showUserList()
            self.remoteAvatar.hide()
        elif self.config.user['showAvatars']:
            self.hideUserList()
            self.remoteAvatar.show_all()
            self.remoteAvatar.update()
        else:
            self.hideUserList()
            self.remoteAvatar.hide()
        if not self.config.user['showHeader']:
            self.header.hide()
        else:
            self.header.show()
        if not self.config.user['showTabCloseButton']:
            self.tabWidget.closeButton.hide()
        else:
            self.tabWidget.closeButton.show()
        if not self.config.user['showStatusBar']:
            self.status.hide()
        else:
            self.status.show()
        inputHeight = self.config.user['convInputHeight']
        self.toolbarinput.set_size_request(0, inputHeight)
    def scrollToBottom(self):
        '''scroll to the end of the conversation'''
        self.textview.scrollToBottom()
    def showUserList(self):
        self.scrollList.show_all()
        self.listOfUsers.show()
    def hideUserList(self):
        self.scrollList.hide()
        self.listOfUsers.hide()
    def rebuildStatusText(self):
        '''Builds the text displayed in the statusbar, based in
        self.contactTyping. The "output" is a comma separated list
        of (full) mails and "is/are typing a message..." '''
        mails = [x for x in self.contactTyping.keys() \
                  if self.contactTyping[x] == True]
        string = ''
        if len(mails) > 0:
            comma = ', ' # TODO: gettext?
            for mail in mails:
                if self.config.user['showMailTyping']:
                    string += str(mail)
                else:
                    contact = self.controller.getContact(mail)
                    if contact:
                        parts = self.parser.getParser(contact.nick).get()
                        for part in parts:
                            string += str(part)
                    else:
                        string += str(mail)
                string += comma
            string = str(unicode(string)[:-len(comma)])
            if len(mails) == 1:
                string += ' ' + _('is typing a message...')
            else:
                string += ' ' + _('are typing a message...')
        self.status.get_children()[0].get_children()[0].set_text(string)
    def setMessageWaiting(self, mail):
        if self.parentConversation.isCurrent:
            return self.setDefault(mail)
        self.tabWidget.setMessageWaiting()
        self.header.setDefault()
        if mail:
            self.contactTyping[mail] = False
            self.messageWaiting[mail] = True
            self.rebuildStatusText()
    def setTyping(self, mail):
        if self.messageWaiting.has_key(mail) and self.messageWaiting[mail]: return
        self.header.setTyping()
        self.tabWidget.setTyping()
        self.contactTyping[mail] = True
        self.rebuildStatusText()
        if self.typingTimeoutID > 0:
            gobject.source_remove(self.typingTimeoutID)
        self.typingTimeoutID = gobject.timeout_add(8000, \
            self.clearTyping, mail)
    def clearTyping(self, mail):
        if mail in self.messageWaiting and self.messageWaiting[mail]:
            self.setMessageWaiting(mail)
        else:
            self.setDefault(mail)
        self.contactTyping[mail] = False
        return False
    def setDefault(self, mail):
        self.tabWidget.setDefault()
        self.header.setDefault()
        if mail:
            self.contactTyping[mail] = False
            self.messageWaiting[mail] = False
            self.rebuildStatusText()
class Header(gtk.HBox):
    '''the header of the conversation'''
    def __init__(self, parentUI, controller):
        '''Contructor'''
        gtk.HBox.__init__(self)
        self.parentUI = parentUI
        self.controller = controller
        self.config = controller.config
        self.set_border_width(2)
        self.set_spacing(2)
        self.parser = controller.unifiedParser
        self.statusImage = gtk.Image()
        self.statusImage.set_from_pixbuf(self.controller.theme.getImage('groupChat'))
        camicon = gtk.Image()
        camicon.set_from_pixbuf(self.controller.theme.getImage('cam'))
        camicon.show()
        self.btn_ask_webcam = gtk.Button()
        self.btn_ask_webcam.add(camicon)
        self.btn_ask_webcam.set_relief(gtk.RELIEF_NONE)
        self.btn_ask_webcam.connect('clicked', self.ask_webcam_clicked)
        self.user = self.controller.getContact(self.parentUI.parentConversation.getMembers()[0])
        tooltips = gtk.Tooltips()
        tooltips.set_tip(self.btn_ask_webcam, _('View webcam'))
        tooltips.enable()
        if self.user.shares_webcam() and Webcam.libmimic:
            self.btn_ask_webcam.show()
        self.text = ''
        self.label = gtk.Label()
        self.label.set_selectable(True)
        self.label.unset_flags(gtk.CAN_FOCUS)
        self.label.set_alignment(0.0, 0.5)
        self.label.set_line_wrap(True)
        self.label.set_ellipsize(pango.ELLIPSIZE_END)
        self.label.set_markup('<b><i>' + _('Connecting...') + '</i></b>')
        self.reconnect = gtk.Button(_('Reconnect'))
        self.reconnect.connect('clicked', self.reconnectClicked)
        self.pack_start(self.statusImage, False, False)
        self.pack_start(self.label)
        self.pack_start(self.reconnect, False, False)
        self.pack_start(self.btn_ask_webcam, False, False)
        self.statusImage.show()
        self.label.show()
        self.show()
    def reconnectClicked(self, button):
        self.parentUI.parentConversation.reconnect()
        self.reconnect.hide()
    def update(self):
        conversation = self.parentUI.parentConversation
        if self.config.user['showTabImageStatus']:
            self.statusImage.hide()
            return
        if conversation.switchboard.status == 'error':
            self.setLabel()
            self.parentUI.parentConversation.parentConversationWindow.menu.update()
        self.statusImage.set_from_pixbuf(conversation.getStatusIcon())
    def setTyping(self):
        typingColor = self.config.user['typingColor']
        styleOpen = "<b><span foreground='" + typingColor + "'>"
        styleClose = "</span></b>"
        self.setLabel(styleOpen, styleClose)
    def setLabel(self, styleOpen='<b>', styleClose='</b>'):
        '''Sets the header label, the one that often includes bold nick,
        gray psm and mail. It also checks the switchboard status.'''
        conversation = self.parentUI.parentConversation
        members = conversation.getMembersDict()
        personalMessageColor = self.config.user['personalMessageColor']
        switchboard = conversation.switchboard
        if switchboard.status == 'error':
            if switchboard.error == 215:
                desc = _('The user is already present')
            elif switchboard.error == 217:
                desc = _('The user is offline')
            else:
                desc = str(switchboard.error)
            self.text = '%s<i> %s (%s) </i>%s' % \
                (styleOpen, _('Can\'t connect'), desc, styleClose)
            self.label.set_markup(self.text)
            self.btn_ask_webcam.hide()
            self.reconnect.show()
            return
        self.reconnect.hide()
        if self.user.shares_webcam():
            self.btn_ask_webcam.show()
        if len(members) == 1:
            contact = members.values()[0]
            parts = self.parser.getParser(contact.nick, PangoDataType).get()
            nick = ''
            for part in parts:
                nick += str(part)
            status = self.controller.status_ordered[0].index(contact.status)
            status =  self.controller.status_ordered[2][status]
            self.text = self.config.user['convHeaderTheme']
            self.text = self.text.replace('%nick%', styleOpen+nick+styleClose)
            self.text = self.text.replace('%email%', contact.email)
            self.text = self.text.replace('%pmColor%', personalMessageColor)
            self.text = self.text.replace('%status%', status)
            self.text = self.text.replace('\\n', "\n")
            if contact.personalMessage:
                pmparts = self.parser.getParser(contact.personalMessage, PangoDataType).get()
                pmp = ''
                for pmpart in pmparts:
                    pmp += str(pmpart)
                self.text = self.text.replace('%pm%', pmp)
            else:
                self.text = self.text.replace('%pm%\n', '').replace('%pm%', '')
        elif len(members) > 1:
            self.text = styleOpen + _('Group chat') + styleClose + \
                "\n<span foreground='%s'>" % personalMessageColor + \
                str(len(members)) + ' ' + _('Members') + "</span>"
            self.statusImage.set_from_pixbuf(
                self.controller.theme.getImage('groupChat') )
        else:
            self.text = styleOpen + '<i>' + _('Connecting...') + '</i>' + styleClose
        self.label.set_markup(self.text)
    def ask_webcam_clicked(self, *args):
        self.parentUI.parentConversation.view_webcam()
    def setDefault(self):
        self.setLabel()
class TabWidget(gtk.HBox):
    '''this class represent the widget that is inside the tab
    it contains a label and a close button'''
    def __init__(self, parentUI, controller):
        '''Contructor'''
        gtk.HBox.__init__(self)
        self.parentUI = parentUI
        self.controller = controller
        self.config = controller.config
        self.text = ''
        self.markup = ''
        self.updateText()
        self.label = gtk.Label()
        self.label.set_use_markup(True)
        self.label.set_markup(self.markup)
        self.label.set_ellipsize(pango.ELLIPSIZE_END)
        img = gtk.Image()
        img.set_from_stock(gtk.STOCK_CLOSE, gtk.ICON_SIZE_MENU)
        self.closeButton = gtk.Button()
        self.closeButton.set_image(img)
        self.closeButton.set_relief(gtk.RELIEF_NONE)
        self.statusImage = gtk.Image()
        self.pack_start(self.statusImage, False, False)
        self.pack_start(self.label)
        self.pack_start(self.closeButton, False, False)
        self.closeButton.connect('clicked', self.onCloseClicked)
        self.show_all()
    def onCloseClicked(self, button):
        self.parentUI.parentConversation.close()
    def update(self):
        self.updateText()
        members = self.parentUI.parentConversation.getMembersDict()
        if len(members) == 1:
            contact = members.values()[0]
            basepixbuf = self.controller.theme.statusToPixbuf(contact.status)
        else:
            basepixbuf = self.controller.theme.getImage('userPanel')
        self.statusImage.set_from_pixbuf(resizePixbuf(basepixbuf, 16, 16))
    def updateText(self):
        title = self.parentUI.parentConversation.getTitle()
        parts = self.controller.unifiedParser.getParser(title,
            PangoDataType).get()
        self.markup = ''.join([str(x) for x in parts])
        self.text = self.controller.unifiedParser.getParser(title).get()
    def setDefault(self):
        self.label.set_markup(self.markup)
    def setTyping(self):
        typingColor = self.config.user['typingColor']
        self.label.set_markup('<span foreground="%s">%s</span>' % (typingColor, self.text))
    def setMessageWaiting(self):
        messageWaitingColor = self.config.user['messageWaitingColor']
        self.label.set_markup('<span foreground="%s">%s</span>' % (messageWaitingColor, self.text))
class AvatarHBox(gtk.HBox):
    def __init__(self, parentUI, controller, mail='', localavatar=False):
        gtk.HBox.__init__(self)
        self.parentUI = parentUI
        self.controller = controller
        self.config = controller.config
        self.mail = mail
        self.hidebutton = gtk.Button()
        self.hidebutton.connect('clicked', self.hideShowButton)
        self.hidebutton.set_relief(gtk.RELIEF_NONE)
        self.hidebuttonArrow = Widgets.TinyArrow(gtk.ARROW_RIGHT)
        self.hidebutton.add(self.hidebuttonArrow)
        self.image = Widgets.avatarHolder(cellKeyPosition=gtk.ANCHOR_SOUTH)
        self.imageEventBox = gtk.EventBox()
        self.imageEventBox.set_events(gtk.gdk.BUTTON_PRESS_MASK)
        self.imageEventBox.set_visible_window(False)
        self.imageEventBox.add(self.image)
        if mail:
            contact = self.controller.getContact(mail)
            if contact:
                self.image.set_from_pixbuf(
                    self.controller.theme.getUserDisplayPicture(contact))
            self.controller.msn.connect('display-picture-changed',
                self.on_display_picture_changed)
            self.key = 'showAvatarOther'
        else:
            if self.controller.avatar:
                self.image.set_from_pixbuf(self.controller.avatar.getImage())
            else:
                self.image.set_from_pixbuf(
                    self.controller.theme.getImage('icon96'))
            self.controller.connect('avatar-changed',
                self.on_display_picture_changed)
            self.imageEventBox.connect('button-press-event',
                lambda w, e: self.controller.set_picture_dialog())
            self.key = 'showAvatarMine'
        self.filler = gtk.VBox()
        self.vboxButtonFrame = gtk.VBox()
        self.vboxButtonFrame.pack_start(self.hidebutton, False, False)
        self.vboxButtonFrame.pack_start(self.filler, True, True)
        self.filler2 = gtk.VBox()
        self.vboxAvatarFrame = gtk.VBox()
        self.vboxAvatarFrame.pack_start(self.imageEventBox, False, False)
        self.vboxAvatarFrame.pack_start(self.filler2, True, True)
        self.container = gtk.VBox()
        if localavatar:
            self.container.pack_start(self.vboxButtonFrame, False, False)
            self.container.pack_start(self.vboxAvatarFrame, False, False)
        else:
            self.container.pack_start(self.vboxAvatarFrame, False, False)
            self.container.pack_start(self.vboxButtonFrame, False, False)
        self.pack_start(self.container, False, False)
        self.update()
    def update(self):
        if not self.config.user[self.key]:
            self.imageEventBox.hide()
            self.hidebuttonArrow.set(gtk.ARROW_LEFT, gtk.SHADOW_NONE)
        else:
            self.imageEventBox.show()
            self.hidebuttonArrow.set(gtk.ARROW_RIGHT, gtk.SHADOW_NONE)
    def on_display_picture_changed(self, *args):
        if self.mail:
            contact = self.controller.getContact(self.mail)
            if contact:
                self.image.set_from_pixbuf(
                    self.controller.theme.getUserDisplayPicture(contact))
        else:
            self.image.set_from_pixbuf(self.controller.avatar.getImage())
    def hideShowButton(self, *args):
        if self.config.user['showAvatars']:
            self.config.user[self.key] = not self.config.user[self.key]
        self.update()
class InputWidget(gtk.HBox):
    '''This class represents the input widgets (text entry) and avatar (if enabled)'''
    TARGET_TYPE_URI_LIST = 80
    DND_LIST = [('text/uri-list', 0, TARGET_TYPE_URI_LIST)]
    def __init__(self, parentUI, controller, setFocus=True):
        gtk.HBox.__init__(self)
        self.set_border_width(2)
        self.parentUI = parentUI
        self.controller = controller
        self.config = controller.config
        self.lastKeyPressed = 0
        self.history = []
        self.historyIndex = -1
        self.localAvatar = AvatarHBox(self, controller, localavatar=True)
        self.toolbar = ToolbarWidget(self.parentUI, controller)
        self.toolbar.set_style(gtk.TOOLBAR_ICONS)
        self.toolbar.set_small(self.config.user['smallIcons'])
        self.input = Widgets.inputBox()
        self.input.set_left_margin(6)
        self.input.set_right_margin(6)
        self.input.set_wrap_mode(gtk.WRAP_WORD_CHAR)
        self.sendbutton = gtk.Button(_('Send'))
        self.sendbutton.show()
        self.sendbuttonwin = gtk.EventBox()
        self.sendbuttonwin.add(self.sendbutton)
        self.input.add_child_in_window(
            self.sendbuttonwin, gtk.TEXT_WINDOW_TEXT, 0, 0)
        self.sendbutton.connect('clicked', self.message_send)
        self.sendbuttonwin.connect('realize', self.change_cursor)
        self.inputBuffer = self.input.get_buffer()
        self.scrollInput = gtk.ScrolledWindow()
        self.scrollInput.set_policy(gtk.POLICY_AUTOMATIC,
                                    gtk.POLICY_AUTOMATIC)
        self.scrollInput.add(self.input)
        frameInput = gtk.Frame()
        frameInput.set_shadow_type(gtk.SHADOW_IN)
        avatar_frame = gtk.AspectFrame(xalign=0.0, yalign=1.0)
        avatar_frame.add(self.localAvatar)
        avatar_frame.set_shadow_type(gtk.SHADOW_NONE)
        self.vbox = gtk.VBox()
        self.vbox.pack_start(self.toolbar, False)
        self.vbox.pack_start(self.scrollInput, True, True)
        frameInput.add(self.vbox)
        self.pack_start(frameInput, True, True)
        self.pack_start(avatar_frame, False, False)
        self.inputBuffer.connect('changed', self.on_changed_event)
        self.input.connect('key-press-event', self.on_key_press_event)
        self.input.connect('drag-data-received', self.on_drag_data_received)
        self.input.drag_dest_set(gtk.DEST_DEFAULT_MOTION | \
            gtk.DEST_DEFAULT_HIGHLIGHT | gtk.DEST_DEFAULT_DROP, \
            InputWidget.DND_LIST, gtk.gdk.ACTION_COPY)
        self.last_dropped_file = ''
        self.input.connect_after('message-send', self.message_send)
        self.input.connect_after('escape-pressed',
                                    self.on_escape_pressed_event)
        self.input.connect_after('map-event', self.on_input_map_event)
        self.input.connect('size-allocate', self.move_sendbtn)
        self.scrollInput.get_vadjustment().connect('value-changed',
            self.move_sendbtn)
        if setFocus:
            self.input.grab_focus()
        self.tag = None
        self.id_timeout = None
        self.parse_off = 0
        self.applyAttrsToInput()
        self.show_all()
    def update(self):
        '''update the inner componnents if needed'''
        if self.config.user['showAvatars']:
            self.localAvatar.show()
            self.localAvatar.update()
        else:
            self.localAvatar.hide()
        self.toolbar.update()
        if not self.config.user['showToolbar']:
            self.toolbar.hide()
        else:
            self.toolbar.show()
        if not self.config.user['showSendButton']:
            self.sendbutton.hide()
        else:
            self.sendbutton.show()
    def transformEmo(self, *args):
        ''' transform smiley shorcuts in pixbuf '''
        if not self.controller or not self.controller.customEmoticons:
            return
        if not self.controller.config.user['parseSmilies']:
            return
        theme = self.controller.theme
        emos = []
        customEmo = self.controller.customEmoticons
        for code in theme.getSmileysList():
            iter_start = self.inputBuffer.get_start_iter()
            iter_start.set_offset(self.parse_off)
            while iter_start.forward_search(code,
                                gtk.TEXT_SEARCH_VISIBLE_ONLY):
                iter_pos, iter_end = iter_start.forward_search(code, \
                    gtk.TEXT_SEARCH_VISIBLE_ONLY)
                pixbuf = theme.getSmiley(code, False)
                pixbuf.shortcut = code
                tag = gtk.TextTag()
                tag.set_property('invisible', True)
                tagtable = self.inputBuffer.get_tag_table()
                tagtable.add(tag)
                self.inputBuffer.apply_tag(tag, iter_pos, iter_end)
                mark1 = self.inputBuffer.create_mark(None, iter_pos)
                mark2 = self.inputBuffer.create_mark(None, iter_end)
                img = gtk.Image()
                img.set_from_pixbuf(pixbuf)
                img.show()
                img.shortcut = code
                emos.append((img, mark1, mark2))
                iter_start = iter_end
        for code, filename in customEmo.list.iteritems():
            iter_start = self.inputBuffer.get_start_iter()
            iter_start.set_offset(self.parse_off)
            while iter_start.forward_search(code,
                                            gtk.TEXT_SEARCH_VISIBLE_ONLY):
                iter_pos, iter_end = iter_start.forward_search(code,
                    gtk.TEXT_SEARCH_VISIBLE_ONLY)
                pixbuf = resizePixbuf(gtk.gdk.pixbuf_new_from_file(filename),
                                      24, 24)
                tag = gtk.TextTag()
                tag.set_property('invisible', True)
                tagtable = self.inputBuffer.get_tag_table()
                tagtable.add(tag)
                self.inputBuffer.apply_tag(tag, iter_pos, iter_end)
                mark1 = self.inputBuffer.create_mark(None, iter_pos)
                mark2 = self.inputBuffer.create_mark(None, iter_end)
                img = gtk.Image()
                img.set_from_pixbuf(pixbuf)
                img.show()
                img.shortcut = code
                emos.append((img, mark1, mark2))
                iter_start = iter_end
        for emo in emos:
            iterStart = self.inputBuffer.get_iter_at_mark(emo[1])
            iterEnd = self.inputBuffer.get_iter_at_mark(emo[2])
            self.inputBuffer.delete(iterStart, iterEnd)
            iterStart = self.inputBuffer.get_iter_at_mark(emo[1])
            anchor = self.inputBuffer.create_child_anchor(iterStart)
            self.input.add_child_at_anchor(emo[0], anchor)
        iter_off = self.inputBuffer.get_end_iter()
        if iter_off.inside_word() or iter_off.ends_word():
            iter_off.backward_word_start()
        self.parse_off = iter_off.get_offset()
    def grabFocus(self):
        self.input.grab_focus()
    def clearMessageInput(self):
        self.setInputText('')
        return True
    def sendMessage(self):
        if self.getInputText(True) != '':
            message = self.getInputText()
            self.parentUI.parentConversation.sendMessage(message)
            self.setInputText('')
            self.history.append(message)
            self.historyIndex = -1
        return True
    def setInputText(self, string, tag=None):
        if tag == None:
            self.inputBuffer.set_text(string)
        else:
            pass
    def getInputText(self, fast=False):
        '''return the text in the input textview'''
        iterStart, iterEnd = self.inputBuffer.get_bounds()
        text = self.inputBuffer.get_slice(iterStart, iterEnd)
        if fast:
            return text
        emos = []
        while iterStart.forward_search('\xef\xbf\xbc', \
            gtk.TEXT_SEARCH_VISIBLE_ONLY):
            iterPos, iterEnd = iterStart.forward_search('\xef\xbf\xbc', \
                gtk.TEXT_SEARCH_VISIBLE_ONLY)
            anchor = iterPos.get_child_anchor()
            if anchor and anchor.get_widgets():
                emos.append(anchor.get_widgets()[0].shortcut)
            elif anchor is None:
                emos.append('')
            iterStart = iterEnd
        for emo in emos:
            text = text.replace('\xef\xbf\xbc', emo, 1)
        return text
    def appendInputText(self, text, tag = None):
        '''append the given text to the inputBuffer, if tag insert
        with the given tag'''
        self.inputBuffer.insert_at_cursor(text)
    def on_changed_event(self, *args):
        '''Method called when text is inserted in the textbuffer (typing)
        First, it checks if the message length is >= 5 or if
        the message is a slash command ("^\/[^\/]" is confusing)
        Then, checks the last key pressed time, and if it's more
        than 5 seconds, send a typing message'''
        if self.inputBuffer.get_start_iter().get_offset() < self.parse_off:
            self.parse_off = 0
        if self.id_timeout:
            gobject.source_remove(self.id_timeout)
        text = self.getInputText(True)
        if len(text) > 5 and not (text[0] == '/' and text[1] != '/'):
            self.id_timeout = gobject.timeout_add(200, self.transformEmo)
        else:
            self.updateInputFormat()
            return
        actualTime = int(time.time())
        if (actualTime - self.lastKeyPressed) > 5 and \
           self.config.user['sendTyping']:
            self.lastKeyPressed = actualTime
            try:
                self.parentUI.parentConversation.sendIsTyping()
            except Exception , e:
                print str(e)
        self.updateInputFormat()
    def applyAttrsToInput(self):
        '''apply the current attributes to the text in input'''
        tag = gtk.TextTag()
        if self.config.user['disableFormat']:
            return tag
        tag.set_property('font', self.config.user['fontFace'])
        tag.set_property('size-points', self.config.user['fontSize'])
        tag.set_property("foreground", self.config.user['fontColor'])
        if self.config.user['fontBold']:
            tag.set_property("weight", pango.WEIGHT_BOLD)
        if self.config.user['fontItalic']:
            tag.set_property("style", pango.STYLE_ITALIC)
        if self.config.user['fontUnderline']:
            tag.set_property("underline-set", True)
            tag.set_property("underline" , pango.UNDERLINE_SINGLE)
        else:
            tag.set_property("underline-set", True)
            tag.set_property("underline", pango.UNDERLINE_NONE)
        self.tag = tag
        self.tag.set_property("strikethrough", self.config.user['fontStrike'])
        self.inputBuffer.get_tag_table().add(self.tag)
        self.tag.set_property('left-margin', 6)
        self.tag.set_priority(self.inputBuffer.get_tag_table().get_size() - 1)
        self.updateInputFormat()
        self.controller.emit('input-format-changed', self.input)
    def updateInputFormat(self):
        if self.tag:
            self.inputBuffer.apply_tag(self.tag,
                                       self.inputBuffer.get_start_iter(),
                                       self.inputBuffer.get_end_iter())
    def message_send(self, widget):
        if self.id_timeout:
            gobject.source_remove(self.id_timeout)
            self.id_timeout = None
        return self.sendMessage()
    def on_input_map_event(self, widget, event):
        if self.parentUI.parentConversation.isCurrent:
            self.input.grab_focus()
    def on_escape_pressed_event(self, widget):
        if not self.parentUI.closed and not self.config.user['disableEsc']:
            self.parentUI.parentConversation.close()
    def on_key_press_event(self , widget, event):
        if event.keyval == gtk.keysyms.space:
            if self.id_timeout:
                gobject.source_remove(self.id_timeout)
                self.id_timeout = None
            if not self.getInputText(True).startswith('/'):
                self.transformEmo()
        elif event.keyval in (gtk.keysyms.Up, gtk.keysyms.Down) and \
            (event.state & gtk.gdk.CONTROL_MASK):
            up = event.keyval == gtk.keysyms.Up
            if up and self.historyIndex == -1 and len(self.history) > 0:
                self.historyIndex = len(self.history) - 1
                if self.getInputText(True) != '':
                    self.history.append(self.getInputText())
            elif self.historyIndex > 0 and \
              self.historyIndex < len(self.history):
                if up: self.historyIndex -= 1
                else: self.historyIndex += 1
            if self.historyIndex > -1 and self.historyIndex < len(self.history):
                self.setInputText(self.history[self.historyIndex])
            elif self.historyIndex != -1:
                self.setInputText('')
    def on_drag_data_received(self, widget, context, x, y, selection,
                              target_type, timestamp):
        '''Callback to on_drag_data_received'''
        print "Dropped", repr(selection.data)
        if target_type == InputWidget.TARGET_TYPE_URI_LIST and \
           selection.data != self.last_dropped_file:
            uri = selection.data.strip()
            uri_splitted = uri.split()
            for uri in uri_splitted:
                path = self.get_file_path_from_dnd_dropped_uri(uri)
                if os.path.isfile(path):
                    self.parentUI.parentConversation.sendFile(path)
            self.last_dropped_file = str(selection.data)
    def get_file_path_from_dnd_dropped_uri(self, uri):
        '''Parses an URI received from dnd and return the real path'''
        path = urllib.url2pathname(uri) # escape special chars
        path = path.strip('\r\n\x00') # remove \r\n and NULL
        if path.startswith('file:\\\\\\'): # windows
            path = path[8:] # 8 is len('file:///')
        elif path.startswith('file://'): # nautilus, rox
            path = path[7:] # 7 is len('file://')
        elif path.startswith('file:'): # xffm
            path = path[5:] # 5 is len('file:')
        return path
    def move_sendbtn(self, *args):
        '''update sendbutton position'''
        allocation = self.input.get_allocation()
        x = allocation.x
        y = allocation.y
        w = allocation.width
        h = allocation.height
        sendwin_alloc = self.sendbutton.get_allocation()
        xswin = sendwin_alloc.x
        yswin = sendwin_alloc.y
        wswin = sendwin_alloc.width
        hswin = sendwin_alloc.height
        space = 2
        self.input.set_right_margin(wswin + space)
        self.input.move_child(self.sendbuttonwin, w - wswin - space, h / 2 -
                                                                hswin / 2)
        self.input.queue_draw()
    def change_cursor(self, *args):
        self.sendbuttonwin.window.set_cursor(gtk.gdk.Cursor(gtk.gdk.LEFT_PTR))
class ToolbarWidget(gtk.Toolbar):
    '''This represents a toolbar that contains text formatation,
    smilies button, do nudge button, and so on...'''
    def __init__(self, parentUI, controller):
        gtk.Toolbar.__init__(self)
        self.parentUI = parentUI
        self.controller = controller
        self.config = controller.config
        self.smilieWindow = SmilieWindow.SmilieWindow(self.controller, \
                                                       self.smilieSelected, \
                                                       None)
        self.FontStyleWindow = FontStyleWindow.FontStyleWindow( \
                                                        self.controller, \
                                                       self.parentUI)
        self.fontFace = gtk.ToolButton()
        self.fontFace.set_stock_id(gtk.STOCK_SELECT_FONT)
        self.fontFace.connect('clicked', self.on_font_face_clicked)
        self.fontColor = gtk.ToolButton()
        self.fontColor.set_stock_id(gtk.STOCK_SELECT_COLOR)
        self.fontColor.connect('clicked', self.on_font_color_clicked)
        self.fontStyleButton = gtk.ToolButton()
        self.fontStyleButton.set_label(_('Fontstyle'))
        self.fontStyleButton.set_stock_id(gtk.STOCK_BOLD)
        self.fontStyleButton.connect('clicked', self.showFontStyleWindow)
        self.smilieButton = gtk.ToolButton()
        self.smilieButton.set_label(_('Smilie'))
        self.smilieButton.connect('clicked', self.showSmilieWindow)
        self.nudgeButton = gtk.ToolButton()
        self.nudgeButton.set_label(_('Nudge'))
        self.nudgeButton.connect('clicked', self.doNudge)
        self.inviteButton = gtk.ToolButton()
        self.inviteButton.set_label(_('Invite'))
        self.inviteButton.set_stock_id(gtk.STOCK_ADD)
        self.inviteButton.connect('clicked', self.showInviteDialog)
        imgclear = gtk.Image()
        imgclear.set_from_stock(gtk.STOCK_CLEAR, gtk.ICON_SIZE_LARGE_TOOLBAR)
        self.clearButton = gtk.ToolButton(imgclear, _('Clear Conversation'))
        self.clearButton.connect('clicked', self.clearOutputText)
        self.sendfileButton = gtk.ToolButton()
        self.sendfileButton.set_label(_('Send File'))
        self.sendfileButton.set_stock_id(gtk.STOCK_GOTO_TOP)
        self.sendfileButton.connect('clicked', self.sendFileClicked)
        self.insert(self.fontFace, -1)
        self.insert(self.fontColor, -1)
        self.insert(self.fontStyleButton, -1)
        self.insert(gtk.SeparatorToolItem(), -1)
        self.insert(self.smilieButton, -1)
        self.insert(self.nudgeButton, -1)
        self.insert(gtk.SeparatorToolItem(), -1)
        self.insert(self.inviteButton, -1)
        self.insert(self.sendfileButton, -1)
        self.insert(gtk.SeparatorToolItem(), -1)
        self.insert(self.clearButton, -1)
        self.tooltips = gtk.Tooltips()
        self.tooltips.enable()
        self.fontFace.set_tooltip(self.tooltips, _('Font selection'))
        self.fontColor.set_tooltip(self.tooltips, _('Font color selection'))
        self.fontStyleButton.set_tooltip(self.tooltips, _('Font styles'))
        self.smilieButton.set_tooltip(self.tooltips, _('Insert a smilie'))
        self.nudgeButton.set_tooltip(self.tooltips, _('Send nudge'))
        self.inviteButton.set_tooltip(self.tooltips, \
            _('Invite a friend to the conversation'))
        self.clearButton.set_tooltip(self.tooltips, _('Clear conversation'))
        self.sendfileButton.set_tooltip(self.tooltips, _('Send a file'))
        self.show_all()
    def setFontBold(self, value):
        self.FontStyleWindow.buttonBold.set_active(value)
    def setFontUnderline(self, value):
        self.FontStyleWindow.buttonUnderline.set_active(value)
    def setFontStrike(self, value):
        self.FontStyleWindow.buttonStrike.set_active(value)
    def setFontItalic(self, value):
        self.FontStyleWindow.buttonItalic.set_active(value)
    def on_font_face_clicked(self, *args):
        '''Called when user clicks on the font selection button
        in the toolbar'''
        self.parentUI.parentConversation.parentConversationWindow.changeFont()
    def on_font_color_clicked(self, *args):
        ''' Called when user clicks on the color selection button in the toolbar'''
        self.parentUI.parentConversation.parentConversationWindow.changeColor()
    def set_small(self, value):
        '''sets the icons size to small if value is True'''
        if value:
            size = gtk.ICON_SIZE_MENU
        else:
            size = gtk.ICON_SIZE_LARGE_TOOLBAR
        imgSmilie = gtk.Image()
        imgNudge = gtk.Image()
        nudge = self.controller.theme.getImage('nudge')
        grin = self.controller.theme.getSmiley(':D')
        if isinstance(grin, gtk.gdk.PixbufAnimation):
            grin = grin.get_static_image()
        grin = resizePixbuf(grin, *gtk.icon_size_lookup(size))
        nudge = resizePixbuf(nudge, *gtk.icon_size_lookup(size))
        imgSmilie.set_from_pixbuf(grin)
        imgNudge.set_from_pixbuf(nudge)
        self.smilieButton.set_icon_widget(imgSmilie)
        self.nudgeButton.set_icon_widget(imgNudge)
        settings = self.get_settings()
        settings.set_long_property('gtk-toolbar-icon-size', size, \
            'emesene:ConversationUI')
    def smilieSelected(self, smilie=None):
        '''this method is called when the user click a smilie in the
        smiliewindow'''
        if smilie:
            self.parentUI.input.appendInputText(smilie)
        self.parentUI.input.grabFocus()
    def showSmilieWindow(self, *args):
        '''this method is called when the user click the smilie button'''
        self.smilieWindow.show()
    def showFontStyleWindow(self, *args):
        '''this method calls the font styles window'''
        self.FontStyleWindow.show()
    def showInviteDialog(self, *args):
        '''this method is called when the user click the invite button'''
        if not self.controller or not self.controller.msn:
            return
        conversation = self.parentUI.parentConversation
        window = self.parentUI.parentConversation.parentConversationWindow
        window.show_invite_dialog(conversation)
    def clearOutputText(self, *args):
        self.parentUI.parentConversation.textBuffer.set_text('')
    def doNudge(self, *args):
        '''this method is called when the user click the nudge button'''
        self.parentUI.parentConversation.doNudge()
    def sendFileClicked(self, *args):
        if not self.controller or not self.controller.msn:
            return
        win = self.parentUI.parentConversation.parentConversationWindow
        if win is not None:
            win.send_file_dialog()
    def update(self, *args):
        '''this method disables some buttons on switchboard error'''
        conversation = self.parentUI.parentConversation
        if conversation.switchboard.status == 'error' \
         or not self.controller or not self.controller.msn:
            self.nudgeButton.set_sensitive(False)
            self.inviteButton.set_sensitive(False)
            self.sendfileButton.set_sensitive(False)
            self.smilieButton.set_sensitive(False)
        else:
            self.nudgeButton.set_sensitive(True)
            self.inviteButton.set_sensitive(True)
            self.sendfileButton.set_sensitive(True)
            self.smilieButton.set_sensitive(True)
class InviteWindow(gtk.Dialog):
    '''This class represent a list where the user can pick wich users
    he want to invite to a conversation'''
    def __init__(self, controller, father, onlineUsers,
                    usersInConversation, theme, callback):
        '''Constructor callback is called when a user is selected with
        double click'''
        gtk.Dialog.__init__(self, _('Invite'), father,
                     gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
                     (gtk.STOCK_CANCEL, gtk.RESPONSE_REJECT, \
                     gtk.STOCK_ADD, gtk.RESPONSE_ACCEPT))
        self.set_default_size(400, 300)
        self.callback = callback
        users = onlineUsers
        self.controller = controller
        for i in usersInConversation.keys():
            if users.has_key(i):
                del users[i]
        self.userList = UserList.UserList(self.controller, theme,
            self.controller.config, False)
        self.selection = self.userList.get_selection()
        if gtk.gtk_version >= (2, 10, 0):
            self.userList.set_rubber_banding(True)
            self.userList.set_property("rubber-banding", True)
            self.selection.set_mode(gtk.SELECTION_MULTIPLE)
        group = emesenelib.ContactData.Group(_('Users'))
        for contact in controller.msn.contactManager.contacts.values():
            if contact and contact.status != 'FLN' and \
               contact.email not in usersInConversation:
                group.setUser(contact.email, contact)
        self.userList.fill({group.name : group})
        self.scroll = gtk.ScrolledWindow()
        self.scroll.set_policy(gtk.POLICY_AUTOMATIC , gtk.POLICY_AUTOMATIC)
        self.scroll.add_with_viewport(self.userList)
        self.connect('response', self.invite)
        self.connect('delete-event', self.close)
        tipText = _("For multiple select hold CTRL key and click")
        self.tipLabel = gtk.Label()
        self.tipLabel.set_markup("<small><i>" + tipText + "</i></small>")
        self.vbox.set_spacing(3)
        self.vbox.pack_start(self.scroll)
        self.vbox.pack_start(self.tipLabel, False, False)
        self.vbox.show_all()
    def invite(self, dialog, response_id):
        '''get the user in the userlist and call the callback'''
        try:
            if response_id == gtk.RESPONSE_ACCEPT:
                model, iter = self.selection.get_selected_rows()
                for i in range(len(iter)):
                    print model[iter[i][0]][2]
                    self.callback(model[iter[i][0]][2])
                self.close()
                return True
            else:
                self.close()
                return False
        except Exception, e:
            print e
    def userListClicked(self, userList, t):
        if t and t[0] == 'user':
            self.callback(t[1].email)
            self.close()
    def close(self, *args):
        '''close the window'''
        self.hide()
