import gtk
import pango
import gobject
import time
import os
import subprocess
from urllib import quote
from warnings import warn
import emesenelib
import dialog
import abstract.stock as stock
import ConversationUI
import FileTransfer
import Webcam
MAX_MESSAGE_LENGTH = 1100
class Conversation(gobject.GObject):
    '''This class is an abstraction of a conversation, it is used to separate
    the data from the GUI in a conversation to let us have single and tabbed
    windows with the same codebase (also MVC is good :P)'''
    def __init__(self, controller, switchboard):
        '''Constructor'''
        gobject.GObject.__init__(self)
        self.callbackIdList = []
        self.P2PSignals = {}
        self.ui = None
        self.switchboard = None
        self.setSwitchboard(switchboard)
        msn = switchboard.msn
        self.controller = controller
        self.parser = controller.unifiedParser
        self.config = controller.config
        self.title = ""
        self.lastMessageMail = '' # the mail of the user who sent the last message
        self.isCurrent = False # if True this is the tab that has the focus
        self.closed = False
        self.textBuffer = gtk.TextBuffer()
        self.autoreplySent = False # if true we allready sent the autoreply message
        self.inputText = ''
        self.theme = controller.theme
        self.parentConversationWindow = None
        msn.connect('user-attr-changed', self.onUserAttrChanged)
        msn.connect('custom-emoticon-transfered', self.onCustomEmoticonTransfered)
        controller.connect('color-changed', self.onColorChanged)
        controller.connect('font-changed', self.onFontChanged)
        self.ui = ConversationUI.ConversationUI(self.controller, self)
        self.customEmoticons = CustomEmoticons()
        self.parser = self.controller.unifiedParser
        self.sendOffline = False
        self.lastSpeaker = ''
        self.user = switchboard.user
        self.transfers = []
    def onFtInvite(self, p2p, session, context, sender):
        ft = FileTransfer.FileTransfer(self.controller, p2p, self, session,
            context, sender)
        self.transfers.append(ft)
        self.ui.transfers.add(ft)
        self.ui.transfers.show_all()
        if self.config.user['autoAcceptTransfer']:
            ft.accept()
    def view_webcam(self):
        p2p = self.controller.msn.p2p[self.switchboard.firstUser]
        handler = emesenelib.p2p.transfers.WebcamHandler(we_invited=True, p2p=p2p)
        self.webcam_other = Webcam.Webcam(p2p, self, handler.session_id, handler)
    def on_webcam_invite(self, p2p, session, sender):
        self.webcam_other = Webcam.Webcam(p2p, self, session, sender)
        dialog.yes_no(_("Accept webcam from %s?") % sender, self.accept_webcam_other)
    def accept_webcam_other(self, response):
        if response == stock.YES:
            self.webcam_other.accept()
    def sendFile(self, path):
        '''This sends a file'''
        p2p = self.controller.msn.p2p[self.switchboard.firstUser]
        sender = emesenelib.p2p.transfers.FTSender(p2p, path)
        self.onFtInvite(p2p, sender.session_id, sender.context, 'Me')
    def getSwitchboard(self):
        return self.switchboard
    def close(self):
        '''close the tab'''
        self.parentConversationWindow.closeTab(self)
        self.setClosed(True)
    def onColorChanged(self, controller, colorStr):
        self.config.user['fontColor'] = colorStr
        self.ui.input.applyAttrsToInput()
    def onFontChanged(self, controller, font, bold, italic, size):
        self.setFont(font, italic, bold, size)
        self.ui.input.toolbar.setFontBold(bold)
        self.ui.input.toolbar.setFontItalic(italic)
        self.ui.input.applyAttrsToInput()
    def onUserAttrChanged(self, msnp, contact):
        if contact.email in self.getMembers() and \
           self.isCurrent and self.parentConversationWindow:
            win = self.parentConversationWindow
            win.update_title()
            if not self.config.user['avatarsInTaskbar']:
                win.set_icon(self.getWindowIcon())
    def getStatusIcon(self):
        '''returns the status icon for this conversation'''
        members = self.getMembers()
        theme = self.controller.theme
        user = None
        if len(members) > 1:
            return theme.getImage('groupChat')
        if len(members) == 0:
            return theme.getImage('userPanel')
        user = self.controller.getContact(members[0])
        if not user:
            return theme.getImage('userPanel')
        return theme.statusToPixbuf(user.status)
    def getWindowIcon(self):
        '''returns the window icon for this conversation'''
        members = self.getMembers()
        user = None
        if len(members) == 1:
            user = self.controller.getContact(members[0])
        if user and self.config.user['avatarsInTaskbar'] and \
                self.controller.theme.hasUserDisplayPicture(user):
            return self.controller.theme.getUserDisplayPicture(user, 64, 64)
        return self.getStatusIcon()
    def getFontColor(self):
        '''return the user color'''
        return self.config.user['fontColor']
    def setFont(self, font, italic=False, bold=False, size=10):
        '''set the font of the user text'''
        self.config.user['fontFace'] = font
        self.config.user['fontItalic'] = italic
        self.config.user['fontBold'] = bold
        self.config.user['fontSize'] = size
    def getTitle(self):
        '''return a title according to the users in the conversation'''
        if not self.controller:
            return self.title
        members = self.getMembers()
        if len(members) > 1:
            self.title = _('Group chat')
        elif len(members) == 1:
            if self.config.user['useAliasIfAvailable']:
                title = self.controller.contacts.get_alias(members[0])
            if not title:
                title = self.controller.contacts.get_nick(members[0])
            if title:
                self.title = title
        return self.title
    def getUser(self):
        '''return the (local) user mail'''
        return self.switchboard.user
    def getRTL(self, message):
        '''check whether it's an right-to-left string'''
        if pango.find_base_dir(message, -1) == pango.DIRECTION_RTL:
            return '1'
        else:
            return '0'
    def getStyle(self, message=''):
        '''return the style string to use in the sendMessage method'''
        effectValue = ''
        if self.config.user['fontBold']:
            effectValue += 'B'
        if self.config.user['fontItalic']:
            effectValue += 'I'
        if self.config.user['fontUnderline']:
            effectValue += 'U'
        if self.config.user['fontStrike']:
            effectValue += 'S'
        color = self.config.user['fontColor'].replace('#', '')
        color = color[ 4:6 ] + color[ 2:4 ] + color[ :2 ]
        face = self.config.user['fontFace'].replace(' ', '%20')
        return "X-MMS-IM-Format: FN=" + face + \
            "; EF=" + effectValue + "; CO=" + color + \
            "; PF=0;RL=" + self.getRTL(message)
    def getOnlineUsers(self):
        '''This method returns a list ol mails of the contacts who are not offline'''
        return self.switchboard.getOnlineUsers()
    def invite(self, mail):
        '''invite a user to the conversation'''
        self.switchboard.invite(mail)
    def getMembers(self):
        '''return a list of the members in the conversation'''
        members = self.switchboard.getMembers()
        if len(members) != 0: return members
        members = self.switchboard.getInvitedMembers()
        if len(members) != 0: return members
        return [self.switchboard.firstUser]
    def setWindow(self, window):
        '''set the window that hold this conversation'''
        self.parentConversationWindow = window
    def setIsCurrent(self, current):
        '''set the isCurrent attribute, if true, this conversation
        is the tab that is shown'''
        self.isCurrent = current
    def receiveNudge(self, switchboard, mail):
        '''This method is called when a nudge is received in the
        switchboard'''
        nick = self.controller.msn.getUserDisplayName(mail)
        self.appendOutputText(None, _("%s just sent you a nudge!")% \
          self.parser.getParser(nick).get(False), 'information')
        self.doMessageWaiting(mail)
    def receive_wink(self, switchboard, mail, msnobj):
        '''this method is called when a wink is received'''
        nick = self.controller.msn.getUserDisplayName(mail)
        name = msnobj.friendly.replace('\x00', '')
        self.appendOutputText(None, _("%s sent you a wink: %s")% \
          (self.parser.getParser(nick).get(False), name),  'information')
        self.doMessageWaiting(mail)
    def receiveOIM(self, nick, message, date):
        '''This method is called when a offline message is received'''
        self.appendOutputText(nick, message, 'offline_incoming', \
            timestamp=time.mktime(date))
        self.doMessageWaiting('')
    def receiveError(self, msnp, to, message, error):
        '''This method is called when a error message is received'''
        self.appendOutputText('Error', "Can\'t send message (%s)\n%s" % \
            (error, message), 'error')
        self.doMessageWaiting('')
    def onReceiveMessage(self, switchboard, mail, nick, message,
                         format, charset):
        '''This method is called when a message is received in the switchboard'''
        self.controller.conversationManager.emit('receive-message', self, \
            mail, nick, message, format, charset)
    def onInkMessage(self, switchboard, mail, filename):
        '''This method is called when an ink message is received
        in the switchboard'''
        self.doMessageWaiting(mail)
        self.appendOutputText(mail, quote(filename), 'ink_incoming')
    def doMessageWaiting(self, mail):
        win = self.parentConversationWindow
        if win and (not win.has_toplevel_focus() or not self.isCurrent):
            self.ui.setMessageWaiting(mail)
            self.parentConversationWindow.setUrgency()
            self.parentConversationWindow.show()
        else:
            self.ui.setDefault(mail)
    def do_receive_message(self, mail, nick, message, format, charset):
        '''This method is called when a message is received
        in the switchboard'''
        self.doMessageWaiting(mail)
        if self.config.user['autoReply'] and not self.autoreplySent:
            msg = self.config.user['autoReplyMessage']
            self.switchboard.sendMessage('AutoMessage: ' + msg)
            self.appendOutputText(None, 'AutoMessage: %s\n' % msg, \
                'information')
            self.autoreplySent = True
        if message is not None:
            self.appendOutputText(mail, message, 'incoming', \
                self.parseFormat(mail, format))
    def userJoin(self, switchboard, mail):
        '''This method is called when someone joins the conversation'''
        if switchboard.isGroupChat():
            nick = self.controller.msn.getUserDisplayName(mail)
            nick = self.parser.getParser(nick).get(False)
            self.appendOutputText('', \
                _("%s has joined the conversation") % nick, 'information')
        if self.isCurrent:
            self.parentConversationWindow.update_title()
            self.parentConversationWindow.set_icon(self.getWindowIcon())
        if self.ui:
            self.ui.update()
    def userLeave(self, switchboard, mail):
        '''method called when someone leaves the conversation'''
        nick = self.controller.msn.getUserDisplayName(mail)
        nick = self.parser.getParser(nick).get(False)
        self.appendOutputText('', _("%s has left the conversation")%nick, \
            'information')
        if self.isCurrent:
            self.parentConversationWindow.update_title()
            self.parentConversationWindow.set_icon(self.getWindowIcon())
        if self.ui:
            self.ui.update()
    def sbStatusChange(self, switchboard):
        if self.ui:
            self.ui.update()
    def inviteUser(self, mail):
        '''method called when the user selects a friend in the invite dialog'''
        if self.switchboard.status == 'closed':
            self.reconnect()
        self.invite(mail)
        self.ui.messageWaiting[mail] = False
        self.ui.contactTyping[mail] = False
        self.ui.update()
    def doNudge(self):
        '''this method is called when the user clicks the nudge button'''
        if self.switchboard.status == 'closed':
            self.reconnect()
        try:
            self.switchboard.sendNudge()
        except Exception:
            self.reconnect()
            self.switchboard.sendNudge()
        self.appendOutputText(None, _("you have sent a nudge!"), 'information')
    def reconnect(self):
        '''reconnect the switchboard'''
        if not self.controller or not self.controller.msn:
            return
        user = self.getMembers()[0]
        self.setSwitchboard(self.controller.msn.getSwitchboard(user))
        self.autoreplySent = False
    def do_send_message(self, message, retry=0):
        '''Send the message from the UI input. This chooses between OIM and
        switchboard to send the message.'''
        remoteMail = self.switchboard.firstUser
        remoteStatus = self.controller.contacts.get_status(remoteMail)
        def do_send_offline(response, mail='', message=''):
            '''callback for the confirm dialog asking to send offline
            message'''
            if response == stock.YES:
                self.sendOffline = True
                self.switchboard.msn.msnOIM.send(mail, message)
                self.appendOutputText(self.user, message, 'outgoing')
        if self.switchboard.status == 'error' and remoteStatus == 'FLN':
            if self.sendOffline == True:
                do_send_offline(stock.YES, remoteMail, message)
            else:
                dialog.yes_no(
                 _("Are you sure you want to send a offline message to %s") %\
                 remoteMail, do_send_offline, remoteMail, message)
            return
        if self.switchboard.status == 'closed':
            self.reconnect()
        messageChunks = splitMessage(message)
        for chunk in messageChunks:
            try:
                self.switchboard.sendCustomEmoticons(chunk)
                self.switchboard.sendMessage(chunk, self.getStyle(chunk))
                self.appendOutputText(self.user, chunk, 'outgoing')
            except Exception, e:
                raise
                print str(e)
                self.reconnect()
                if retry < 3:
                    self.do_send_message(
                        ''.join(messageChunks[messageChunks.index(chunk):]), \
                        retry + 1)
                else:
                    self.appendOutputText(None, _('Can\'t send message'), \
                        'information')
                return
    def sendMessage(self, message):
        '''send a message to the conversation'''
        self.controller.conversationManager.emit('send-message', self, message)
    def sendIsTyping(self):
        '''an easy method to send the is typing message'''
        if self.switchboard.status == 'closed':
            self.reconnect()
        try:
            self.switchboard.sendIsTyping()
        except Exception:
            self.reconnect()
            self.sendIsTyping()
    def sendAction(self, actionMessage):
        '''send an action message to the conversation'''
        user = self.controller.getContact(self.getMembers()[0])
        if user.getMSNC() >= 6:
            self.switchboard.sendAction(actionMessage)
            self.appendOutputText(None, actionMessage, 'information')
        else:
            self.appendOutputText(None, \
                _('User can\'t receive action messages'), 'information')
    def parseFormat(self, mail, format):
        '''parse the format of a mail and return the style'''
        if self.config.user['useFriendsUnifiedFormat']:
            font = self.config.user['friendsUnifiedFont']
            color = self.config.user['friendsUnifiedColor']
            return 'font-family: ' + emesenelib.common.escape(font) + \
                ';color: ' + emesenelib.common.escape(color) + ';'
        style = ''
        if format.find("FN=") != -1:
            font = format.split('FN=')[1].split(';')[0].replace('%20', ' ')
            style += 'font-family: ' + emesenelib.common.escape(font) + ';'
        if format.find("CO=") != -1:
            color = format.split('CO=')[1].split(';')[0]
            if len(color) == 3:
                color = color[2] + color[1] + color[0]
                style += 'color: #' + emesenelib.common.escape(color) + ';'
            else:
                color = color.zfill(6)
            if len(color) == 6:
                color = color[4:6] + color[2:4] + color[:2]
                style += 'color: #' + emesenelib.common.escape(color) + ';'
        if format.find("EF=") != -1:
            effect = set(format.split('EF=')[1].split(';')[0])
            if "B" in effect: style += 'font-weight: bold;'
            if "I" in effect: style += 'font-style: italic;'
            if "U" in effect: style += 'text-decoration: underline;'
            if "S" in effect: style += 'text-decoration: line-through;'
        return style
    def setSwitchboard(self, switchboard):
        '''set a new switchboard for the conversation.
        useful if the conversation is closed
        and the other user starts a new one'''
        signalDict = {
            'nudge': self.receiveNudge,
            'message': self.onReceiveMessage,
            'ink-message': self.onInkMessage,
            'user-join': self.userJoin,
            'user-leave': self.userLeave,
            'typing': self.receiveTyping,
            'custom-emoticon-received': self.onCustomEmoticonReceived,
            'wink': self.receive_wink,
        }
        if self.switchboard:
            while len(self.callbackIdList) > 0:
                self.switchboard.disconnect(self.callbackIdList.pop())
            if self.switchboard.msn:
                for mail in self.P2PSignals.keys():
                    handlers = self.P2PSignals.pop(mail)
                    for handler in handlers:
                        self.switchboard.msn.p2p[mail].disconnect(handler)
        if switchboard is None:
            return
        self.switchboard = switchboard
        for signalName in signalDict.keys():
            self.callbackIdList.append(self.switchboard.connect(signalName, \
                signalDict[signalName]))
        for mail in switchboard.getMembers():
            ft_invite_handler = switchboard.msn.p2p[mail].connect(\
                'file-transfer-invite', self.onFtInvite)
            webcam_invite_handler = switchboard.msn.p2p[mail].connect(\
                    'webcam-invite', self.on_webcam_invite)
            self.P2PSignals[mail] = [ft_invite_handler, webcam_invite_handler]
        self.autoreplySent = False
    def onCustomEmoticonReceived(self, switchboard, shortcut, msnobj):
        '''call when a smiley is received'''
        self.customEmoticons.setNew(msnobj.creator, shortcut, msnobj.sha1d)
    def onCustomEmoticonTransfered(self, switchboard, to, msnobj, path):
        '''call when a smiley is transfered'''
        if self.ui:
            self.ui.textview.setCustomObject(msnobj.sha1d, path, \
                type='application/x-emesene-emoticon')
    def appendOutputText(self, username, text, type, style = None, timestamp = None):
        '''append the given text to the outputBuffer'''
        if type.startswith('ink_'):
            type = type[4:]
            ink = True
        else:
            ink = False
        if type != 'incoming' and type != 'outgoing':
            self.lastSpeaker = ''
        elif username == self.lastSpeaker:
            type = 'consecutive_' + type
        self.lastSpeaker = username
        if username == self.switchboard.user:
            nick = emesenelib.common.escape(self.controller.msn.nick)
        elif username:
            nick = emesenelib.common.escape( \
                self.controller.msn.getUserDisplayName(username))
        else:
            nick = ''
        if timestamp == None:
            timestamp = time.time()
        displayedText = self.controller.conversationLayoutManager.layout(\
            username, text, style, self, type, timestamp, ink)
        try:
            self.ui.textview.display_html(displayedText.encode('ascii', \
                'xmlcharrefreplace'))
        except Exception, e:
            print 'error trying to display "' + displayedText + '"'
            print e
        self.ui.scrollToBottom()
    def getStatus(self):
        return self.switchboard.status
    def setStatus(self, value):
        self.switchboard.setStatus(value)
    def isClosed(self):
        return self.closed
    def setClosed(self, value):
        self.closed = value
        if value == True:
            self.switchboard.leaveChat()
            self.setSwitchboard(None)
    def receiveTyping(self, switchboard, mail):
        '''This method is called when a is typing message is received in the
        switchboard'''
        if self.ui:
            self.ui.setTyping(mail)
    def getMembersDict(self):
        '''return a dict with email as key and contact instance as value'''
        userDict = {}
        for mail in self.getMembers():
            contact = self.controller.getContact(mail)
            if contact:
                userDict[mail] = contact
        return userDict
gobject.type_register(Conversation)
class CustomEmoticons(object):
    def __init__(self):
        self.emoticons = {}
    def setNew(self, user, shortcut, id):
        if user:
            user = user.lower()
        if user in self.emoticons:
            self.emoticons[user].update({shortcut:id})
        else:
            self.emoticons.update({user:{shortcut:id}})
    def get(self, user=None):
        if user:
            user = user.lower()
        if user in self.emoticons:
            return self.emoticons[user]
        else:
            return []
def splitMessage(message):
    '''Split large messages'''
    messageChunks = []
    messageLen = len(message)
    msgStart = 0
    while msgStart < messageLen:
        chunk = message[msgStart:msgStart+MAX_MESSAGE_LENGTH]
        chunkLen = len(chunk)
        if chunkLen == MAX_MESSAGE_LENGTH:
            chunkEnd = chunk.rfind(' ')
            if chunkEnd!=-1 and chunkEnd>0:
                messageChunks.append(chunk[0:chunkEnd])
                msgStart += chunkEnd+1
            else:
                msgStart += chunkLen
                messageChunks.append(chunk)
        else:
            msgStart += chunkLen
            messageChunks.append(chunk)
    return messageChunks
