VERSION = '0.2'
import os
import sys
import time
import socket
import emesenelib.common
import Plugin
class MainClass( Plugin.Plugin ):
    '''Main plugin class'''
    def __init__( self, controller, msn ):
        '''Contructor'''
        Plugin.Plugin.__init__( self, controller, msn )
        self.description = _('Make some useful commands available, try /help')
        self.authors = { 'marianoguerra' : 'luismarianoguerra gmail com' }
        self.website = 'http://emesene.org'
        self.displayName = _('Commands')
        self.name = 'Commands'
        self.controller = controller
        self.Slash = controller.Slash
        self.vars = {}
        self.vars['%me%'] = self._me
        self.vars['%nick%'] = self._nick
        self.vars['%status%'] = self._status
        self.vars['%mail%'] = self._mail
        self.vars['%date%'] = self._date
        self.vars['%time%'] = self._time
        self.vars['%host%'] = self._host
        self.vars['%ip%'] = self._ip
    def _me(self):
        return self.controller.msn.user.split('@')[0]
    def _nick(self):
        return self.controller.msn.nick
    def _status(self):
        return emesenelib.common.reverse_status[self.controller.msn.status]
    def _mail(self):
        return self.controller.msn.user
    def _date(self):
        return time.strftime('%d-%b-%y')
    def _time(self):
        return time.strftime( '%H:%M' )
    def _host(self):
        return socket.gethostname()
    def _ip(self):
        try:
            result = socket.getaddrinfo(self._host(), None, 0, socket.SOCK_STREAM)
            return ''.join([x[4][0] for x in result])
        except socket.gaierror:
            return '0.0.0.0'
    def start( self ):
        '''start the plugin'''
        self.Slash.register('repl', self.repl, _('Replaces variables'))
        self.Slash.register('me', self.slash_me, 
            _('Replaces me with your username'))
        self.Slash.register('nudge', self.slash_nudge,
            _('Sends a nudge'))
        self.Slash.register('invite', self.slash_invite,
            _('Invites a buddy'))
        self.Slash.register('send', self.slash_send,
            _('Send a file'))
        self.Slash.register('add', self.slash_contact_actions,
            _('Add a contact'))
        self.Slash.register('remove', self.slash_contact_actions,
            _('Remove a contact'))
        self.Slash.register('block', self.slash_contact_actions,
            _('Block a contact'))
        self.Slash.register('unblock', self.slash_contact_actions,
            _('Unblock a contact'))
        self.Slash.register('clear', self.slash_clear,
            _('Clear the conversation'))
        self.Slash.register('nick', self.slash_nick_psm,
            _('Set your nick'))
        self.Slash.register('psm', self.slash_nick_psm,
            _('Set your psm'))
        self.enabled = True
    def repl(self, slash_action):
        data = slash_action.getParams()
        for (var,replacement) in self.vars.iteritems():
            data = data.replace(var, replacement())
        slash_action.outputText(data, True)
    def slash_me(self, slash_action):
        data = slash_action.getParams()
        slash_action.outputText(self._nick() + ' ' + data, True)
    def slash_nick_psm(self, slash_action):
        '''Set your nick or psm'''
        data = slash_action.params
        name = slash_action.name
        contact_manager = self.controller.contacts
        if name == 'nick':
            contact_manager.set_nick(data)
        elif name == 'psm':
            contact_manager.set_message(data)
    def slash_nudge(self, slash_action):
        slash_action.conversation.doNudge()
    def slash_invite(self, slash_action):
        slash_action.conversation.parentConversationWindow.show_invite_dialog()
    def slash_contact_actions(self, slash_action):
        contacts = self.controller.contacts
        data = slash_action.params
        name = slash_action.name
        if not data:
            slash_action.outputText(_("Usage /%s contact" % name))
            return
        if name == 'add':
            contacts.add(data)
        elif name == 'block':
            contacts.block(data)
        elif name == 'unblock':
            contacts.unblock(data)
        elif name == 'remove':
            contacts.remove(data)
    def slash_send(self, slash_action):
    	'''Send a file'''
    	conversation = slash_action.conversation
    	file_path = slash_action.params
    	if file_path:
    	    if os.path.exists(file_path):
    	        conversation.sendFile(file_path)
    	    else:
    	    	slash_action.outputText(_("File doesn't exist"))
    	else:
            conversation.parentConversationWindow.send_file_dialog()
    def slash_clear(self, slash_action):
        slash_action.conversation.parentConversationWindow.clearOutputText()
    def stop( self ):    
        '''stop the plugin'''
        self.Slash.unregister('repl')
        self.Slash.unregister('me')
        self.Slash.unregister('nudge')
        self.Slash.unregister('invite')
        self.Slash.unregister('send')
        self.Slash.unregister('add')
        self.Slash.unregister('remove')
        self.Slash.unregister('block')
        self.Slash.unregister('unblock')
        self.Slash.unregister('clear')
        self.Slash.unregister('psm')
        self.enabled = False
    def check( self ):
        return ( True, 'Ok' )
