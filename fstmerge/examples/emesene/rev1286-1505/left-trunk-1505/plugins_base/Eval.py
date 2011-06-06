"""
Eval plugin v0.4
THIS IS A DEVELOPERS TOOL, USE AT YOUR OWN RISK!
Simple usage:
 /eval out("somestring")
 /eval out(dir())
 /eval out(slashAction.conversation.switchboard)
Remote usage:
 From the other side..:
   !eval out("somestring")
   !eval out(dir())
   !eval out(conversation.switchboard)
"""
VERSION = '0.4.2'
import sys
import time
import gobject
import traceback
import Plugin
class MainClass(Plugin.Plugin):
    '''Main plugin class'''
    description = _('Evaluate python commands - USE AT YOUR OWN RISK')
    authors = {'Dx' : 'dx@dxzone.com.ar'}
    website = 'http://www.dxzone.com.ar'
    displayName = 'Eval'
    name = 'Eval'
    def __init__(self, controller, msn):
        '''Constructor'''
        Plugin.Plugin.__init__(self, controller, msn)
        self.description = _('Evaluate python commands - USE AT YOUR OWN RISK')
        self.authors = {'Dx' : 'dx@dxzone.com.ar'}
        self.website = 'http://www.dxzone.com.ar'
        self.displayName = 'Eval'
        self.name = 'Eval'
        self.catchOutput = False
        self.controller = controller
        self.Slash = controller.Slash
        self.config = controller.config
        self.config.readPluginConfig(self.name)
        users = self.config.getPluginValue(self.name, 'users', '')
        self.allowed = users.split()
    def start(self):
        '''start the plugin'''
        self.Slash.register('eval', self.slashCommand, _('Run python commands'))
        conv_manager = self.controller.conversationManager
        self.receiveId = conv_manager.connect('receive-message', self.receive)
        self.enabled = True
    def slashCommand(self, slashAction):
        _ = None
        conversation = slashAction.conversation
        controller = self.controller
        msn = controller.msn
        params = str(slashAction.getParams())
        def out(text):
            slashAction.outputText(str(text))
        def send(text, includeCommand=False):
            text = str(text)
            if includeCommand:
                slashAction.outputText(' /eval ' + params + '\n' + \
                    str(text), True)
            else:
                slashAction.outputText(str(text), True)
        if self.catchOutput:
            oldstdout = sys.stdout
        try:
            if self.catchOutput:
                sys.stdout = SlashOut(slashAction)
            eval(compile(params, "<eval>", "exec"))
        except:
            exception = sys.exc_info()
            slashAction.outputText(traceback.format_exception(*exception)[-1])
            traceback.print_exception(*exception)
        if self.catchOutput:
            sys.stdout = oldstdout
    def receive(self, cm, conversation, mail, nick, message, format, charset):
    	'''Eval commands if mail is in allowed list'''
        if mail not in self.allowed or not message.startswith('!eval'):
            return
        cm.emit_stop_by_name('receive-message')
        message = message.replace('\r\n', '\n')
        _ = None
        controller = self.controller
        msn = controller.msn
        params = str(message.split('!eval ')[1] )
        def out(text):
            conversation.sendMessage(str(text))
        if self.catchOutput:
            oldstdout = sys.stdout
        try:
            if self.catchOutput:
                sys.stdout = RemoteOut(conversation)
            eval(compile(params, "<eval>", "exec"))
        except:
            exception = sys.exc_info()
            conversation.sendMessage(traceback.format_exception(*exception)[-1])
            traceback.print_exception(*exception)
        if self.catchOutput:
            sys.stdout = oldstdout
    def stop(self):
        '''stop the plugin'''
        self.Slash.unregister('eval')
        self.enabled = False
    def check(self):
        return (True, 'Ok')
    def configure(self):
    	'''Configuration Dialog'''
        l=[]
        l.append(Plugin.Option('users', str, _('Alowed users:'), '', 
                 self.config.getPluginValue( self.name, 'users', '' )))
        response = Plugin.ConfigWindow(_('Remote Configuration'), l).run()
        if response != None:
            self.users = str(response['users'].value)
            self.allowed = self.users.split()
            self.config.setPluginValue(self.name,'users', self.users)
        return True
class FakeOut:
    def __init__(self):
        self.encoding = self.mode = self.name = self.newlines = None
        self.closed = False
        self.softspace = 0
    def close(self):
        pass
    def fileno(self):
        pass
    def flush(self):
        pass
    def isatty(self):
        pass
    def read(self):
        pass
    def tell(self):
        pass
    def write(self):
        pass
    def writelines(self):
        pass
class SlashOut(FakeOut):
    def __init__(self, slashAction):
        FakeOut.__init__(self)
        self.slashAction = slashAction
    def write(self,text):
        self.slashAction.outputText(text)
class RemoteOut(FakeOut):
    def __init__(self, slashAction):
        FakeOut.__init__(self)
        self.conversation = conversation
    def write(self,text):
        self.conversation.sendMessage(text)
