class SlashAction(object):
    def __init__(self, conversation, name, params=''):
        self.conversation = conversation
        self.name = name
        self.params = params
    def getConversation(self):
        return self.conversation
    def getParams(self):
        return self.params
    def outputText(self, string, send=False):
        if send:
            self.conversation.sendMessage(string)
        else:
            self.conversation.appendOutputText(None, string, 'error')
class SlashCommands(object):
    def __init__(self, controller):
        self.commands = {}
        self.register('help', self.slashHelp)
        self.controller = controller
        self.sendmessageID = controller.conversationManager.connect(
            'send-message', self.sendMessage)
    def unregister_slash(self):
        self.controller.conversationManager.disconnect(self.sendmessageID)
    def sendMessage(self, obj, conversation, message):
        '''Send Message Interceptor'''
        if len(message) < 3:
            return
        if message[0] == '/':
            obj.emit_stop_by_name('send-message')
            if not message[1] == '/':
                emit(conversation, self.commands, message)
            else:
                conversation.do_send_message(message[1:])
    def register(self, command, callback, help=None):
        if not self.commands.has_key(command):
            self.commands[command] = [callback, help]
        else:
            print 'The command', command,'is registered, try another'
    def unregister(self, command):
        if self.commands.has_key(command):
            self.commands.pop(command)
    def getCommandHelp(self, command):
        if self.commands.has_key(command):
            if self.commands[command][1] != None:
                return self.commands[command][1]
            else:
                return 'The command %s has not help' % command 
        else:
            return 'The command %s doesn\'t exist' % command
    def slashHelp(self, slashAction):
        params = slashAction.getParams()
        if params:
            slashAction.outputText(self.getCommandHelp(params))
        else:
            text = [_('Use "/help <command>" for help on a specific command'),
                    _('The following commands are available:'),
                    ', '.join(self.commands.keys())]
            slashAction.outputText('\n'.join(text))
def emit(conversation, commands, message):
    name = params = ''
    cmd = message[1:].split(" ",1)
    name = cmd[0]
    if len(cmd) > 1:
        params = cmd[1]
    if commands.has_key(name):
        slashAction = SlashAction(conversation, name, params)
        commands[name][0](slashAction)
    else:
        conversation.appendOutputText(None,
            _('The command %s doesn\'t exist') % name, 'error')
