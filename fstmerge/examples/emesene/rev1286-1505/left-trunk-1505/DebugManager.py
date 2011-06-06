import inspect
class DebugLevel:
    info = 0
    warning = 10
    error = 20
class DebugManager:
    '''Manages debug informations, events and cleanups'''
    def __init__(self):
        '''constructor'''
        self.messages = [] #list of messages, each one is a dict
        self.message_limit = -1
        self.__event_callbacks = {}
    def set_size_limit(self, limit):
        '''maximum size of the messages list, -1 is infinite'''
        self.message_limit = limit
    def add(self, message):
        '''add the message'''
        if len(self.messages) == self.message_limit:
            self.emit('message-removed', self.messages[0])
            self.messages.pop(0)
        self.messages.append(message)
        self.emit('message-added', message)
    def get_all(self):
        '''get all messages'''
        return self.messages
    def get_n(self, n):
        '''return the nth message'''
        return self.messages[n]
    def emit(self, event_name, *args):
        '''emits the signal named event_name'''
        if event_name not in self.__event_callbacks:
            return
        for callback in self.__event_callbacks[event_name]:
            callback(*args)
    def connect(self, event_name, callback):
        '''connect the event called "event_name" with the callback'''
        if event_name not in self.__event_callbacks:
            self.__event_callbacks[event_name] = []
        self.__event_callbacks[event_name].append(callback)
def print_and_inspect(message):
    print '%(category)s : %(message)s {%(caller)s @ %(filename)s} ' % message
