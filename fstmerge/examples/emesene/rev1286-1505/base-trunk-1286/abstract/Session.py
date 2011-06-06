'''a module that define a class to contain all the information regarding a 
session
'''
class Session(Object.Object):
    '''a class that contain information and methods to handle a session'''
    def __init__(self, account, password, host, port):
        '''class constructor'''
        self.account = account
        self.password = password
        self.host = host
        self.port = port
        self.contacts = None
        self.groups = None
        self.signal_add('login-successful', 0)
        self.signal_add('login-error', 1)
        self.signal_add('connection-error', 1)
    def login(self, account, password, stat):
        '''do login and set status'''
        raise NotImplementedError("This method isn't not implemented")
    def cancel_login(self):
        '''cancel the login process if posible on the protocol implementation
        '''
        raise NotImplementedError("This method isn't not implemented")
    def logout(self):
        '''do logout'''
        raise NotImplementedError("This method isn't not implemented")
