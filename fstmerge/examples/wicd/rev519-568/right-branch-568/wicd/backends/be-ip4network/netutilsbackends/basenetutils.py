class BaseNetworkInterface(object):
    def __init__(self, interface_name):
        self.interface_name = interface_name
    def connect(self, finished_callback):
        ''' Connects to the network in self.current_network. '''
        raise NotImplementedError('connect is not implemented in this class.')
    def cancel_connection_attempt(self):
        raise NotImplementedError(
            'cancel_connection_attempt is not implemented in this class.'
        )
