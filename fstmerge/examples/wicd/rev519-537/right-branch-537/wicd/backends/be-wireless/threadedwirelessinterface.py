from misc import WicdError
from baseinterface import needsidle
from encryptablewirelessinterface import EncryptableWirelessInterface
from asyncrunner import AsyncManager, AsyncError
class ThreadedWirelessInterface(EncryptableWirelessInterface):
    def __init__(self, interface_name):
        EncryptableWirelessInterface.__init__(self, interface_name)
        self.__async_manager = AsyncManager()
    def scan(self, finished_callback):
        ''' Performs a scan. Scanning is done asynchronously. '''
        def _do_scan(abort_if_needed, self):
            return EncryptableWirelessInterface._do_scan(self)
        def finish_up(result):
            print 'scan finished', result
            self.networks = result
            finished_callback()
        self.__async_manager.run(_do_scan, finish_up, self)
    def connect(self, finished_callback):
        ''' Attempts to connect. Connecting is done asynchronously.'''
        def _do_connect(abort_if_needed, interface, network):
            print 'connecting...'
            print interface
            print network
            import time
            while True:
                time.sleep(10)
                print 'in connecting thread...'
                abort_if_needed()
            print 'done connecting'
        def finish_up(result):
            finished_callback()
        self.__async_manager.run(_do_connect, finish_up, self,
                                                     self.current_network,
                                                     name='connect')
    def cancel_connection_attempt(self):
        ''' Cancel the current attempt to connect to the network. '''
        self.__async_manager.stop('connect')
