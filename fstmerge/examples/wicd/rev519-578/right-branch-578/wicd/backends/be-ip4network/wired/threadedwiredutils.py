from misc import WicdError
from wiredutils import WiredInterface
from asyncrunner import AsyncManager, AsyncError
import logging
class ThreadedWiredInterface(WiredInterface):
    def __init__(self, interface_name):
        WiredInterface.__init__(self, interface_name)
        self.__async_manager = AsyncManager()
    def connect(self, profile, finished_callback):
        ''' Attempts to connect. Connecting is done asynchronously.'''
        def _do_connect(abort_if_needed, interface, network_profile):
            logging.debug('connecting to network %s',
                          network_profile['profile_name'])
            interface.reset()
            interface.up()
            got_ip = interface.do_ip_address(network_profile)
            logging.debug('%s: interface got IP: %s',
                          interface.interface_name,
                          got_ip)
            return got_ip
        def _finish_up(result):
            finished_callback(result)
        self.__async_manager.run(_do_connect, _finish_up, self,
                                 profile,
                                 name='connect')
    def cancel_connection_attempt(self):
        ''' Cancel the current attempt to connect to the network. '''
        self.dhcp_manager.stop()
        self.__async_manager.stop('connect')
