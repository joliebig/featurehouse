from baseplugin import BasePlugin
from misc import WicdError
import logging
class AutoconnectPlugin(BasePlugin):
    ''' A plugin that will autoconnect to networks. '''
    PRIORITY = 100
    def do_start(self):
        logging.debug( 'autoconnect plugin started...')
    def do_got_link(self, interface):
        self._run_autoconnect('onlink', interface.interface_name)
    def do_autoconnect(self, interface_name=None):
        self._run_autoconnect('secondary', interface_name)
    def _run_autoconnect(self, value, interface_name):
        logging.debug( 'running _run_autoconnect... %s', value)
        if interface_name is None:
            for interface_name in self.daemon.ListInterfaces():
                self._autoconnect_interface(value, interface_name)
        else:
            self._autoconnect_interface(value, interface_name)
    def _autoconnect_interface(self, value, interface_name):
        interface = self.daemon.interface_manager.get(interface_name)
        logging.debug( 'autoconnecting %s', interface.interface_name,)
        if not interface.get_connected_to_something():
            if hasattr(interface, 'do_autoconnect'):
                try:
                    getattr(interface, 'do_autoconnect')(value)                    
                except WicdError, e:
                    logging.debug( 'error autoconnecting interface %s: %s',
                                   interface_name,
                                    e)
        else:
            logging.debug('already connected')
