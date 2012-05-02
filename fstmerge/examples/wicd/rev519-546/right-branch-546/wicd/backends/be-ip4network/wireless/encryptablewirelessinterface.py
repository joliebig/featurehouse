from profiledwirelessinterface import ProfiledWirelessInterface
from encryptionmanager import EncryptionManager
class EncryptableWirelessInterface(ProfiledWirelessInterface):
    ''' Adds wpa_supplicant support to the wireless interface.'''
    def __init__(self, interface_name):
        ProfiledWirelessInterface.__init__(self, interface_name)
        self.encryption_manager = EncryptionManager(self.interface_name)
    def set_up_encryption(self, network):
        if 'encryption_type' in network.profile and \
           network.profile['encryption_type']:
            self.encryption_manager.start(network)
