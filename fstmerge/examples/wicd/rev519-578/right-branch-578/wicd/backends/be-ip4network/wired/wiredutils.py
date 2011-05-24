import re
import misc
from misc import WicdError
from profilednetutils import ProfiledNetworkInterface, NetworkProfile
class WiredNetwork: pass
class WiredInterface(ProfiledNetworkInterface):
    ''' Represents a hardware wireless interface. '''
    def __init__(self, interface_name):
        ProfiledNetworkInterface.__init__(self, interface_name)
        self.profiles = []
    def load_profiles(self):
        for section in self.config_manager.sections():
            settings = self.config_manager.items(section)
            self.profiles.append(NetworkProfile(dict(settings)))
    def create_profile(self, profile_name):
        new_profile = NetworkProfile()
        new_profile['profile_name'] = profile_name
        self.profiles.append(new_profile)
    def delete_profile(self, profile_name):
        self.profiles.remove(self._get_profile('profile_name', profile_name))
    def list_profiles(self):
        return [ profile['profile_name'] for profile in self.profiles ]
    def save_profiles(self):
        self._save_config(self.profiles, 'profile_name')
    def _get_profile(self, the_property, value):
        for profile in self.profiles:
            if the_property in profile and profile[the_property] == value:
                return profile
        raise WicdError('No network with the specified %s exists.' % the_property)
