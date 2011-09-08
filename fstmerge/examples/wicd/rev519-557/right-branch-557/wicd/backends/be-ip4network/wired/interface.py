from baseinterface import BaseInterface, needsidle
from threadedwiredutils import ThreadedWiredInterface
from misc import WicdError
import os
from logfile import log
def needscurrentprofile(method):
    '''
    Can be used as a decorator to raise an 
    error if the interface is not idle.
    '''
    def profilecheck(self, *args, **kwargs):
        if not hasattr(self.interface, 'current_profile'):
            raise WicdError('Must run set_current_network() before %s()' % method.func_name)
        return method(self, *args, **kwargs)
    profilecheck.func_name = method.func_name
    return profilecheck
class BackendWiredInterface(BaseInterface):
    @staticmethod
    def get_type():
        return 'wired'
    @staticmethod
    def find_available_interfaces():
        """
        Static method. Returns a list of the interfaces of this type.
        Accepts:
        Nothing
        Returns:
        A list with zero or more strings of the names of interfaces.
        These names can be passed to __init__ to instantiate an object.
        """
        basedir = '/sys/class/net/'
        return [iface for iface in os.listdir(basedir) if not 'wireless' 
            in os.listdir(basedir + iface) and 
            open(basedir + iface + "/type").readlines()[0].strip() == "1"]
    def __init__(self, interface_name, status_change_callback):
        BaseInterface.__init__(self, interface_name, status_change_callback)
        if not interface_name in self.find_available_interfaces():
            raise self.CannotCreateInterfaceException()
        self.name = 'Wired Interface %s' % self.interface_name
        self._status_change('idle')
        self.interface = ThreadedWiredInterface(interface_name)
        self.interface.load_profiles()
    def get_status(self):
        return "Wired Interface. Connected %s" % self.get_connected_to_network()
    @needsidle
    def do_update(self):
        """ Updates interface status. """
        self._status_change('updating')
        self._status_change('idle')
    def get_ip(self):
        """ Gets the interface's current IP. """
        return self.interface.get_ip()
    def get_has_link(self):
        ''' Returns True if the interface has a link, False otherwise. '''
        return self.interface.check_link()
    @needsidle
    def set_current_profile(self, profile_name):
        ''' Sets the current network to the one with the specified BSSID. '''
        self.interface.current_profile = \
            self.interface._get_profile('profile_name', profile_name)
    @needsidle
    def do_create_profile(self, profile_name):
        ''' Creates a new network profile with the specified name. '''
        self.interface.create_profile(profile_name)
    def get_profiles(self):
        ''' Returns a list of all the profile names. '''
        return self.interface.list_profiles()
    def _get_profile(self, the_property, value):
        ''' Returns the first network where property == value. '''
        for network in self.interface.networks:
            if hasattr(network, the_property):
                if getattr(network, the_property) == value:
                    return network
    def get_connected_to_network(self):
        return bool(self.interface.get_ip())
    @needsidle
    def set_profile_property(self, name, value):
        self.interface.current_profile[name] = value
    @needscurrentprofile
    def get_profile_property(self, name):
        if not name in self.interface.current_profile:
            raise WicdError('Current network has no property %s' % name)
        return self.interface.current_profile[name]
    @needscurrentprofile
    @needsidle
    def do_connect(self):
        ''' Connects to the wireless network set with set_current_network(). '''
        def finished_callback():
            ''' Sets the current interface state. '''
            self._connected_to_network = True
            self._status_change('idle')
        self._status_change('connecting')
        self.interface.connect(finished_callback)
    def do_cancel_connect(self):
        ''' Cancels the current connection attempt. '''
        self.interface.cancel_connection_attempt()
    @needsidle
    def do_save(self):
        ''' Saves the wireless profiles. '''
        self.interface.save_profiles()
    @needscurrentprofile
    def do_autoconnect(self, value):
        log('%s: do_autoconnect %s' % (self.interface.interface_name, value))
        if self.interface.current_profile:
            self.do_connect()
