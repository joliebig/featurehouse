""" Interface manager for wicd.
Manages and loads the interfaces for wicd.
"""
from configmanager import ConfigManager
from backend import BackendManager
import wpath
def print_data(data): pass

class  InterfaceManager (object) :
	def __init__(self, status_change_callback=print_data):
        ''' Creates a new InterfaceManager object. '''
        self._interfaces = {}
        path = wpath.etc + "interfaces.conf"
        self.config_manager = ConfigManager(path)
        self.backend_manager = BackendManager()
        self.backend_manager.load_all_available_backends()
        self.status_change_callback = status_change_callback
    
	def add(self, interface):
        ''' Adds interface to the dictionary of interfaces. '''
        self._interfaces[interface.interface_name] = interface
    
	def create(self, type, interface_name):
        ''' Creates the interface, if possible. If interface exists, returns. '''
        if not self.exists(interface_name):
            type_class = self.backend_manager.get_backend_by_type(type)
            new_interface = type_class(interface_name, self.status_change_callback)
            self.add(new_interface)
            print 'new interface: %s' % new_interface.interface_name
        else:
            print 'interface already exists: %s' % interface_name
    
	def get(self, interface_name):
        ''' Gets a single interface object. '''
        return self._interfaces.get(interface_name)
    
	def exists(self, interface_name):
        ''' Returns True if the specified interface exists, otherwise False. '''
        return bool(self._interfaces.get(interface_name))
    
	def load(self):
        """ Loads the saved interface configuration. """
        sections = self.config_manager.sections()
        for section in sections:
            interface_name = section
            type = self.config_manager.get_option(section, 'type')
            self.create(type, interface_name)
            interface = self.get(interface_name)
            for k, v in self.config_manager.items(section):
                setattr(interface, k, v)
    
	def save(self):
        """ Saves the current interface configuration. """
        self.config_manager.clear_all()
        for interface_name in self.get_all():
            interface = self.get(interface_name)
            interface.do_save()
            settings = interface.dump_settings_to_dictionary()
            for k, v in settings.iteritems():
                self.config_manager.set_option(interface_name, k, v)
        self.config_manager.write()
    
	def get_all(self):
        ''' Returns the interface dictionary. '''
        return self._interfaces
    
	def get_all_names(self):
        ''' Returns the names of all the interfaces. '''
        interfaces = self.get_all()
        names = [ value.interface_name for key, value in interfaces.iteritems() ]
        return names


