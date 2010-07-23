'''
[Home]
test0 = wireless, get_networks, 00:00:00:00:00:00
test0 = wireless, get_networks, MyRandomAccessPointESSID
action0 = wired, set_current_profile, MyHomeWiredNetworkProfileName
[SomewhereElse]
test0 = wireless, get_networks, "ALargeNetworkWithManyAPsESSID"
action0 = wired, set_current_profile, "BigPlace"
'''
from baseplugin import BasePlugin
from configmanager import ConfigManager
from misc import WicdError
from logfile import log
global get_all_by_type
get_all_by_type = None
class LocationError(WicdError): pass
class BaseItem(object):
    def __init__(self, test_line):
        self.interface_type, self.method_name, self.data = \
            self._parse_line(test_line)
    def _test_interface_type_has_method(self):
        interfaces = get_all_by_type(self.interface_type)
        for interface in interfaces:
            if not hasattr(interface, self.method_name):
                raise LocationError('Error in locations.conf: ' + \
                                    'Interface %s does not have method %s' %
                                    (interface.interface_name, self.method_name))
    def _parse_line(self, line):
        parts = line.split(',')
        parts = [ part.strip() for part in parts ]
        if not len(parts) == 3:
            raise LocationError("Error in locations.conf: " + \
                                "must be exactly 2 commas per line.")
        self.interface_type = parts[0]
        self.method_name = parts[1]
        self._test_interface_type_has_method()
        return parts
class Test(BaseItem):
    def _recursive_find(self, the_list, value):
        ''' Finds an item in embedded iterables.
	For example:
	iterables = [{'a' : 'b'},['c','d',['e',['f'],]]]
	>>> self._recursive_find(iterables, 'f')
	True
	>>> self._recursive_find(iterables, 'b')
	True
	whereas
	>>> 'f' in iterables
	False
	>>> 'b' in iterables
	False
	'''
        def check_item(item):
            if item == value:
                return True
            else:
                if self._recursive_find(item, value):
                    return True
        try:
            if value in the_list:
                return True
            elif hasattr(the_list, 'iteritems'):
                for item in the_list.iteritems():
                    if check_item(item):
                        return True
            elif hasattr(the_list, '__iter__'):
                for item in the_list:
                    if check_item(item):
                        return True
            elif value == the_list:
                return True
        except TypeError:
            pass
        return False
    def perform(self):
        self._test_interface_type_has_method()
        interfaces = get_all_by_type(self.interface_type)
        passed = []
        for interface in interfaces:
            result = False
            try:
                result = getattr(interface, self.method_name)()
            except WicdError, e:
                log( 'error occured while running %s: %s' % (self.method_name, e))
            else:
                passed.append(self._recursive_find(result, self.data))
        return passed
class Action(BaseItem):
    def perform(self):
        self._test_interface_type_has_method()
        interfaces = get_all_by_type(self.interface_type)
        for interface in interfaces:
            try:
                getattr(interface, self.method_name)(self.data)
            except WicdError, e:
                log( 'error occured while running %s: %s' % (self.method_name, e))
        return True
class Location(object):
    def __init__(self, name, test_strings, action_strings):
        self.name = name
        self.tests = self._parse_items(test_strings, Test)
        self.actions = self._parse_items(action_strings, Action)
    def _parse_items(self, strings, the_class):
        results = []
        for string in strings:
            results.append(the_class(string))
        return results
    def run_tests(self):
        results = []
        for test in self.tests:
            results.extend(test.perform())
        return results
    def do_actions(self):
        for action in self.actions:
            action.perform()
class LocationManager(ConfigManager):
    class NoLocationFoundError(LocationError): pass
    def __init__(self, *args):
        ConfigManager.__init__(self, *args)
        self.locations = []
        self.load_all_locations()
    def load_all_locations(self):
        self.locations = []
        for location_name in self.sections():
            tests = self.get_tests_for_location(location_name)
            actions = self.get_actions_for_location(location_name)
            self.locations.append(Location(location_name, tests, actions))
    def get_tests_for_location(self, location):
        return self._get_numbered_items_by_location('test', location)
    def get_actions_for_location(self, location):
        return self._get_numbered_items_by_location('action', location)
    def _get_numbered_items_by_location(self, item_base, location):
        items = []
        for name, value in self.items(location):
            if name.startswith(item_base):
                try:
                    items.insert(int(name[len(item_base):]), value)
                except ValueError:
                    log( '%s is not a valid item' % name )
        return items
    def find_location(self):
        for location in self.locations:
            results = location.run_tests()
            log( location,'results:',results )
            if True in results and not False in results:
                return location
        raise self.NoLocationFoundError('No location was detected.')
class LocationPlugin(BasePlugin):
    ''' A plugin meant to guess your location so that
    Wicd can configure network connections better. '''
    PRIORITY = 10
    def __init__(self, *args):
        BasePlugin.__init__(self, *args)
        global get_all_by_type
        get_all_by_type = self.daemon.interface_manager.get_all_by_type
        self.location_manager = LocationManager('/etc/wicd/locations.conf')
    def do_start(self):
        pass
    def location_test(self):
        log( 'location plugin triggered...' )
        location = None
        try:
            location = self.location_manager.find_location()
        except LocationManager.NoLocationFoundError:
            log( 'location could not be determined' )
        else:
            log( 'location is', location.name )
            location.do_actions()
            self.daemon.plugin_manager.action('new_location', ( location, ))
    def do_signal_idle_from_scanning(self, interface_name):
        log( 'location plugin caught idle from scanning signal...' )
        self.location_test()
        self.daemon.plugin_manager.action('autoconnect', (interface_name, ))
