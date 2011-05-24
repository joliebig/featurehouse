""" The wicd DBus Manager.
A module for storing wicd's dbus interfaces.
"""
import dbus
if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):
    import dbus.glib
else:
    from dbus.mainloop.glib import DBusGMainLoop
    DBusGMainLoop(set_as_default=True)
def prepare_data_for_dbus(data):
    if data in [('dbusdoesntallowemptytuples',), None, (),
                'dbusdoesntallowemptytuples', []]:
        return ('dbusdoesntallowemptytuples',)
    else:
        return data
def undo_data_from_dbus(data):
    if data in (('dbusdoesntallowemptytuples', ), 'dbusdoesntallowemptytuples'):
        return tuple()
    else:
        return data
class DBusManager(object):
    def __init__(self):
        self._bus = dbus.SystemBus()
        self._dbus_ifaces = {}
        self._connected_to_dbus = False
    def get_dbus_ifaces(self):
        """ Returns a dict of dbus interfaces. """
        return self._dbus_ifaces
    def get_bus(self):
        """ Returns the loaded SystemBus. """
        return self._bus
    def connect_to_signal(self, signal_name, method):
        self.proxy_obj.connect_to_signal(signal_name, method)
    def connect_to_dbus(self):
        """ Connects to wicd's dbus interfaces and loads them into a dict. """
        if not self._connected_to_dbus:
            self._connected_to_dbus = True
            proxy_obj = self._bus.get_object("org.wicd.daemon",
                                             '/org/wicd/daemon')
            self.proxy_obj = proxy_obj
            daemon = dbus.Interface(proxy_obj, 'org.wicd.daemon')
            interface = dbus.Interface(proxy_obj, 'org.wicd.daemon.interface')
            ui = dbus.Interface(proxy_obj, 'org.wicd.daemon.ui')
            self._dbus_ifaces = {"daemon" : daemon,
                                "interface" : interface,
                                "ui" : ui}
def do(interface_name, method, data=None):
    data = prepare_data_for_dbus(data)
    return undo_data_from_dbus(
        interface.DoInterfaceAction(interface_name, method, data)
    )
def get(interface_name, method, data=None):
    data = prepare_data_for_dbus(data)
    return undo_data_from_dbus(
        interface.GetInterfaceData(interface_name, method, data)
    )
def set(interface_name, method, data=None):
    data = prepare_data_for_dbus(data)
    return undo_data_from_dbus(
        interface.SetInterfaceData(interface_name, method, data)
    )
global dbus_manager
global daemon, interface
if not 'dbus_manager' in globals():
    dbus_manager = DBusManager()
    dbus_manager.connect_to_dbus()
    daemon = dbus_manager.get_dbus_ifaces()['daemon']
    interface = dbus_manager.get_dbus_ifaces()['interface']
