""" The wicd DBus Manager.
A module for storing wicd's dbus interfaces.
"""
import dbus
class DBusManager(object):
    def __init__(self):
        self._bus = dbus.SystemBus()
        self._dbus_ifaces = {}  
    def get_dbus_ifaces(self):
        """ Returns a dict of dbus interfaces. """
        return self._dbus_ifaces
    def get_bus(self):
        """ Returns the loaded SystemBus. """
        return self._bus
    def connect_to_dbus(self):
        """ Connects to wicd's dbus interfaces and loads them into a dict. """
        proxy_obj = self._bus.get_object("org.wicd.daemon", '/org/wicd/daemon')
        daemon = dbus.Interface(proxy_obj, 'org.wicd.daemon')
        interface = dbus.Interface(proxy_obj, 'org.wicd.daemon.interface')
        ui = dbus.Interface(proxy_obj, 'org.wicd.daemon.ui')
        self._dbus_ifaces = {"daemon" : daemon,
                                "interface" : interface, 
                                "ui" : ui}
