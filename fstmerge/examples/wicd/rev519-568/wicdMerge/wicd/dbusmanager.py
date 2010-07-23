""" The wicd DBus Manager.
A module for storing wicd's dbus interfaces.
"""

import dbus

if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)













global dbus_manager

global daemon, interface

if not 'dbus_manager' in globals():

    dbus_manager = DBusManager()

    dbus_manager.connect_to_dbus()

    daemon = dbus_manager.get_dbus_ifaces()['daemon']

    interface = dbus_manager.get_dbus_ifaces()['interface']



