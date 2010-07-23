""" The wicd DBus Manager.
A module for storing wicd's dbus interfaces.
"""

from logfile import log

import dbus

if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)





