""" Wicd GUI module.
Module containg all the code (other than the tray icon) related to the 
Wicd user interface.
"""

import os

import sys

import time

import gobject

import pango

import gtk

import gtk.glade

from dbus import Dictionary as DBusDictionary

from dbus import DBusException

from dbus import version as dbus_version

from wicd import wpath

from dbusmanager import DBusManager

from xmlui import simpleinterfacegtk as simpleinterface

from wicd import misc

misc.RenameProcess('wicd-gui')

dbus_manager = DBusManager()

dbus_manager.connect_to_dbus()

dbus_ifaces = dbus_manager.get_dbus_ifaces()

daemon = dbus_ifaces['daemon']

interface = dbus_ifaces['interface']

ui = dbus_ifaces['ui']

if __name__ == '__main__':

    wpath.chdir(__file__)



try:

    import pygtk

    pygtk.require("2.0")

except:

    pass



if not dbus_version or (dbus_version < (0, 80, 0)):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)









if __name__ == '__main__':

    app = appGui(standalone=True)

    gtk.main()

























































































































































































































































if __name__ == '__main__':

    wpath.chdir(__file__)



try:

    import pygtk

    pygtk.require("2.0")

except:

    pass



if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)



if __name__ == '__main__':

    app = appGui(standalone=True)

    bus.add_signal_receiver(app.dbus_scan_finished, 'SendEndScanSignal',
                            'org.wicd.daemon')

    bus.add_signal_receiver(app.dbus_scan_started, 'SendStartScanSignal',
                            'org.wicd.daemon')

    bus.add_signal_receiver(app.update_connect_buttons, 'StatusChanged',
                            'org.wicd.daemon')

    gtk.main()



