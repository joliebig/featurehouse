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

try:

    import pygtk

    pygtk.require("2.0")

except:

    pass



from wicd import wpath

from misc import rename_process

from dbusmanager import daemon

from dbus.exceptions import DBusException

from uimanager import UiManager

import logging

logging.basicConfig(level=logging.DEBUG,
                    format='%(message)s',
                    datefmt='%Y%b%d %H:%M:%S')

logging.info('Wicd GUI initalizing...')

rename_process('wicd-gui')



if __name__ == '__main__':

    app = WicdGui(standalone=True)

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



