""" wicd - wireless connection daemon frontend implementation
This module implements a usermode frontend for wicd.  It updates connection
information, provides an (optional) tray icon, and allows for launching of 
the wicd GUI and Wired Profile Chooser.
class TrayIcon() -- Parent class of TrayIconGUI and IconConnectionInfo.
    class TrayConnectionInfo() -- Child class of TrayIcon which provides
        and updates connection status.
    class TrayIconGUI() -- Child class of TrayIcon which implements the tray.
        icon itself.  Parent class of StatusTrayIconGUI and EggTrayIconGUI.
    class StatusTrayIconGUI() -- Implements the tray icon using a 
                                 gtk.StatusIcon.
    class EggTrayIconGUI() -- Implements the tray icon using egg.trayicon.
def usage() -- Prints usage information.
def main() -- Runs the wicd frontend main loop.
"""

import sys

import gtk

import gobject

import getopt

import os

import pango

import time

from dbus import DBusException

from dbus import version as dbus_version

from wicd import wpath

from wicd import misc

from wicd import gui

from wicd.dbusmanager import DBusManager

from logfile import log

if not (gtk.gtk_version[0] >= 2 and gtk.gtk_version[1] >= 10):

    try:

        import egg.trayicon

        USE_EGG = True

    except ImportError:

        log( 'Unable to load wicd.py: Missing egg.trayicon module.' )

        sys.exit(1)

else:

    USE_EGG = False



if not dbus_version or (dbus_version < (0, 80, 0)):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)



misc.RenameProcess("wicd-client")

if __name__ == '__main__':

    wpath.chdir(__file__)



dbus_manager = None

daemon = None

wireless = None

wired = None

wired = None

language = misc.get_language_list_tray()











if __name__ == '__main__':

    main(sys.argv)



































if not (gtk.gtk_version[0] >= 2 and gtk.gtk_version[1] >= 10):

    class Dummy(object): pass

    gtk.StatusIcon = Dummy

    try:

        import egg.trayicon

        USE_EGG = True

    except ImportError:

        print 'Unable to load tray icon: Missing egg.trayicon module.'

        ICON_AVAIL = False

else:

    USE_EGG = False



if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)



if __name__ == '__main__':

    wpath.chdir(__file__)



if __name__ == '__main__':

    main(sys.argv)



