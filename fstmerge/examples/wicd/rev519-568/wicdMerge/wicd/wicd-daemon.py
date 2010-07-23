""" wicd - wireless connection daemon implementation.
This module implements the wicd daemon that provides network
connection management, for both wireless and wired networks. The daemon
must be run as root to control the networks, however the user interface
components should be run as a normal user.
class WicdDaemon -- The main DBus daemon for Wicd.
def usage() -- Print usage information.
def daemonize() -- Daemonize the current process with a double fork.
def main() -- The wicd daemon main loop.
"""

import logging, logging.handlers

logging.basicConfig(level=logging.DEBUG,
                    format='%(levelname)-8s %(message)s',
                    datefmt='%Y%b%d %H:%M:%S')

logfile = logging.handlers.RotatingFileHandler('/var/log/wicd/wicd.log',
                                               maxBytes=1024*1024,
                                               backupCount=3)

logfile.setFormatter(
    logging.Formatter(
        '%(levelname)s:%(filename)s:%(funcName)s:%(lineno)d: %(message)s'
        )
    )

logging.getLogger().addHandler(logfile)

import sys, os, optparse, signal

import gobject

import dbus

import dbus.service

if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)



import wpath

import misc

from misc import WicdError

from interfacemanager import InterfaceManager

from pluginmanager import PluginManager

from wglobals import global_config

if __name__ == '__main__':

    wpath.chdir(__file__)



misc.rename_process("wicd")











if __name__ == '__main__':

    if os.getuid() != 0:

        logging.critical( "Root priviledges are required for the daemon to run properly." +
               "  Exiting." )

        sys.exit(1)

    signal.signal(signal.SIGTERM, sigterm_caught)

    try:

        main(sys.argv)

    finally:

        shut_down()



































if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)



if __name__ == '__main__':

    wpath.chdir(__file__)



if __name__ == '__main__':

    if os.getuid() != 0:

        print ("Root priviledges are required for the daemon to run properly." +
               "  Exiting.")

        sys.exit(1)

    main(sys.argv)



