""" monitor -- connection monitoring process
This process is spawned as a child of the daemon, and is responsible
for monitoring connection status and initiating autoreconnection
when appropriate.
"""

import dbus

import gobject

import time

if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)



import wicd.wpath as wpath

import wicd.misc as misc

from logfile import log

misc.RenameProcess("wicd-monitor")

if __name__ == '__main__':

    wpath.chdir(__file__)



bus = dbus.SystemBus()

proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon')

daemon = dbus.Interface(proxy_obj, 'org.wicd.daemon')

proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon/wired')

wired = dbus.Interface(proxy_obj, 'org.wicd.daemon.wired')

proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon/wireless')

wireless = dbus.Interface(proxy_obj, 'org.wicd.daemon.wireless')









if __name__ == '__main__':

    main()







if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)



if __name__ == '__main__':

    wpath.chdir(__file__)



if __name__ == '__main__':

    main(sys.argv[1:])



