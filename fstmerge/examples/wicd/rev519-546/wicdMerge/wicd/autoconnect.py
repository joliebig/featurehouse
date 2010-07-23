import dbus

import time

import gobject

from dbus.mainloop.glib import DBusGMainLoop

DBusGMainLoop(set_as_default=True)

bus = dbus.SystemBus()

proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon')

daemon = dbus.Interface(proxy_obj, 'org.wicd.daemon')





print daemon.Hello()

time.sleep(3)

daemon.SetSuspend(False)

if not daemon.CheckIfConnecting():

    print daemon.AutoConnect(True, reply_handler=reply_handle,
                             error_handler=error_handle)

    daemon.SetForcedDisconnect(False)



loop = gobject.MainLoop()

loop.run()

if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):

    import dbus.glib

else:

    from dbus.mainloop.glib import DBusGMainLoop

    DBusGMainLoop(set_as_default=True)



if not daemon.CheckIfConnecting():

    print daemon.AutoConnect(True, reply_handler=reply_handle,
                             error_handler=error_handle)

    daemon.SetForcedDisconnect(False)



