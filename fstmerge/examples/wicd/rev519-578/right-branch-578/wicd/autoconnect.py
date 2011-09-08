import dbus
import time
import gobject
from dbus.mainloop.glib import DBusGMainLoop
import logging
DBusGMainLoop(set_as_default=True)
bus = dbus.SystemBus()
proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon')
daemon = dbus.Interface(proxy_obj, 'org.wicd.daemon')
def reply_handle():
    loop.quit()
def error_handle(e):
    loop.quit()
log(daemon.Hello())
time.sleep(3)
daemon.SetSuspend(False)
if not daemon.CheckIfConnecting():
    logging.debug(daemon.AutoConnect(True, reply_handler=reply_handle,
                             error_handler=error_handle))
    daemon.SetForcedDisconnect(False)
loop = gobject.MainLoop()
loop.run()
