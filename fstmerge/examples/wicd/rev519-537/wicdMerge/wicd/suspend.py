""" Suspends the wicd daemon.
Sets a flag in the daemon that will stop it from monitoring networkg status.
Used for when a laptop enters hibernation/suspension.
"""
import dbus
import dbus.service
bus = dbus.SystemBus()
proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon')
daemon = dbus.Interface(proxy_obj, 'org.wicd.daemon')
if __name__ == '__main__':
    daemon.Disconnect()
    daemon.SetForcedDisconnect(False)
    daemon.SetSuspend(True)

if __name__ == '__main__':
    daemon.Disconnect()
    daemon.SetForcedDisconnect(False)
    daemon.SetSuspend(True)

