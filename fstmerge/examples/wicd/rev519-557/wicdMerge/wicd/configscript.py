""" Configure the scripts for a particular network.
Script for configuring the scripts for a network passed in as a
command line argument.  This needs to run a separate process because
editing scripts requires root access, and the GUI/Tray are typically
run as the current user.
"""

import sys

import os

import gtk

import ConfigParser

import dbus

import gtk.glade

import wicd.wpath as wpath

import wicd.misc as misc

_ = misc.get_gettext()

language = {}

language['configure_scripts'] = _("Configure Scripts")

language['before_script'] = _("Pre-connection Script")

language['after_script'] = _("Post-connection Script")

language['disconnect_script'] = _("Disconnection Script")

bus = dbus.SystemBus()

try:

    log( 'Attempting to connect tray to daemon...')

    proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon')

    log( 'Success.')

except Exception:

    log( 'Daemon not running...')

    misc.PromptToStartDaemon()

    sys.exit(1)



wireless = dbus.Interface(proxy_obj, 'org.wicd.daemon.wireless')

wired = dbus.Interface(proxy_obj, 'org.wicd.daemon.wired')

wireless_conf = wpath.etc + 'wireless-settings.conf'

wired_conf = wpath.etc + 'wired-settings.conf'













if __name__ == '__main__':

    if os.getuid() != 0:

        log( "Root privileges are required to configure scripts.  Exiting.")

        sys.exit(0)

    main(sys.argv)





try:

    print 'Attempting to connect tray to daemon...'

    proxy_obj = bus.get_object('org.wicd.daemon', '/org/wicd/daemon')

    print 'Success.'

except Exception:

    print 'Daemon not running...'

    misc.PromptToStartDaemon()

    sys.exit(1)



if __name__ == '__main__':

    if os.getuid() != 0:

        print "Root privileges are required to configure scripts.  Exiting."

        sys.exit(0)

    main(sys.argv)



