""" Path configuration and functions for the wicd daemon and gui clients.
chdir() -- Change directory to the location of the current file.
"""
import os
current = os.path.dirname(os.path.realpath(__file__)) + '/'
lib = '/usr/lib/wicd/'
share = '/usr/share/wicd/'
etc = '/etc/wicd/'
images = '/usr/share/pixmaps/wicd/'
encryption = '/etc/wicd/encryption/templates/'
bin = '/usr/bin/'
networks = '/var/lib/wicd/configurations/'
log = '/var/log/wicd/'
resume = '/etc/acpi/resume.d/'
suspend = '/etc/acpi/suspend.d/'
sbin = '/usr/sbin/'
dbus = '/etc/dbus-1/system.d/'
desktop = '/usr/share/applications/'
backends= '/usr/lib/wicd/backends/'
translations = '/usr/share/locale/'
icons = '/usr/share/icons/hicolor/'
autostart = '/etc/xdg/autostart/'
init = '/etc/init.d/'
docdir = '/usr/share/doc/wicd/'
mandir = '/usr/share/man/'
kdedir = '/usr/share/autostart/'
python = '/usr/bin/python'
pidfile = '/var/run/wicd/wicd.pid'
initfile = 'init/debian/wicd'
initfilename = 'wicd'
no_install_init = False
no_install_man = False
no_install_kde = False
no_install_acpi = False
no_install_install = False
no_install_license = False
def chdir(file):
    """Change directory to the location of the specified file.
    Keyword arguments:
    file -- the file to switch to (usually __file__)
    """
    os.chdir(os.path.dirname(os.path.realpath(file)))

