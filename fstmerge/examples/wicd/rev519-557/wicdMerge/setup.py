print "DON'T RUN THIS YET. IT WON'T WORK."

import sys

sys.exit(127)

from distutils.core import setup, Command

import os

import shutil

import sys

~~FSTMerge~~ VERSION_NUM = '1.5.4' ##FSTMerge## VERSION_NUM = '1.5.3' ##FSTMerge## VERSION_NUM = '1.6.0'







try:

    import wpath

except:

    print '''Error importing wpath.py. You can safely ignore this
message. It is probably because you haven't run python setup.py
configure yet or you are running it for the first time.'''



data = []

try:

    print "Using init file",(wpath.init, wpath.initfile)

    data = [
    (wpath.dbus, ['other/wicd.conf']),
    (wpath.desktop, ['other/wicd.desktop']),
    (wpath.log, []), 
    (wpath.icons + 'scalable/apps/', ['icons/scalable/wicd-client.svg']),
    (wpath.icons + '192x192/apps/', ['icons/192px/wicd-client.png']),
    (wpath.icons + '128x128/apps/', ['icons/128px/wicd-client.png']),
    (wpath.icons + '96x96/apps/', ['icons/96px/wicd-client.png']),
    (wpath.icons + '72x72/apps/', ['icons/72px/wicd-client.png']),
    (wpath.icons + '64x64/apps/', ['icons/64px/wicd-client.png']),
    (wpath.icons + '48x48/apps/', ['icons/48px/wicd-client.png']),
    (wpath.icons + '36x36/apps/', ['icons/36px/wicd-client.png']),
    (wpath.icons + '32x32/apps/', ['icons/32px/wicd-client.png']),
    (wpath.icons + '24x24/apps/', ['icons/24px/wicd-client.png']),
    (wpath.icons + '22x22/apps/', ['icons/22px/wicd-client.png']),
    (wpath.icons + '16x16/apps/', ['icons/16px/wicd-client.png']),
    (wpath.images, [('images/' + b) for b in os.listdir('images') if not b.startswith('.')]),
    (wpath.encryption, [('encryption/templates/' + b) for b in os.listdir('encryption/templates') if not b.startswith('.')]),
    (wpath.networks, []),
    (wpath.bin, ['scripts/wicd-client', ]), 
    (wpath.sbin,  ['scripts/wicd', ]),  
    (wpath.share, ['data/wicd.glade', ]),
    (wpath.lib, ['wicd/wicd-client.py', 'wicd/monitor.py', 'wicd/wicd-daemon.py', 'wicd/configscript.py', 'wicd/suspend.py', 'wicd/autoconnect.py']), 
    (wpath.backends, ['wicd/backends/be-external.py', 'wicd/backends/be-ioctl.py']),
    (wpath.autostart, ['other/wicd-tray.desktop', ]),
    (wpath.docdir, [ 'AUTHORS', 'README' ])
    ]

    piddir = os.path.dirname(wpath.pidfile)

    if not piddir.endswith('/'):

        piddir += '/'

    data.append (( piddir, [] ))

    if not wpath.no_install_install:

        data.append(( wpath.docdir, [ 'INSTALL' ] ))

    if not wpath.no_install_license:

        data.append(( wpath.docdir, [ 'LICENSE' ] ))

    if not wpath.no_install_kde:

        data.append(( wpath.kdedir, [ 'other/wicd-tray.desktop' ]))

    if not wpath.no_install_init:

        data.append(( wpath.init, [ wpath.initfile ]))

    if not wpath.no_install_man:

        data.append(( wpath.mandir + 'man8/', [ 'man/wicd.8' ]))

        data.append(( wpath.mandir + 'man5/', [ 'man/wicd-manager-settings.conf.5' ]))

        data.append(( wpath.mandir + 'man5/', [ 'man/wicd-wired-settings.conf.5' ]))

        data.append(( wpath.mandir + 'man5/', [ 'man/wicd-wireless-settings.conf.5' ]))

    if not wpath.no_install_acpi:

        data.append(( wpath.resume, ['other/80-wicd-connect.sh' ]))

        data.append(( wpath.suspend, ['other/50-wicd-suspend.sh' ]))

    print 'Creating pid path', os.path.basename(wpath.pidfile)

    print 'Language support for',

    for language in os.listdir('translations/'):

        if not language.startswith('.'):

            codes = language.split('_')

            short_language = language

            if codes[0].lower() == codes[1].lower():

                short_language = codes[0].lower()

            print short_language,

            data.append((wpath.translations + short_language + '/LC_MESSAGES/', ['translations/' + language + '/LC_MESSAGES/wicd.mo']))

    print

except:

    print '''Error setting up data array. This is normal if 
python setup.py configure has not yet been run.'''



setup(cmdclass={'configure' : configure, 'get_translations' : get_translations, 'uninstall' : uninstall},
      name="Wicd",
      version=VERSION_NUM,
      description="A wireless and wired network manager",
      long_description="""A complete network connection manager
Wicd supports wired and wireless networks, and capable of
creating and tracking profiles for both.  It has a 
template-based wireless encryption system, which allows the user
to easily add encryption methods used.  It ships with some common
encryption types, such as WPA and WEP. Wicd will automatically
connect at startup to any preferred network within range.
""",
      author="Adam Blackburn, Dan O'Reilly",
      author_email="compwiz18@users.sourceforge.net, imdano@users.sourceforge.net",
      url="http://wicd.net",
      license="http://www.gnu.org/licenses/old-licenses/gpl-2.0.html",
      py_modules=['wicd.networking', 'wicd.misc', 'wicd.gui', 'wicd.wnettools', 'wicd.wpath', 'wicd.prefs', 'wicd.netentry', 'wicd.dbusmanager', 'wicd.logfile', 'wicd.backend', 'wicd.configmanager'],
      data_files=data
      )



try:

    import wpath

except:

    print '''Error importing wpath.py. You can safely ignore this
message. It is probably because you haven't run python setup.py
configure yet or you are running it for the first time.'''



try:

    print "Using init file",(wpath.init, wpath.initfile)

    data = [
    (wpath.dbus, ['other/wicd.conf']),
    (wpath.desktop, ['other/wicd.desktop']),
    (wpath.log, []), 
    (wpath.etc, []),
    (wpath.icons + 'scalable/apps/', ['icons/scalable/wicd-client.svg']),
    (wpath.icons + '192x192/apps/', ['icons/192px/wicd-client.png']),
    (wpath.icons + '128x128/apps/', ['icons/128px/wicd-client.png']),
    (wpath.icons + '96x96/apps/', ['icons/96px/wicd-client.png']),
    (wpath.icons + '72x72/apps/', ['icons/72px/wicd-client.png']),
    (wpath.icons + '64x64/apps/', ['icons/64px/wicd-client.png']),
    (wpath.icons + '48x48/apps/', ['icons/48px/wicd-client.png']),
    (wpath.icons + '36x36/apps/', ['icons/36px/wicd-client.png']),
    (wpath.icons + '32x32/apps/', ['icons/32px/wicd-client.png']),
    (wpath.icons + '24x24/apps/', ['icons/24px/wicd-client.png']),
    (wpath.icons + '22x22/apps/', ['icons/22px/wicd-client.png']),
    (wpath.icons + '16x16/apps/', ['icons/16px/wicd-client.png']),
    (wpath.images, [('images/' + b) for b in os.listdir('images') if not b.startswith('.')]),
    (wpath.encryption, [('encryption/templates/' + b) for b in os.listdir('encryption/templates') if not b.startswith('.')]),
    (wpath.networks, []),
    (wpath.bin, ['scripts/wicd-client', ]), 
    (wpath.sbin,  ['scripts/wicd', ]),  
    (wpath.share, ['data/wicd.glade', ]),
    (wpath.lib, ['wicd/wicd-client.py', 'wicd/monitor.py', 'wicd/wicd-daemon.py', 'wicd/configscript.py', 'wicd/suspend.py', 'wicd/autoconnect.py']), 
    (wpath.autostart, ['other/wicd-tray.desktop', ]),
    ]

    piddir = os.path.dirname(wpath.pidfile)

    if not piddir.endswith('/'):

        piddir += '/'

    data.append (( piddir, [] ))

    if not wpath.no_install_docs:

        data.append(( wpath.docdir, [ 'INSTALL', 'LICENSE', 'AUTHORS', 'README' ]))

    if not wpath.no_install_kde:

        data.append(( wpath.kdedir, [ 'other/wicd-tray.desktop' ]))

    if not wpath.no_install_init:

        data.append(( wpath.init, [ wpath.initfile ]))

    if not wpath.no_install_man:

        data.append(( wpath.mandir + 'man8/', [ 'man/wicd.8' ]))

        data.append(( wpath.mandir + 'man5/', [ 'man/wicd-manager-settings.conf.5' ]))

        data.append(( wpath.mandir + 'man5/', [ 'man/wicd-wired-settings.conf.5' ]))

        data.append(( wpath.mandir + 'man5/', [ 'man/wicd-wireless-settings.conf.5' ]))

        data.append(( wpath.mandir + 'man1/', [ 'man/wicd-client.1' ]))

    if not wpath.no_install_acpi:

        data.append(( wpath.resume, ['other/80-wicd-connect.sh' ]))

        data.append(( wpath.suspend, ['other/50-wicd-suspend.sh' ]))

    if not wpath.no_install_pmutils:

        data.append(( wpath.pmutils, ['other/55-wicd' ]))

    print 'Creating pid path', os.path.basename(wpath.pidfile)

    print 'Language support for',

    for language in os.listdir('translations/'):

        if not language.startswith('.'):

            codes = language.split('_')

            short_language = language

            if codes[0].lower() == codes[1].lower():

                short_language = codes[0].lower()

            print short_language,

            data.append((wpath.translations + short_language + '/LC_MESSAGES/', ['translations/' + language + '/LC_MESSAGES/wicd.mo']))

    print

except:

    print '''Error setting up data array. This is normal if 
python setup.py configure has not yet been run.'''



