from distutils.core import setup
import glob, os.path
from emesenecommon import APP_VERSION
mo_files = []
for filepath in glob.glob("po/*/LC_MESSAGES/*.mo"):
    lang = filepath[len("po/"):]
    targetpath = os.path.dirname(os.path.join("share/locale",lang))
    mo_files.append((targetpath, [filepath]))
setup(name         = 'emesene',
      version      = APP_VERSION,
      description  = 'MSN messenger client',
      author       = 'Luis Mariano Guerra',
      author_email = 'marianoguerra@users.sourceforge.net',
      url          = 'http://www.emesene.org/',
      license      = 'GNU GPL 2',
      requires     = ["gtk"],
      platforms    = ["Platform Independent"],
      packages     = ['', 'abstract', 'emesenelib', 'plugins_base', 'plugins_base.currentSong'],
      package_data = {'': ['conversation_themes/*/*', 'smilies/*/*',
                           'sound_themes/*/*', 'themes/*/*', 'hotmlog.htm']},
      scripts      = ['emesene'],
      data_files   = [('share/pixmaps', ['misc/emesene.png']),
                      ('share/icons/hicolor/scalable/apps', ['misc/emesene.svg']),
                      ('share/man/man1', ['misc/emesene.1']),
                      ('share/applications', ['misc/emesene.desktop'])] + mo_files
      )
