from distutils.core import setup, Extension
import glob, os.path
mo_files = []
for filepath in glob.glob("po/*/LC_MESSAGES/*.mo"):
    lang = filepath[len("po/"):]
    targetpath = os.path.dirname(os.path.join("share/locale",lang))
    mo_files.append((targetpath, [filepath]))
libmimic_module = Extension('libmimic',
                    sources = ['libmimic/' + file for file in ['bitstring.c', 
                     'colorspace.c', 'deblock.c', 'decode.c', 'encode.c',
                     'fdct_quant.c', 'idct_dequant.c', 'mimic.c', 'vlc_common.c', 
                     'vlc_decode.c', 'vlc_encode.c', 'py_libmimic.c']])
setup(name         = 'emesene',
      version      = '1.1',  # emesenecommon no longer exists
      description  = 'MSN messenger client',
      author       = 'Luis Mariano Guerra and dx',
      author_email = 'marianoguerra@users.sourceforge.net',
      url          = 'http://www.emesene.org/',
      license      = 'GNU GPL 2',
      requires     = ["gtk"],
      platforms    = ["Platform Independent"],
      packages     = ['', 'abstract', 'emesenelib', 'plugins_base', 'plugins_base.currentSong'],
      package_data = {'./': ['conversation_themes/*/*', 'smilies/*/*',
                           'sound_themes/*/*', 'themes/*/*', 'hotmlog.htm']},
      scripts      = ['emesene'],
      data_files   = [('share/pixmaps', ['misc/emesene.png']),
                      ('share/icons/hicolor/scalable/apps', ['misc/emesene.svg']),
                      ('share/man/man1', ['misc/emesene.1']),
                      ('share/applications', ['misc/emesene.desktop'])] + mo_files,
      ext_modules = [libmimic_module]
      )
