nsis_options = ''
nsis = 'c:\Program Files\NSIS\makensis.exe'
BRANDED_JPG = 'splashb.jpg'
import sys
import os
import shutil
import subprocess
import Image, ImageFont, ImageDraw
os.chdir('../..'); WDIR = os.getcwd()
shutil.rmtree('build', True)
shutil.rmtree('dist', True)
shutil.copytree('exe/webui/Mozilla Firefox', 'dist/Mozilla Firefox')
subprocess.check_call('python win-setup.py py2exe', shell = True, cwd = WDIR)
sys.path.insert(0, WDIR)
from exe.engine import version
versions = "/DEXE_VERSION=%s /DEXE_REVISION=%s /DEXE_SPLASH=%s" \
        % (version.release, version.revision, BRANDED_JPG)
os.chdir(os.path.join(WDIR, 'installs/windows'))
font = ImageFont.truetype("arial.ttf", 12)
fontcolor = '#808080'
(w, h) = font.getsize("Version:")
im = Image.open("splash1.jpg")
draw = ImageDraw.Draw(im)
draw.text((150, 102), "Version: " + version.release, font=font,
        fill=fontcolor)
draw.text((150, 102+h), "Revision: " + version.revision,
        font=font, fill=fontcolor)
del draw
im.save(BRANDED_JPG)
for installer in ('exe.nsi', 'exe.standalone.nsi'):
    try:
        pnsis = subprocess.Popen('%s %s %s %s' %
                                 ('makensis', nsis_options, versions, installer))
    except OSError:
        try:
            pnsis = subprocess.Popen('%s %s %s %s' %
                                     (nsis, nsis_options, versions, installer))
        except OSError:
            print '*** unable to run makensis, check PATH or explicit pathname'
            print '    in make.py'
    pnsis.wait()
os.remove(BRANDED_JPG)
