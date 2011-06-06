"""
Browser module
"""
import os
import sys
import re
import logging
from urllib import quote
from twisted.internet import reactor
log = logging.getLogger(__name__)
def setBrowserVersion(browserPath, profile_dir):
    version = os.popen(browserPath + " -v", "r").read()
    log.info(u"Firefox version: " + version)
    vs = re.search(r"(?P<vs>\d+\.\d+\.\d+\.\d+)", version)
    if vs:
        prefs = os.path.join(profile_dir, "prefs.js")
        try:
            lines = open(prefs, 'rt').readlines()
            prefs = open(prefs, 'wt')
            for line in lines:
                if line.find("extensions.lastAppVersion") > -1:
                    line = re.sub(r'\d+\.\d+\.\d+\.\d+',
                            vs.group('vs'), line, 1)
                    log.info(u"updated browser version in prefs: " + line)
                prefs.write(line)
        except IOError:
            log.info(u"Unable to update version number in Firefox preferences")
def launchBrowser(config, packageName):
    """
    Launch the webbrowser (Firefox) for this platform
    """
    log.info(u"Browser path: " + config.browserPath)
    url     = u'http://127.0.0.1:%d/%s' % (config.port, quote(packageName))
    log.info(u"url "+url)
    profile_src = "linux-profile"
    if sys.platform[:3] == u"win":
        profile     = "linux-profile"
    else:
        profile = "linux-profile:"
    if (config.configDir/profile).exists():
        (config.configDir/profile).rmtree()
    log.info("Creating FireFox profile copied from"+
             config.webDir/profile_src+" to "+
             config.configDir/profile)
    (config.webDir/profile_src).copytreeFilter(config.configDir/profile, filterDir=lambda dirName: dirName.basename() != '.svn')
    log.info("setupMoz configDir "+config.configDir+ " profile "+profile)
    log.info(u"profile = " + config.configDir/profile)
    if sys.platform[:5] == u"linux":
        setBrowserVersion(config.browserPath, config.configDir/profile)
    if sys.platform[:3] == u"win":
        try:
            os.environ["MOZ_NO_REMOTE"] = "1"
            os.spawnl(os.P_DETACH, 
                      config.browserPath,
                      config.browserPath.basename(),
                      '-profile', 
                      '"' + config.configDir/profile + '"', 
                      url)
        except OSError:
            print u"Cannot launch Firefox, please manually run Firefox"
            print u"and go to", url     
    else:
        launchString  = 'LOGNAME=eXe7913 '
        launchString += config.browserPath
        launchString += ' -profile "' + config.configDir/profile + '/" '
        launchString += url
        launchString += "&"
        log.info(u'Launching firefox with: ' + launchString)
        os.system(launchString)
