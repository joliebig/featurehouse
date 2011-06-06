"""
Browser module
"""
import os
import sys
import logging
from urllib import quote
from twisted.internet import reactor
log = logging.getLogger(__name__)
def launchBrowser(config, packageName):
    """
    Launch the webbrowser (Firefox) for this platform
    """
    log.info(u"Browser path: " + config.browserPath)
    url     = u'http://127.0.0.1:%d/%s' % (config.port, quote(packageName))
    log.info(u"url "+url)
    profile    = "linux-profile"
    if (config.configDir/profile).exists():
        (config.configDir/profile).rmtree()
    log.info("Creating FireFox profile copied from"+
             config.webDir/profile+" to "+
             config.configDir/profile)
    (config.webDir/profile).copytreeFilter(config.configDir/profile, filterDir=lambda dirName: dirName.basename() != '.svn')
    log.info("setupMoz configDir "+config.configDir+ " profile "+profile)
    log.info(u"profile = " + config.configDir/profile)
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
        launchString += ' -profile "' + config.configDir/profile + '" '
        launchString += url
        launchString += "&"
        log.info(u'Launching firefox with: ' + launchString)
        os.system(launchString)
