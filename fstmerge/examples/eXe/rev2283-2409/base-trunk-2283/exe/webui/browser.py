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
    if sys.platform[:3] == u"win":
        profile    = "win-profile"
    else:
        profile    = "linux-profile"
    if (config.configDir/profile).exists():
        (config.configDir/profile).rmtree()
    log.info("Creating FireFox profile copied from"+
             config.webDir/profile+" to "+
             config.configDir/profile)
    if not (config.configDir/profile).exists():
        (config.configDir/profile).mkdir()
    for filename in (config.webDir/profile).files():
        filename.copy(config.configDir/profile)
    if not (config.configDir/profile/'Cache').exists():
        (config.configDir/profile/'Cache').mkdir()
    for filename in (config.webDir/profile/'Cache').files():
        filename.copy(config.configDir/profile/'Cache')
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
            reactor.callLater(10, tryAgain, config, profile, url)
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
def tryAgain(config, profile, url):
    """
    Second try of launching the browser,
    called by reactor on a timer thingy
    """
    os.spawnl(os.P_DETACH, 
              config.browserPath,
              config.browserPath.basename(),
              '-profile', 
              '"' + config.configDir/profile + '"', 
              url)
