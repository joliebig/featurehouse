"""
Main application class, pulls together everything and runs it.
"""
import os
import sys
import shutil
import time
import re
from tempfile import mkdtemp
if os.name == 'posix':
    sys.path.insert(0, '/usr/share/exe')
from getopt import getopt, GetoptError
from exe.webui.webserver     import WebServer
from twisted.internet import reactor
from exe.webui.browser       import launchBrowser
from exe.engine.idevicestore import IdeviceStore
from exe.engine.packagestore import PackageStore
from exe.engine.translate    import installSafeTranslate
from exe.engine              import version
from exe                     import globals
from exe.engine.path         import TempDirPath
import logging
import re
log = logging.getLogger(__name__)
class Windows_Log(object):
    """
    Logging for py2exe application
    """
    def __init__(self, level):
        self.level = level
    def write(self, text):
        log.log(self.level, text)
if sys.platform[:3] == "win":
    sys.stdout = Windows_Log(logging.INFO)
    sys.stderr = Windows_Log(logging.ERROR)
del Windows_Log
globals.application = None
class Application:
    """
    Main application class, pulls together everything and runs it.
    """
    def __init__(self):
        """
        Initialize
        """
        self.config       = None
        self.packageStore = None
        self.ideviceStore = None
        self.packagePath  = None
        self.webServer    = None
        self.standalone   = False # Used for the ready to run exe
        self.persistNonPersistants = False  
        self.tempWebDir   = mkdtemp('.eXe')
        self.afterUpgradeHandlers = []
        assert globals.application is None, "You tried to instantiate two Application objects"
        globals.application = self
    def main(self):
        """
        Main function, starts eXe
        """
        self.processArgs()
        self.loadConfiguration()
        eXeStart = os.path.join(
                os.path.dirname(globals.application.tempWebDir),
                'tmpExeStartupTime')
        if os.name == 'posix':
            eXeStart = eXeStart + '.' + str(os.getuid())
        log.debug(u'eXeStart: ' + eXeStart)
        if os.path.exists(eXeStart):
            inStartFH=open(eXeStart, "r")
            lastRunTimeS = 0
            for line in inStartFH:
                lastRunTimeS = int(line)
            inStartFH.close()
            log.debug('lastRunTimeS: ' + `lastRunTimeS`)
            currentTime = int (time.time())
            currentTime2 = int (time.time())
            log.info('currentTime: ' + `currentTime`)
            if(currentTime <= lastRunTimeS + 3 and currentTime >= lastRunTimeS):
                return None
        if sys.platform[:3] == "win" and self.packagePath is not None:
            self.packagePath = self.config.getLongPathName(self.packagePath)
        installSafeTranslate()
        self.preLaunch()
        if self.config.port >= 0:
            self.launch()
            log.info('serving')
            self.serve()
            log.info('done serving')
        else:
            log.error('eXe appears to already be running')
            log.error('looks like the eXe server was not able to find a valid port; terminating...')
        shutil.rmtree(self.tempWebDir, True)
        try:
            os.remove(eXeStart)
        except IOError:
            pass
    def processArgs(self):
        """
        Processes the command line arguments
        """
        try:
            options, packages = getopt(sys.argv[1:], 
                                       "hV", ["help", "version", "standalone"])
        except GetoptError:
            self.usage()
            sys.exit(2)
        if len(packages) == 1:
            self.packagePath = packages[0]
        elif len(packages) > 1:
            self.usage()
            sys.exit(2)
        for option in options:
            if option[0] in ("-V", "--version"):
                print "eXe", version.version
                sys.exit()
            elif option[0] in ("-h", "--help"):
                self.usage()
                sys.exit()
            elif option[0].lower() == '--standalone':
                self.standalone = True
    def loadConfiguration(self):
        """
        Loads the config file and applies all the settings
        """
        if self.standalone:
            from exe.engine.standaloneconfig import StandaloneConfig
            self.config = StandaloneConfig()
        elif sys.platform[:3] == "win":
            from exe.engine.winconfig import WinConfig
            self.config = WinConfig()
        elif sys.platform[:6] == "darwin":
            from exe.engine.macconfig import MacConfig
            self.config = MacConfig()
        else:
            from exe.engine.linuxconfig import LinuxConfig
            self.config = LinuxConfig()
        log.debug("logging set up")
    def preLaunch(self):
        """
        Sets ourself up for running 
        Needed for unit tests
        """
        log.debug("preLaunch")
        self.packageStore = PackageStore()
        self.ideviceStore = IdeviceStore(self.config)
        self.ideviceStore.load()
        sys.path.append(self.config.configDir/'idevices')
        self.webServer = WebServer(self)
        self.webServer.find_port()
    def serve(self):
        """
        Starts the web server,
        this func doesn't return until after the app has finished
        """
        print "Welcome to eXe: the eLearning XHTML editor"
        log.info("eXe running...")
        self.webServer.run()
    def _loadPackage(self, packagePath):
        try:
            log.info("webDir: " + self.config.webDir)
            log.info("tempWebDir: " + self.tempWebDir)
            inSplashFile =  self.config.webDir + "/docs/splash.xulTemplate"
            outSplashFile = self.config.webDir + "/docs/splash.xul"
            outSplashData = self.config.webDir + "/docs/splash.dat"
            outSplashFile = self.tempWebDir + "/splash.xul"
            outSplashData = self.tempWebDir + "/splash.dat"
            log.info("inSplashFile: " + inSplashFile)
            log.info("outSplashFile: " + outSplashFile)
            log.info("outSplashData: " + outSplashData)
            outSplashFH = open(outSplashData, "w")
            outSplashFH.write("")
            outSplashFH.close()
            inSplashFH = open(inSplashFile, "r")
            outSplashFH = open(outSplashFile, "w")
            pleaseWaitLoad = _(u'Please wait until loading finishes')
            for line in inSplashFH:
                line = line.replace("LOADING_FILE_NAME", packagePath)
                line = line.replace("PLEASE_WAIT_LOAD", pleaseWaitLoad)
                outSplashFH.write(line)
            inSplashFH.close()
            outSplashFH.close()
            log.info("packagePath: " + packagePath)
            launchBrowser(self.config, outSplashFile, "splash")
            shutil.copyfile(self.config.webDir + '/images/exe_logo.png', 
                                      self.tempWebDir + '/exe_logo.png')
            package = self.packageStore.loadPackage(packagePath)
            port = self.config.port
            editorUrl = u'http://127.0.0.1:%d/%s' % (port, package.name)
            log.info("package.name: "+package.name)
            log.info("editorUrl: " + editorUrl)
            log.info("TempDirPath: " + editorUrl)
            outSplashFH = open(outSplashData, "w")
            outSplashFH.write("1000;" + editorUrl)
            outSplashFH.close()
            self.webServer.root.bindNewPackage(package)
            return package
        except Exception, e:
            log.error('Error loading first Package (%s): %s' % (packagePath, e))
            message = _(u'Sorry, wrong file format')
            outSplashFH=open(globals.application.tempWebDir + \
                               '/splash.dat',"w")
            message = re.sub(";",":",message)
            port = self.config.port
            outSplashFH.write("1000;http://127.0.0.1:" + `port` + "/;" + \
                               message)
            outSplashFH.close()
            return None
    def xulMessage(self, msg):
        """
        launches the web browser and displays a message 
        without the need of a running/responding webserver
        """
        inXulMsgFile =  self.config.webDir + "/docs/xulMsg.xulTemplate"
        outXulMsgFile = self.tempWebDir + "/xulMsg.xul"
        log.info("outXulMsgFile: " + outXulMsgFile)
        log.info("xulMessage: " + msg)
        inXulMsgFH = open(inXulMsgFile, "r")
        outXulMsgFH = open(outXulMsgFile, "w")
        for line in inXulMsgFH:
            line = re.sub("XUL_MESSAGE", msg, line)
            outXulMsgFH.write(line)
        inXulMsgFH.close()
        outXulMsgFH.close()
        launchBrowser(self.config, outXulMsgFile, "xulMsg")
        shutil.copyfile(self.config.webDir + '/images/exe_logo.png', self.tempWebDir + '/exe_logo.png')
        time.sleep(3)
    def launch(self):
        """
        launches the webbrowser
        """
        package = None
        if self.packagePath:
            package = self._loadPackage(self.packagePath)
        else:
            launchBrowser(self.config, "", "")
    def usage(self):
        """
        Print usage info
        """
        print "Usage: "+os.path.basename(sys.argv[0])+" [OPTION] [PACKAGE]"
        print "  -V, --version    print version information and exit"
        print "  -h, --help       display this help and exit"
        print "  --standalone     Run totally from current directory"
        print "Settings are read from exe.conf "
        print "in $HOME/.exe on Linux/Unix or"
        print "in Documents and Settings/<user name>/Application Data/exe "
        print "on Windows"
