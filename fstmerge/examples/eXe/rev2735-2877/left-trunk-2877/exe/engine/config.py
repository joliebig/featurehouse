"""
Config settings loaded from exe.conf
Is responsible for the system-wide settings we use
O/S specific config classes are derieved from here
"""
from exe.engine.configparser import ConfigParser
from exe.engine.path import Path
from exe.engine.locales import chooseDefaultLocale
import logging
from logging.handlers import RotatingFileHandler
import sys
import os
import gettext
import tempfile
class Config:
    """
    The Config class contains the configuration information for eXe.
    """
    optionNames = {
        'system': ('webDir', 'xulDir', 'port', 'dataDir', 
                   'configDir', 'localeDir', 'browserPath'),
        'user': ('locale',),
    }
    def __init__(self):
        """
        Initialise
        """
        self.configPath = None
        self.configParser = ConfigParser(self.onWrite)
        self.exePath     = Path(sys.argv[0]).abspath()
        self.webDir      = self.exePath.dirname()
        self.xulDir      = self.exePath.dirname()
        self.localeDir   = self.exePath.dirname()/"locale"
        self.port        = 51235
        self.dataDir     = Path(".")
        self.configDir   = Path(".")
        self.browserPath = Path("firefox")
        self.locale = chooseDefaultLocale(self.localeDir)
        self.styles      = []
        self.recentProjects = []
        self.hiddeniDevices = []
        self._overrideDefaultVals()
        self.webDir = Path(self.webDir)
        if not (self.webDir/'scripts').isdir() \
           and (self.webDir/'webui').isdir():
            self.webDir /= 'webui'
        self.xulDir = Path(self.xulDir)
        if not (self.xulDir/'scripts').isdir() \
           and (self.xulDir/'xului').isdir():
            self.xulDir /= 'xului'
        self.__setConfigPath()
        self._writeDefaultConfigFile()
        self.loadSettings()
        self.setupLogging()
        self.loadStyles()
        self.loadLocales()
    def _overrideDefaultVals(self):
        """
        Override this to override the
        default config values
        """
    def _getConfigPathOptions(self):
        """
        Override this to give a list of
        possible config filenames
        in order of preference
        """
        return ['exe.conf']
    def _writeDefaultConfigFile(self):
        """
        [Over]writes 'self.configPath' with a default config file 
        (auto write is on so we don't need to write the file at the end)
        """
        for sectionName, optionNames in self.optionNames.items():
            for optionName in optionNames:
                defaultVal = getattr(self, optionName)
                self.configParser.setdefault(sectionName, 
                                             optionName, 
                                             defaultVal)
        self.configParser.setdefault('logging', 'root', 'INFO')
    def __setConfigPath(self):
        """
        sets self.configPath to the filename of the config file that we'll
        use.
        In descendant classes set self.configFileOptions to a list
        of directories where the configDir should be in order of preference.
        If no config files can be found in these dirs, it will
        force creation of the config file in the top dir
        """
        self.configPath = None
        configFileOptions = map(Path, self._getConfigPathOptions())
        if "EXECONF" in os.environ:
            envconf = Path(os.environ["EXECONF"])
            if envconf.isfile():
                self.configPath = os.environ["EXECONF"]
        if self.configPath is None:
            for confPath in configFileOptions:
                if confPath.isfile():
                    self.configPath = confPath
                    break
            else:
                self.configPath = configFileOptions[0]
                folder = self.configPath.abspath().dirname()
                if not folder.exists():
                    folder.makedirs()
                self.configPath.touch()
        self.configParser.read(self.configPath)
        self.configParser.autoWrite = True
    def upgradeFile(self):
        """
        Called before loading the config file,
        removes or upgrades any old settings.
        """
        if self.configParser.has_section('system'):
            system = self.configParser.system
            if system.has_option('appDataDir'):
                self.configDir = Path(system.appDataDir)
                system.configDir = self.configDir
                del system.appDataDir
            if system.has_option('greDir'):
                del system.greDir
    def loadSettings(self):
        """
        Loads the settings from the exe.conf file.
        Overrides the defaults set in __init__
        """
        def defVal(dummy, option):
            """If something is not in the config file, just use the default in
            'self'"""
            return getattr(self, option)
        self.configParser.defaultValue = defVal
        self.upgradeFile()
        if self.configParser.has_section('system'):
            system = self.configParser.system
            self.webDir         = Path(system.webDir)
            self.xulDir         = Path(system.xulDir)
            self.localeDir      = Path(system.localeDir)
            self.port           = int(system.port)
            self.browserPath    = Path(system.browserPath)
            self.dataDir        = Path(system.dataDir)
            self.configDir      = Path(system.configDir)
        if not self.dataDir.isdir():
            self.dataDir = tempfile.gettempdir()
        self.webDir = self.webDir.expand().abspath()
        if not self.configDir.exists():
            self.configDir.mkdir()
        self.recentProjects = []
        if self.configParser.has_section('recent_projects'):
            recentProjectsSection = self.configParser.recent_projects
            for key, path in recentProjectsSection.items():
                self.recentProjects.append(path)
        self.hiddeniDevices = []
        if self.configParser.has_section('idevices'):
            idevicesSection = self.configParser.idevices
            for key,value in idevicesSection.items():
                value = value.strip().lower()
                if value == "0" or value == "no" or value == "false" or \
                        value == "off":
                    self.hiddeniDevices.append(key.lower())
        if self.configParser.has_section('user'):
            if self.configParser.user.has_option('locale'):
                self.locale = self.configParser.user.locale
                return
        self.locale = chooseDefaultLocale(self.localeDir)
    def onWrite(self, configParser):
        """
        Called just before the config file is written.
        We use it to fill out any settings that are stored here and 
        not in the config parser itself
        """
        self.configParser.delete('recent_projects')
        recentProjectsSection = self.configParser.addSection('recent_projects')
        for num, path in enumerate(self.recentProjects):
            recentProjectsSection[str(num)] = path
    def setupLogging(self):
        """
        setup logging file
        """
        try:
            hdlr = RotatingFileHandler(self.configDir/'exe.log', 'a', 
                                       500000, 10)
            hdlr.doRollover()
        except OSError:
            hdlr = logging.FileHandler(self.configDir/'exe.log')
        format = "%(asctime)s %(name)s %(levelname)s %(message)s"
        log    = logging.getLogger()
        hdlr.setFormatter(logging.Formatter(format))
        log.addHandler(hdlr)
        loggingLevels = {"DEBUG"    : logging.DEBUG,
                         "INFO"     : logging.INFO,
                         "WARNING"  : logging.WARNING,
                         "ERROR"    : logging.ERROR,
                         "CRITICAL" : logging.CRITICAL }
        if self.configParser.has_section('logging'):
            for logger, level in self.configParser._sections["logging"].items():
                if logger == "root":
                    logging.getLogger().setLevel(loggingLevels[level])
                else:
                    logging.getLogger(logger).setLevel(loggingLevels[level])
        log.info("************** eXe logging started **************")
        log.info("configPath  = %s" % self.configPath)
        log.info("exePath     = %s" % self.exePath)
        log.info("browserPath = %s" % self.browserPath)
        log.info("webDir      = %s" % self.webDir)
        log.info("xulDir      = %s" % self.xulDir)
        log.info("localeDir   = %s" % self.localeDir)
        log.info("port        = %d" % self.port)
        log.info("dataDir     = %s" % self.dataDir)
        log.info("configDir   = %s" % self.configDir)
        log.info("locale      = %s" % self.locale)
    def loadStyles(self):
        """
        Scans the eXe style directory and builds a list of styles
        """
        log = logging.getLogger()
        self.styles = []
        styleDir    = self.webDir/"style"
        log.debug("loadStyles from %s" % styleDir)
        for subDir in styleDir.dirs():
            styleSheet = subDir/'content.css'
            log.debug(" checking %s" % styleSheet)
            if styleSheet.exists():
                style = subDir.basename()
                log.debug(" loading style %s" % style)
                self.styles.append(style)
    def loadLocales(self):
        """
        Scans the eXe locale directory and builds a list of locales
        """
        log = logging.getLogger()
        log.debug("loadLocales")
        gettext.install('exe', self.localeDir, True)
        self.locales = {}
        for subDir in self.localeDir.dirs():
            if (subDir/'LC_MESSAGES'/'exe.mo').exists():
                self.locales[subDir.basename()] = \
                    gettext.translation('exe', 
                                        self.localeDir, 
                                        languages=[str(subDir.basename())])
                if subDir.basename() == self.locale:
                    locale = subDir.basename()
                    log.debug(" loading locale %s" % locale)
                    self.locales[locale].install(unicode=True)
