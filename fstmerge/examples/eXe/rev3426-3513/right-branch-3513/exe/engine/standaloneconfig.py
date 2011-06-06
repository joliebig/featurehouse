"""
The StandAlone config overrides the Config class with Standalone specific
configuration
"""
import sys, os
from exe.engine.config import Config
from exe.engine.path import Path
class StandaloneConfig(Config):
    """
    The StandaloneConfig overrides the Config class with ready-to-run specific
    configuration
    """
    def _overrideDefaultVals(self):
        """
        Setup with our default settings
        """
        self.exePath = Path(sys.argv[0])
        if self.exePath.isfile():
            self.exePath = self.exePath.dirname()
        exePath = self.exePath
        self.webDir        = exePath
        self.dataDir       = exePath/'packages'
        if not self.dataDir.exists():
            self.dataDir.makedirs()
        self.configDir     = exePath/'config'
        self.localeDir     = exePath/'locale'
        self.xulrunnerPath = exePath/'xulrunner/xulrunner'
        self.styles        = []
        self.browserPath = exePath/'firefox'/'firefox.exe'
    def _getConfigPathOptions(self):
        """
        Returns the best places for a linux config file
        """
        return [self.configDir/'exe.conf']
