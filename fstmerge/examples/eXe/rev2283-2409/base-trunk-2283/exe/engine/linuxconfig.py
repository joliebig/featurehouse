"""
The LinuxConfig overrides the Config class with Linux specific
configuration
"""
import os
from exe.engine.config import Config
from exe.engine.path import Path
class LinuxConfig(Config):
    """
    The LinuxConfig overrides the Config class with Linux specific
    configuration
    """
    def _overrideDefaultVals(self):
        """
        Setup with our default settings
        """
        self.webDir      = Path("/usr/share/exe")
        self.xulDir      = Path("/usr/share/exe")
        self.localeDir   = Path("/usr/share/exe/locale")
        self.dataDir     = Path(os.environ['HOME'])
        self.configDir   = Path(self.dataDir)/'.exe'
        browserPath      = self.webDir/'firefox'/'firefox'
        if browserPath.isfile():
            self.browserPath = browserPath
    def _getConfigPathOptions(self):
        """
        Returns the best places for a linux config file
        """
        return [Path(os.environ["HOME"])/'.exe/exe.conf',
                Path('/etc/exe/exe.conf'),
                self.webDir/'exe.conf']
