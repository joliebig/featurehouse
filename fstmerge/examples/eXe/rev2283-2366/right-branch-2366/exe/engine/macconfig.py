"""
The MacConfig overrides the Config class with Mac specific
configuration
"""
import os
from exe.engine.linuxconfig import LinuxConfig
from exe.engine.path import Path
class MacConfig(LinuxConfig):
    """
    The MacConfig overrides the Config class with Mac specific
    configuration. We use the same _getConfigPathOptions as LinuxConfig
    """
    def _overrideDefaultVals(self):
        """
        Sets default mac values. Uses LinuxConfig's _getConfigPathOptions.
        """
        self.webDir      = Path("../Resources/exe")
        self.xulDir      = Path("../Resources/exe")
        self.localeDir   = Path("../Resources/exe/locale")
        self.dataDir     = Path(os.environ['HOME'])
        self.configDir   = Path(self.dataDir)/'.exe'
        self.browserPath = self.webDir/'firefox/Firefox.app/Contents/MacOS/firefox'
