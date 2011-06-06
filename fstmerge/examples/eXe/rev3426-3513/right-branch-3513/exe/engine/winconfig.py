"""
The WinConfig overrides the Config class with Windows specific
configuration
"""
from exe.engine.config import Config
from exe.engine.path   import Path
APPDATA        = 0x001a
COMMON_APPDATA = 0x0023
MYDOCUMENTS    = 0x0005 # Code for c:\documents and settings\myuser\My Documents
PROGRAMFILES   = 0x0026
class WinConfig(Config):
    """
    The WinConfig overrides the Config class with Windows specific
    configuration
    """
    def _overrideDefaultVals(self):
        """Sets the default values
        for windows"""
        exeDir = self.exePath.dirname()
        self.browserPath = exeDir/'Mozilla Firefox'/'firefox.exe'
        if not self.browserPath.isfile():
            programFiles = Path(self.__getWinFolder(PROGRAMFILES))
            self.browserPath = programFiles/'Mozilla Firefox'/'firefox.exe'
        self.dataDir   = Path(self.__getWinFolder(MYDOCUMENTS))
        self.configDir = Path(self.__getWinFolder(APPDATA))/'exe'
    def _getConfigPathOptions(self):
        """
        Returns the best options for the
        location of the config file under windows
        """
        folders = map(self.__getWinFolder, [APPDATA, COMMON_APPDATA])
        folders = [folder/'exe' for folder in folders] 
        folders.append(self.__getInstallDir())
        folders.append('.')
        options = [folder/'exe.conf' for folder in map(Path, folders)]
        return options
    def __getWinFolder(self, code):
        """
        Gets one of the windows famous directorys
        depending on 'code'
        Possible values can be found at:
        http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/shell/reference/enums/csidl.asp#CSIDL_WINDOWS
        """
        from ctypes import WinDLL, create_unicode_buffer
        dll = WinDLL('shell32')
        result = create_unicode_buffer(260)
        resource = dll.SHGetFolderPathW(None, code, None, 0, result)
        if resource != 0: 
            return Path('')
        else: 
            return Path(result.value)
    def __getInstallDir(self):
        """
        Returns the path to where we were installed
        """
        from _winreg import OpenKey, QueryValue, HKEY_LOCAL_MACHINE
        try:
            exeKey = None
            softwareKey = None
            try:
                softwareKey = OpenKey(HKEY_LOCAL_MACHINE, 'SOFTWARE')
                exeKey = OpenKey(softwareKey, 'exe')
                return Path(QueryValue(exeKey, ''))
            finally:
                if exeKey:
                    exeKey.Close()
                if softwareKey:
                    softwareKey.Close()
        except WindowsError:
            return Path('')
    def getLongPathName(self, path):
        """
        Convert from Win32 short pathname to long pathname
        """
        from ctypes import windll, create_unicode_buffer
        buf = create_unicode_buffer(260)
        r = windll.kernel32.GetLongPathNameW(unicode(path), buf, 260)
        if r == 0 or r > 260:
            return path
        else:
            return buf.value
