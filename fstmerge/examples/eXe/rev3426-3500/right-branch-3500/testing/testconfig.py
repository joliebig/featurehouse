from exe.engine.config import Config
import logging
from logging import DEBUG, INFO, WARNING, ERROR, CRITICAL
import unittest, sys
from exe.engine.configparser import ConfigParser
from exe.engine.path import Path
import utils
class TestConfig(utils.SuperTestCase):
    def _setupConfigFile(self, configParser):
        """
        Setup our own config file
        """
        utils.SuperTestCase._setupConfigFile(self, configParser)
        tmp = Path('tmp')
        if not tmp.exists():
            tmp.mkdir()
        logfn = tmp/'exe.log'
        if logfn.exists():
            try:
                logfn.remove()
            except OSError:
                pass
        configParser.system.configDir = tmp
        configParser.logging.root = 'ERROR'
        configParser.logging.foo = 'DEBUG'
        configParser.logging.foo = 'DEBUG'
    def testSetupLogging(self):
        """
        Tests that the correct logging directory is made
        """
        Config._getConfigPathOption  = lambda s: ['test.conf']
        rootLog = logging.getLogger()
        fooLog  = logging.getLogger("foo")
        barLog  = logging.getLogger("bar")
        self.assertEqual(fooLog.level,  DEBUG)
        self.assertEqual(rootLog.level, ERROR)
        rootLog.debug("This")
        rootLog.warning("is free")
        rootLog.error("software")
        rootLog.critical("you can")
        fooLog.debug("distribute")
        fooLog.info("is free")
        fooLog.error("and/or modify")
        barLog.debug("Temple Place")
        barLog.warning("Boston")
        barLog.error("Massachusetts")
        results = ["root ERROR software", "root CRITICAL you can", 
                   "foo DEBUG distribute", "foo INFO is free", 
                   "foo ERROR and/or modify", "bar ERROR Massachusetts"]
        resultFile = open("tmp/exe.log")
        i = 0
        for line in resultFile.readlines():
            self.assertEqual(line[24:].strip(), results[i])
            i += 1
    def testUpgradeAppDir(self):
        """
        Tests that config files with
        'appDataDir' are upgraded to 'configDir'
        """
        configPath = Path(u'test.exe.conf')
        if configPath.exists():
            configPath.remove()
        oldParser = ConfigParser()
        system = oldParser.addSection('system')
        system.appDataDir = 'my old app data dir'
        oldParser.write(configPath)
        del system
        del oldParser
        Config._getConfigPathOptions = lambda self: ['test.exe.conf']
        myconfig = Config()
        myconfig.loadSettings()
        assert not hasattr(myconfig, 'appDataDir')
        self.assertEquals(myconfig.configPath, 'test.exe.conf')
        self.assertEquals(myconfig.configDir, 'my old app data dir')
        newParser = ConfigParser()
        newParser.read(configPath)
        self.assertEquals(newParser.system.configDir, 'my old app data dir')
if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(TestConfig))
    unittest.TextTestRunner(verbosity=2).run(suite)
