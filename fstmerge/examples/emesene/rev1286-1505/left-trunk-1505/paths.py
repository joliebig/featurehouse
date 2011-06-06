import os
import sys
DIR_SEP = os.sep
if hasattr(sys, "frozen"):
    APP_PATH = os.path.dirname(sys.executable)
else:
    APP_PATH = os.path.abspath(os.path.dirname(__file__))
HOME_DIR = os.path.expanduser('~')
CONF_DIR_NAME = '.config' + DIR_SEP + 'emesene1.0'
CONFIG_DIR = HOME_DIR + DIR_SEP + CONF_DIR_NAME
THEME_HOME_PATH = CONFIG_DIR + DIR_SEP + 'themes'
THEME_SYSTEM_WIDE_PATH = APP_PATH + DIR_SEP + 'themes'
DEFAULT_THEME_PATH = THEME_SYSTEM_WIDE_PATH + DIR_SEP + 'default' + DIR_SEP
PLUGINS_HOME = 'pluginsEmesene'
PLUGINS_SYSTEM_WIDE = 'plugins_base'
PLUGIN_SYSTEM_WIDE_PATH = APP_PATH + DIR_SEP + PLUGINS_SYSTEM_WIDE
PLUGIN_HOME_PATH = CONFIG_DIR + DIR_SEP + PLUGINS_HOME
SMILIES_SYSTEM_WIDE_PATH = APP_PATH + DIR_SEP + 'smilies'
SMILIES_HOME_PATH = CONFIG_DIR + DIR_SEP + 'smilies'
DEFAULT_SMILIES_PATH = SMILIES_SYSTEM_WIDE_PATH + DIR_SEP + 'default' + DIR_SEP
CONVTHEMES_SYSTEM_WIDE_PATH = APP_PATH + DIR_SEP + 'conversation_themes'
CONVTHEMES_HOME_PATH = CONFIG_DIR + DIR_SEP + 'conversation_themes'
DEFAULT_CONVTHEMES_PATH = CONVTHEMES_SYSTEM_WIDE_PATH + DIR_SEP + 'default' + DIR_SEP
LANG_PATH = APP_PATH + DIR_SEP + 'po'
SOUNDS_PATH = APP_PATH + DIR_SEP + 'sound_themes'
del os, sys
