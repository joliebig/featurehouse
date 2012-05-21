""" wicd - wireless connection daemon implementation.
This module implements the wicd daemon that provides network
connection management, for both wireless and wired networks. The daemon
must be run as root to control the networks, however the user interface
components should be run as a normal user.
class WicdDaemon -- The main DBus daemon for Wicd.
def usage() -- Print usage information.
def daemonize() -- Daemonize the current process with a double fork.
def main() -- The wicd daemon main loop.
"""
import logging
from configmanager import ConfigManager
global global_config, privileged_config
global plugin_manager
global daemon
if not 'daemon' in globals():
    daemon = None
if not 'plugin_manager' in globals():
    plugin_manager = None
if not 'global_config' in globals():
    logging.debug('global_config does not exist, creating...')
    global_config = ConfigManager('/etc/wicd/global.conf')
if not 'privileged_config' in globals():
    logging.debug('privileged_config does not exist, creating...')
    privileged_config = ConfigManager('/etc/wicd/privileged.conf',
                                      privileged=True)
