'''
[Home]
test0 = wireless, get_networks, 00:00:00:00:00:00
test0 = wireless, get_networks, MyRandomAccessPointESSID
action0 = wired, set_current_profile, MyHomeWiredNetworkProfileName
[SomewhereElse]
test0 = wireless, get_networks, "ALargeNetworkWithManyAPsESSID"
action0 = wired, set_current_profile, "BigPlace"
'''

from baseplugin import BasePlugin

from configmanager import ConfigManager

from misc import WicdError

from logfile import log

global get_all_by_type

get_all_by_type = None















