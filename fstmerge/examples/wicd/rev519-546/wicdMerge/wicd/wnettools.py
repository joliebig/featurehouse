""" Network interface control tools for wicd.
This module implements functions to control and obtain information from
network interfaces.
def SetDNS() -- Set the DNS servers of the system.
def GetWirelessInterfaces() -- Get the wireless interfaces available.
class Interface() -- Control a network interface.
class WiredInterface() -- Control a wired network interface.
class WirelessInterface() -- Control a wireless network interface.
"""

import os

import time

import re

import wpath

import misc

RALINK_DRIVER = 'ralink legacy'





























~~FSTMerge~~ alt_essid_pattern   = re.compile('.*ESSID:(.*?)\n', re.I | re.M | re.S) ##FSTMerge## alt_essid_pattern   = re.compile('.*ESSID:(.*?)"\n', re.I | re.M | re.S) ##FSTMerge##













































