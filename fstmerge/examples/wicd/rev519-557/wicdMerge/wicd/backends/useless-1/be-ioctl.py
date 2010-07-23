""" ioctl Network interface control tools for wicd.
This module implements functions to control and obtain information from
network interfaces.  It utilizes ioctl calls and python modules to
obtain this information whenever possible.
def SetDNS() -- Set the DNS servers of the system.
def GetWirelessInterfaces() -- Get the wireless interfaces available.
class Interface() -- Control a network interface.
class WiredInterface() -- Control a wired network interface.
class WirelessInterface() -- Control a wireless network interface.
"""

import wicd.misc as misc

import wicd.wnettools as wnettools

import wicd.wpath as wpath

import iwscan

import wpactrl

import re

import os

import time

import socket

import fcntl

import struct

import array

NAME = "ioctl"

DESCRIPTION = """IOCTL (fast) backend
This backend uses IOCTL calls and python libraries to query
network information whenever possible.  This makes it fast,
but it may not work properly on all systems.
Dependencies:
python-wpactrl (http://projects.otaku42.de/wiki/PythonWpaCtrl)
python-iwscan (http://projects.otaku42.de/browser/python-iwscan/)"""

strength_pattern = re.compile('.*Quality:?=? ?(\d+)\s*/?\s*(\d*)', 
                              re.I | re.M  | re.S)

altstrength_pattern = re.compile('.*Signal level:?=? ?(\d\d*)',
                                 re.I | re.M | re.S)

signaldbm_pattern = re.compile('.*Signal level:?=? ?(-\d\d*)',
                               re.I | re.M | re.S)

wep_pattern = re.compile('.*Encryption key:(.*?)\n', re.I | re.M  | re.S)

RALINK_DRIVER = 'ralink legacy'

SIOCGIWESSID = 0x8B1B

SIOCGIWRANGE = 0x8B0B

SIOCGIWAP = 0x8B15

SIOCGIWSTATS = 0x8B0F

SIOCGIFADDR = 0x8915

SIOCGIFHWADDR = 0x8927

SIOCGMIIPHY = 0x8947

SIOCETHTOOL = 0x8946

SIOCGIFFLAGS = 0x8913





















