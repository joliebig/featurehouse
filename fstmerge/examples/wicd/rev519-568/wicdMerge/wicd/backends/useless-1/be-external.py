""" Network interface control tools for wicd.
This module implements functions to control and obtain information from
network interfaces.
def SetDNS() -- Set the DNS servers of the system.
def GetWirelessInterfaces() -- Get the wireless interfaces available.
class Interface() -- Control a network interface.
class WiredInterface() -- Control a wired network interface.
class WirelessInterface() -- Control a wireless network interface.
"""

import wicd.misc as misc

import wicd.wnettools as wnettools

import re

import os

import wicd.wpath as wpath

import time

NAME = "external"

DESCRIPTION = """External app (slow) backend
This backend uses external program calls like ifconfig and
iwconfig to query network information.  This makes it a bit
slower and more CPU intensive than the ioctl backend, but
it doesn't require any thirdy party libraries and may be
more stable for some set ups.
"""

essid_pattern       = re.compile('.*ESSID:"(.*?)"\n', re.I | re.M  | re.S)

ap_mac_pattern      = re.compile('.*Address: (.*?)\n', re.I | re.M  | re.S)

channel_pattern     = re.compile('.*Channel:? ?(\d\d?)', re.I | re.M  | re.S)

strength_pattern    = re.compile('.*Quality:?=? ?(\d+)\s*/?\s*(\d*)', re.I | re.M  | re.S)

altstrength_pattern = re.compile('.*Signal level:?=? ?(\d\d*)', re.I | re.M | re.S)

signaldbm_pattern   = re.compile('.*Signal level:?=? ?(-\d\d*)', re.I | re.M | re.S)

mode_pattern        = re.compile('.*Mode:(.*?)\n', re.I | re.M  | re.S)

freq_pattern        = re.compile('.*Frequency:(.*?)\n', re.I | re.M  | re.S)

ip_pattern          = re.compile(r'inet [Aa]d?dr[^.]*:([^.]*\.[^.]*\.[^.]*\.[0-9]*)', re.S)

bssid_pattern       = re.compile('.*Access Point: (([0-9A-Z]{2}:){5}[0-9A-Z]{2})', re.I | re.M | re.S)

wep_pattern         = re.compile('.*Encryption key:(.*?)\n', re.I | re.M  | re.S)

altwpa_pattern      = re.compile('(wpa_ie)', re.I | re.M | re.S)

wpa1_pattern        = re.compile('(WPA Version 1)', re.I | re.M  | re.S)

wpa2_pattern        = re.compile('(WPA2)', re.I | re.M  | re.S)

auth_pattern        = re.compile('.*wpa_state=(.*?)\n', re.I | re.M  | re.S)

RALINK_DRIVER = 'ralink legacy'



















