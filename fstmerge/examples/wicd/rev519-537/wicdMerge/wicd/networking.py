""" networking - Provides wrappers for common network operations
This module provides wrappers of the common network tasks as well as
threads to perform the actual connecting to networks.
class Controller() -- Parent class to Wireless and Wired
class ConnectThread() -- Parent class to WirelessConnectThread and
    WiredConnectThread
class Wireless() -- Wrapper for various wireless functions
class Wired() -- Wrapper for various wired functions
class WirelessConnectThread() -- Connection thread for wireless
    interface
class WiredConnectThread() -- Connection thread for wired
    interface
"""

import re

import time

import threading

import thread

import misc

import wpath

from backend import BackendManager

if __name__ == '__main__':

    wpath.chdir(__file__)



BACKEND = None

BACKEND_MGR = BackendManager()























if __name__ == '__main__':

    wpath.chdir(__file__)



