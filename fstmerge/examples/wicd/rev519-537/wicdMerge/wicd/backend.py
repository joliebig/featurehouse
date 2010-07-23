""" Backend manager for wicd.
Manages and loads the pluggable backends for wicd.
"""

import sys

import os

import wicd.wpath as wpath

from baseinterface import BaseInterface



if __name__ == "__main__":

    print "main"

    be = BackendManager()

    print be.get_available_backend_modules()

    be.load_all_available_backends()

    print be.get_loaded_backends()



