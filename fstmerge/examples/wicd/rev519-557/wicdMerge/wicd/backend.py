""" Backend manager for wicd.
Manages and loads the pluggable backends for wicd.
"""

import sys

import os

from logfile import log

import wicd.wpath as wpath

from baseinterface import BaseInterface



if __name__ == "__main__":

    log( "main")

    be = BackendManager()

    log( be.get_available_backend_modules() )

    be.load_all_available_backends()

    log( be.get_loaded_backends())



