""" Backend manager for wicd.
Manages and loads the pluggable backends for wicd.
"""

import sys

import os

import logging

import wicd.wpath as wpath

from baseinterface import BaseInterface



if __name__ == "__main__":

    logging.debug( "main")

    be = BackendManager()

    logging.debug( be.get_available_backend_modules() )

    be.load_all_available_backends()

    logging.debug( be.get_loaded_backends())



