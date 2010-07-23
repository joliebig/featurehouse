""" Misc - miscellaneous functions for wicd """

import os

import locale

import gettext

import sys

from subprocess import Popen, STDOUT, PIPE, call

import subprocess

import commands

import wpath

import logging

from wglobals import global_config

if __name__ == '__main__':

    wpath.chdir(__file__)



class  WicdError (Exception) :
