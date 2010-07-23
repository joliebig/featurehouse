""" Wicd Preferences Dialog.
Displays the main settings dialog window for wicd.
"""

import gtk

import gobject

import pango

from wicd import misc

from wicd.misc import checkboxTextboxToggle, noneToBlankString

daemon = None

wireless = None

wired = None

language = misc.get_language_list_gui()





