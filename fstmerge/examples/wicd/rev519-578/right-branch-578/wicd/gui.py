""" Wicd GUI module.
Module containg all the code (other than the tray icon) related to the
Wicd user interface.
"""
import logging
logging.basicConfig(level=logging.DEBUG,
                    format='%(message)s',
                    datefmt='%Y%b%d %H:%M:%S')
logging.info('Wicd GUI initalizing...')
import os
import sys
import gtk
import gtk.glade
try:
    import pygtk
    pygtk.require("2.0")
except:
    pass
from wicd import wpath
from misc import rename_process
from dbusmanager import daemon
from dbus.exceptions import DBusException
from uimanager import UiManager
rename_process('wicd-gui')
class WicdGui(object):
    """ The main wicd GUI class. """
    def __init__(self, standalone=False):
        """ Initializes everything needed for the GUI. """
        self.wTree = gtk.glade.XML("guibase1.glade")
        dic = { "about_clicked" : self.display_about,
                "exit_clicked" : self.exit,
                "window_closed" : self.exit }
        self.wTree.signal_autoconnect(dic)
        if os.path.exists(wpath.etc + "wicd.png"):
            self.window.set_icon_from_file(wpath.etc + "wicd.png")
        self.window = self.wTree.get_widget("main_window")
        self.network_vbox = self.wTree.get_widget('network_list')
        self.ui_manager = UiManager(self.network_vbox)
        self.window.show()
        logging.info('Wicd GUI initalized.')
    def display_about(self, button=None):
        logging.info('Wicd version: %s' % daemon.GetVersion())
    def exit(self, button=None):
        logging.info('Exit.')
        sys.exit()
if __name__ == '__main__':
    app = WicdGui(standalone=True)
    gtk.main()
