import os, sys
import gtk.glade
from dbusmanager import daemon, interface
from dbusmanager import set, get, do
import logging
sys.path.insert(0, os.path.abspath(
    os.path.join(os.path.dirname(
        os.path.realpath(__file__)
        ),
                 '../..')))
logging.debug(sys.path[0])
from gtkuibase import ShortInterfaceUiBase, SettingsDialogBox
sys.path.pop(0)
class WirelessNetworkEntry(object):
    def __init__(self, bssid, interface):
        self.interface = interface
        self.bssid = bssid
        cwd = os.getcwd()
        os.chdir(os.path.split(__file__)[0])
        widgets = gtk.glade.XML("wireless.glade", root='entry_hbox')
        box = widgets.get_widget('entry_hbox')
        box.widgets = widgets
        os.chdir(cwd)
        self.box = box
        logging.debug('in wireless network entry init')
        self._load_widgets(['strength_bar',
                            'essid_label',
                            'strength_label',
                            'mac_label',
                            'enc_label',
                            'connect_button',
                            'properties_button'])
        dic = {'on_properties_button_clicked' : self.properties_clicked,
               'on_connect_button_clicked' : self.connect_clicked}
        widgets.signal_autoconnect(dic)
        self._load_values()
    def properties_clicked(self, widget):
        settings_dialog = SettingsDialogBox(self.bssid, self.interface)
        settings_dialog.settings_dialog.show()
    def connect_clicked(self, widget):
        self.interface.do_connect(self.bssid)
    def _load_values(self):
        strength_decimal = int(self.interface.get_network_information(
            self.bssid,
            'quality')) / 100.0
        strength_percent = self.interface.get_network_information(
            self.bssid,
            'quality')
        essid = self.interface.get_network_information(self.bssid, 'essid')
        bssid = self.interface.get_network_information(self.bssid, 'bssid')
        self.strength_label.set_text('%s%%' % strength_percent)
        self.essid_label.set_markup('<b>%s</b>' % essid)
        self.mac_label.set_text(bssid)
        self.strength_bar.set_fraction(strength_decimal)
    def _load_widgets(self, widget_list):
        for item in widget_list:
            setattr(self, item, self.box.widgets.get_widget(item))
class WirelessNetworkListWindow:
    def __init__(self, interface):
        filedir = os.path.split(__file__)[0]
        filepath = os.path.join(filedir, 'network_window.glade')
        widgets = gtk.glade.XML(filepath)
        window = widgets.get_widget('network_window')
        network_vbox = widgets.get_widget('network_vbox')
        window.widgets = widgets
        self.settings_dialog = window
        logging.debug(interface.get_networks())
        for bssid, essid in interface.get_networks().iteritems():
            logging.debug('%s: %s', bssid, essid)
            entry = WirelessNetworkEntry(bssid, interface)
            network_vbox.pack_start(entry.box, fill=False, expand=False)
        window.show_all()
class WirelessShortInterfaceUi(ShortInterfaceUiBase):
    def __init__(self, interface):
        ShortInterfaceUiBase.__init__(self, interface)
        self.image.set_from_icon_name('network-wireless', 6)
        self.settings_dialog_class = WirelessNetworkListWindow
