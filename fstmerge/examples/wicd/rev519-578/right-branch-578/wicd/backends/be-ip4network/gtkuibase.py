import os
import gtk.glade
from dbusmanager import dbus_manager
import logging
class SettingsDialogBox(object):
    def __init__(self, network_id, interface):
        self.interface = interface
        self.network_id = network_id
        cwd = os.getcwd()
        os.chdir(os.path.split(__file__)[0])
        widgets = gtk.glade.XML(
            "uibase.glade", root='settings_dialog'
            )
        self.settings_dialog = widgets.get_widget('settings_dialog')
        self.settings_dialog.widgets = widgets
        self._setup_treeview(widgets.get_widget('dns_treeview'))
        dic = {
            "on_add_clicked" : self.add_clicked,
            'on_remove_clicked' : self.remove_clicked,
            'on_ok_clicked' : self.ok_clicked,
            'on_cancel_clicked' : self.cancel_clicked,
            'on_apply_clicked' : self.apply_clicked,
            'checkbox_toggled' : self.toggle_checkbox
            }
        widgets.signal_autoconnect(dic)
        os.chdir(cwd)
        self.ip_textbox = widgets.get_widget('ip_entry')
        self.netmask_textbox = widgets.get_widget('netmask_entry')
        self.gateway_textbox = widgets.get_widget('gateway_entry')
        self.static_ip_checkbutton = widgets.get_widget('static_ip_checkbutton')
        self.static_dns_checkbutton = widgets.get_widget('static_dns_checkbutton')
        self._load_values()
        self.settings_dialog.set_title('%s: Settings' % self.interface.interface_name)
        self.toggle_checkbox(None)
    def _load_values(self):
        ip = self._load_value('static_ip')
        netmask = self._load_value('static_netmask')
        gateway = self._load_value('static_gateway')
        use_static_ip = self._load_value('use_static_ip', default=False)
        use_static_dns = self._load_value('use_static_dns', default=False)
        self.ip_textbox.set_text(ip)
        self.netmask_textbox.set_text(netmask)
        self.gateway_textbox.set_text(gateway)
        self.static_ip_checkbutton.set_active(use_static_ip)
        self.static_dns_checkbutton.set_active(use_static_dns)
    def _load_value(self, name, default=''):
        if self.interface.get_has_profile_property(self.network_id, name):
            value = self.interface.get_profile_property(self.network_id, name)
            logging.debug('%s is: %s', name, value)
            return value
        else:
            logging.debug('no %s found', name)
            return default
    def add_clicked(self, widget):
        logging.debug('add clicked')
        self.liststore.append(('IP Address', ))
    def apply_clicked(self, widget):
        self.save_settings()
    def close_dialog(self):
        self.settings_dialog.hide()
        self.settings_dialog.destroy()
    def cancel_clicked(self, widget):
        self.close_dialog()
    def remove_clicked(self, widget):
        selection = self.treeview.get_selection()
        model, iter_ = selection.get_selected()
        if iter_:
            model.remove(iter_)
    def _cleanse_value(self, value):
        if value == '':
            return None
        else:
            return value
    def _run_if_not_none(self, method, value, args=[], cleanse=False):
        args = list(args)
        if cleanse:
            value = self._cleanse_value(value)
        if not value is None:
            args.append(value)
            method(*args)
    def save_settings(self):
        ip = self.ip_textbox.get_text()
        netmask = self.netmask_textbox.get_text()
        gateway = self.gateway_textbox.get_text()
        use_static_ip = self.static_ip_checkbutton.get_active()
        use_static_dns = self.static_dns_checkbutton.get_active()
        self._run_if_not_none(self.interface.set_profile_property,
                              use_static_ip,
                              (self.network_id, 'use_static_ip', ))
        self._run_if_not_none(self.interface.set_profile_property,
                              ip,
                              (self.network_id, 'static_ip', ),
                              cleanse=True)
        self._run_if_not_none(self.interface.set_profile_property,
                              netmask,
                              (self.network_id, 'static_netmask', ),
                              cleanse=True)
        self._run_if_not_none(self.interface.set_profile_property,
                              gateway,
                              (self.network_id, 'static_gateway', ),
                              cleanse=True)
        self._run_if_not_none(self.interface.set_profile_property,
                              use_static_dns,
                              (self.network_id, 'use_static_dns', ))
    def ok_clicked(self, widget):
        self.save_settings()
        self.close_dialog()
    def _setup_treeview(self, treeview):
        self.liststore = gtk.ListStore(str)
        treeview.set_model(self.liststore)
        cell = gtk.CellRendererText()
        cell.set_property('editable', True)
        column = gtk.TreeViewColumn('IP address', cell, text=0)
        treeview.append_column(column)
        self.treeview = treeview
        cell.connect('edited', self.edited_cell, self.liststore)
    def edited_cell(self, widget, position, new_text, model):
        logging.debug('cell edited: %s', new_text)
        model[position][0] = new_text
    def toggle_checkbox(self, widget):
        use_static_ip = self.static_ip_checkbutton.get_active()
        use_static_dns = self.static_dns_checkbutton.get_active()
        static_stuff = self.settings_dialog.widgets.get_widget('static_stuff')
        static_stuff.set_sensitive(use_static_ip)
        dns_align = self.settings_dialog.widgets.get_widget('dns_align')
        dns_align.set_sensitive(use_static_dns)
class ShortInterfaceUiBase(object): 
    def __init__(self, interface):
        cwd = os.getcwd()
        os.chdir(os.path.split(__file__)[0])
        widgets = gtk.glade.XML("uibase.glade", root='network_hbox')
        box = widgets.get_widget('network_hbox')
        box.widgets = widgets
        os.chdir(cwd)
        logging.debug(type(box))
        logging.debug(id(self))
        self.box = box
        logging.debug(id(self))
        logging.debug('in gtkuibase init')
        self.interface = interface
        self.image = widgets.get_widget('image')
        self.name_label = widgets.get_widget('name_label')
        self.status_label = widgets.get_widget('status_label')
        self.status_image = widgets.get_widget('status_image')
        self.disconnect_button = widgets.get_widget('disconnect_button')
        self.connect_button = widgets.get_widget('connect_button')
        self.cancel_button = widgets.get_widget('cancel_button')
        self.name_label.set_markup('<b>%s</b>' % self.interface.get_name())
        dic = { "on_connect_button_clicked" : self.connect_clicked,
                "on_disconnect_button_clicked" : self.disconnect_clicked,
                "on_settings_button_clicked" : self.settings_clicked,
                'on_cancel_button_clicked' : self.cancel_clicked}
        widgets.signal_autoconnect(dic)
        dbus_manager.connect_to_signal('StatusChange', self.status_change)
        self._update_status()
        logging.debug('done setting up gtkuibase %s', self.name_label)
    def connect_clicked(self, widget):
        logging.debug('connect clicked')
        self.interface.do_connect()
    def disconnect_clicked(self, widget):
        logging.debug('disconnect clicked')
        self.interface.do_disconnect()
    def settings_clicked(self, widget):
        settings_dialog = self.settings_dialog_class(self.interface)
        settings_dialog.settings_dialog.show()
    def cancel_clicked(self, widget):
        logging.debug('cancelclicked')
        self.interface.do_cancel_connect()
    def status_change(self, interface_name, previous_status, status):
        if not interface_name == self.interface.interface_name:
            return
        self._update_status()
    def state_change(self, interface_name, state=None):
        if not interface_name == self.interface.interface_name:
            return
        self._update_status()
    def _update_status(self):
        status = self.interface.get_internal_status()
        status_string = self.interface.get_status()
        connected = self.interface.get_connected_to_something()
        self.buttons_disabled(status)
        if status == 'idle':
            self.status_label.set_text(status_string)
        else:
            self.status_label.set_text(status.title())
        self.change_connected(connected, status)
    def buttons_disabled(self, status):
        enabled = None
        if status == 'idle':
            enabled = True
        else:
            enabled = False
        self.connect_button.set_sensitive(enabled)
        self.disconnect_button.set_sensitive(enabled)
    def change_connected(self, connected, status):
        if connected and status == 'idle':
            self.status_image.set_from_stock(gtk.STOCK_YES, 1)
            self.connect_button.hide()
            self.disconnect_button.show()
            self.cancel_button.hide()
        elif not connected and status == 'idle':
            self.status_image.set_from_stock(gtk.STOCK_NO, 1)
            self.connect_button.show()
            self.disconnect_button.hide()
            self.cancel_button.hide()
        elif not connected and status == 'connecting':
            self.status_image.set_from_stock(gtk.STOCK_NO, 1)
            self.cancel_button.show()
            self.connect_button.hide()
            self.disconnect_button.hide()
        elif connected:
            self.status_image.set_from_stock(gtk.STOCK_YES, 1)
            self.disconnect_button.show()
            self.connect_button.hide()
            self.cancel_button.hide()
        elif not connected:
            self.status_image.set_from_stock(gtk.STOCK_NO, 1)
            self.disconnect_button.hide()
            self.connect_button.show()
            self.cancel_button.hide()
