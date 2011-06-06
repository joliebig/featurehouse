import os
import sys
import gtk
import time
import gobject
import gettext
if __name__ == '__main__':
    import paths
    if os.path.exists("default.mo"):
        gettext.GNUTranslations(open("default.mo")).install()
    elif os.path.exists(paths.LANG_PATH):
        gettext.install('emesene', paths.LANG_PATH)
    else:
        gettext.install('emesene')
import htmltextview
import plugins_base.Logger as Logger
class LogViewer(gtk.Window):
    '''the main window of the log viewer'''
    def __init__(self, exit_on_quit=False):
        '''class constructor'''
        gtk.Window.__init__(self)
        self.set_default_size(800, 600)
        self.set_title(_('Log viewer'))
        self.logger = None
        self.template = {}
        self.template['nick-changed'] = \
          _('[%(date)s] %(mail)s changed the nick to %(data)s\n')
        self.template['personal-message-changed'] = \
         _('[%(date)s] %(mail)s changed the personal message to %(data)s\n')
        self.template['status-changed'] = \
         _('[%(date)s] %(mail)s changed his status to %(data)s\n')
        self.template['user-offline'] = \
         _('[%(date)s] %(mail)s is now offline\n')
        self.template['message'] = \
         _('[%(date)s] %(mail)s: %(data)s\n')
        self.template['nudge'] = \
         _('[%(date)s] %(mail)s just sent you a nudge\n')
        self.template['action-message'] = \
         _('[%(date)s] %(mail)s %(data)s\n')
        self.template_default = \
          _('[%(date)s] %(mail)s %(event)s %(data)s\n')
        self.exit_on_quit = exit_on_quit
        self.main_vbox = gtk.VBox()
        self.main_vbox.set_spacing(2)
        self.menu = LogViewerMenu()
        self.paned = gtk.HPaned()
        self.left_panel = LeftPanel()
        self.right_panel = RightPanel()
        self.status = gtk.Statusbar()
        self.paned.add1(self.left_panel)
        self.paned.add2(self.right_panel)
        self.left_panel.mail_filter.connect('filter-changed',
            self.on_mail_filter_changed)
        self.left_panel.mail_filter.connect('all-selected',
            self.on_mail_filter_all_selected)
        self.left_panel.mail_filter.connect('all-deselected',
            self.on_mail_filter_all_deselected)
        self.left_panel.user_event_filter.connect('filter-changed',
            self.on_user_event_filter_changed)
        self.left_panel.user_event_filter.connect('all-selected',
            self.on_user_event_filter_all_selected)
        self.left_panel.user_event_filter.connect('all-deselected',
            self.on_user_event_filter_all_deselected)
        self.left_panel.conv_event_filter.connect('filter-changed',
            self.on_user_event_filter_changed)
        self.left_panel.conv_event_filter.connect('all-selected',
            self.on_user_event_filter_all_selected)
        self.left_panel.conv_event_filter.connect('all-deselected',
            self.on_user_event_filter_all_deselected)
        self.menu.connect('open-file', self.on_open_file)
        self.menu.connect('clear', self.on_clear)
        self.menu.connect('quit', self.on_quit)
        self.left_panel.b_run.connect('clicked', self.on_b_run_clicked)
        self.connect('delete-event', self.on_quit)
        self.main_vbox.pack_start(self.menu, False)
        self.main_vbox.pack_start(self.paned)
        self.main_vbox.pack_start(self.status, False)
        self.main_vbox.show_all()
        self.add(self.main_vbox)
    def open_file(self, filename):
        '''open the db and do all the things needed to show the info'''
        self.logger = Logger.Logger(filename)
        self.left_panel.mail_filter.clear()
        self.left_panel.user_event_filter.clear()
        self.left_panel.conv_event_filter.clear()
        for mail in self.logger.get_mails():
            self.left_panel.mail_filter.append((False, mail))
        for event in self.logger.get_user_events():
            self.left_panel.user_event_filter.append((False, event))
        for event in self.logger.get_conversation_events():
            self.left_panel.conv_event_filter.append((False, event))
    def on_open_file(self, widget):
        '''called when the open option is clicked, show a open dialog'''
        dialog = gtk.FileChooserDialog(_('Open a log file'), 
                    self, gtk.FILE_CHOOSER_ACTION_OPEN, 
                    (_( 'Open'), gtk.RESPONSE_OK, 
                    _('Cancel'), gtk.RESPONSE_CANCEL))
        filter = gtk.FileFilter()
        filter.add_pattern('*.db')
        filter.set_name('*.db')
        dialog.add_filter(filter)
        response = dialog.run()
        dialog.hide()
        if response == gtk.RESPONSE_OK:
            self.open_file(dialog.get_filename())
    def on_clear(self, widget):
        '''clear the content of the results'''
        self.left_panel.mail_filter.clear()
        self.left_panel.user_event_filter.clear()
        self.left_panel.conv_event_filter.clear()
        self.right_panel.clear()
        self.logger = None
    def on_quit(self, *args):
        '''quit the program'''
        if self.exit_on_quit:
            sys.exit(0)
        else:
            self.hide()
    def on_mail_filter_changed(self, filter_list, state, mail):
        '''called when a value changes on the mail filter'''
        print state, mail
    def on_mail_filter_all_selected(self, filter_list):
        '''called when all values are selected on the mail filter'''
        print 'all mails selected'
    def on_mail_filter_all_deselected(self, filter_list):
        '''called when all values are desellected on the mail filter'''
        print 'all mails deselected'
    def on_user_event_filter_changed(self, filter_list, state, event):
        '''called when a value changes on the event filter'''
        print state, event
    def on_user_event_filter_all_selected(self, filter_list):
        '''called when all values are selected on the event filter'''
        print 'all events selected'
    def on_user_event_filter_all_deselected(self, filter_list):
        '''called when all values are deselected on the event filter'''
        print 'all events deselected'
    def on_conv_event_filter_changed(self, filter_list, state, event):
        '''called when a value changes on the event filter'''
        print state, event
    def on_conv_event_filter_all_selected(self, filter_list):
        '''called when all values are selected on the event filter'''
        print 'all events selected'
    def on_conv_event_filter_all_deselected(self, filter_list):
        '''called when all values are deselected on the event filter'''
        print 'all events deselected'
    def on_b_run_clicked(self, widget):
        '''called when the run button is clicked'''
        self.show_user_events()
        self.show_conv_events()
    def show_user_events(self):
        '''show the data that match with the user selection'''
        mails = self.left_panel.mail_filter.get_by_status(True)
        user_events = self.left_panel.user_event_filter.get_by_status(True)
        self.right_panel.set_user_text('')
        mail_list = '"' + '","'.join(mails) + '"'
        event_list = '"' + '","'.join(user_events) + '"'
        t1 = time.time()
        for (stamp, name, account, data) in self.logger.query('\
        select e.stamp, e.name, u.account, ue.data \
        from user u, event e, user_event ue \
        where ue.id_event = e.id and \
        ue.id_user = u.id and \
        u.account in (%s) and \
        e.name in (%s) \
        order by e.stamp, u.account, e.name;' % (mail_list, event_list)):
            self.right_panel.append_user_text(self.format_user_event(stamp, 
                name, account, data))
        t2 = time.time()
        print 'time user: ', t2 - t1
    def show_conv_events(self):
        '''show the data that match with the user selection'''
        mails = self.left_panel.mail_filter.get_by_status(True)
        conv_events = self.left_panel.conv_event_filter.get_by_status(True)
        self.right_panel.set_conv_text('')
        mail_list = '"' + '","'.join(mails) + '"'
        event_list = '"' + '","'.join(conv_events) + '"'
        t1 = time.time()
        for (stamp, name, account, data) in self.logger.query('\
        select e.stamp, e.name, u.account, ue.data \
        from event e, conversation_event ue, user u \
        where ue.id_event = e.id and \
        ue.id_user = u.id and \
        u.account in (%s) and \
        e.name in (%s) \
        order by e.stamp, u.account, e.name;' % (mail_list, event_list)):
            self.right_panel.append_conv_text(self.format_conv_event(stamp, 
                name, account, data))
        t2 = time.time()
        print 'time conversation: ', t2 - t1
    def format_user_event(self, stamp, name, account, data):
        '''give a specific format to known events'''
        if name in self.template:
            return self.template[name] % self.map_user_event(stamp, name, 
                account, data)
        return self.template_default % self.map_user_event(stamp, name,
            account, data)
    def format_conv_event(self, stamp, name, account, data):
        '''give a specific format to known events'''
        if name == 'message':
            try:
                data = data.split('\r\n')[2]
            except IndexError:
                print 'invalid message format: %s' % data
        if name in self.template:
            return self.template[name] % self.map_user_event(stamp, name, 
                account, data)
        return self.template_default % self.map_user_event(stamp, name,
            account, data)
    def map_user_event(self, stamp, name, account, data):
        '''create a dict with the data from a user event that can 
        be used to replace values on the template'''
        return {'mail' : account, 
        'data' : gobject.markup_escape_text(data), 
        'date' : gobject.markup_escape_text(time.ctime(stamp)), 
        'event' : gobject.markup_escape_text(name)}
class LogViewerMenu(gtk.MenuBar):
    '''the menu from the main window'''
    __gsignals__ = {
        'open-file' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'clear' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()),
        'quit' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()) }
    def __init__(self):
        gtk.MenuBar.__init__(self)
        file_menu = gtk.Menu()
        file_menu_item = gtk.ImageMenuItem(_('_File'))
        file_menu_item.set_submenu(file_menu)
        open_menu_item = gtk.ImageMenuItem(gtk.STOCK_OPEN)
        file_menu.add(open_menu_item)
        clear_menu_item = gtk.ImageMenuItem(gtk.STOCK_CLEAR)
        file_menu.add(clear_menu_item)
        file_menu.add(gtk.SeparatorMenuItem())
        quit_menu_item = gtk.ImageMenuItem(gtk.STOCK_QUIT)
        file_menu.add(quit_menu_item)
        open_menu_item.connect("activate" , self.on_open_activate)
        clear_menu_item.connect("activate" , self.on_clear_activate)
        quit_menu_item.connect("activate" , self.on_quit_activate)
        self.add(file_menu_item)
        file_menu.show_all()
    def on_open_activate(self, widget):
        self.emit('open-file')
    def on_clear_activate(self, widget):
        self.emit('clear')
    def on_quit_activate(self, widget):
        self.emit('quit')
class LeftPanel(gtk.VBox):
    '''the left panel of the main window'''
    def __init__(self):
        '''class constructor'''
        gtk.VBox.__init__(self)
        self.set_border_width(2)
        self.set_spacing(2)
        self.tabs = gtk.Notebook()
        self.calendar = gtk.Calendar()
        self.mail_filter = FilterList(_('Contact'))
        self.user_event_filter = FilterList(_('Name'))
        self.conv_event_filter = FilterList(_('Name'))
        self.b_run = gtk.Button(stock=gtk.STOCK_EXECUTE)
        self.tabs.append_page(self.mail_filter, gtk.Label(_('Contacts')))
        self.tabs.append_page(self.user_event_filter,gtk.Label(
            _('User Events')))
        self.tabs.append_page(self.conv_event_filter,gtk.Label(
            _('Conversation Events')))
        self.pack_start(self.tabs, True, True)
        self.pack_start(self.b_run, False)
        self.show_all() 
class RightPanel(gtk.VBox):
    '''the right panel of the main window'''
    def __init__(self):
        '''class constructor'''
        gtk.VBox.__init__(self)
        self.set_border_width(2)
        self.user_view = gtk.TextView()#htmltextview.HtmlTextView()
        self.user_buffer = self.user_view.get_buffer()
        self.user_scroll = gtk.ScrolledWindow()
        self.user_scroll.add(self.user_view)
        self.user_scroll.set_policy(gtk.POLICY_AUTOMATIC, 
            gtk.POLICY_AUTOMATIC)
        self.conv_view = gtk.TextView()#htmltextview.HtmlTextView()
        self.conv_buffer = self.conv_view.get_buffer()
        self.conv_scroll = gtk.ScrolledWindow()
        self.conv_scroll.add(self.conv_view)
        self.conv_scroll.set_policy(gtk.POLICY_AUTOMATIC, 
            gtk.POLICY_AUTOMATIC)
        notebook = gtk.Notebook()
        notebook.append_page(self.user_scroll, gtk.Label(_('User events')))
        notebook.append_page(self.conv_scroll, 
            gtk.Label(_('Conversation events')))
        self.pack_start(notebook, True, True)
        self.show_all()
    def clear(self):
        '''clear the content of the view'''
        self.user_buffer.set_text('')
        self.conv_buffer.set_text('')
    def set_user_text(self, text):
        '''set the text of the view'''
        self.user_buffer.set_text(text)
    def set_conv_text(self, text):
        '''set the text of the view'''
        self.conv_buffer.set_text(text)
    def append_user_text(self, text):
        '''append text to the view'''
        self.user_buffer.insert(self.user_buffer.get_end_iter(), text)
    def append_conv_text(self, text):
        '''append text to the view'''
        self.conv_buffer.insert(self.conv_buffer.get_end_iter(), text)
class FilterList(gtk.VBox):
    '''a class that represent a list of items with checkboxes and 
    2 buttons, select all, deselect all'''
    __gsignals__ = {
        'filter-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT, gobject.TYPE_PYOBJECT)),
        'all-selected' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            ()),
        'all-deselected' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            ()),
    }
    def __init__(self, column_name):
        '''class constructor'''
        gtk.VBox.__init__(self)
        self.set_border_width(2)
        self.scroll = gtk.ScrolledWindow()
        self.model = gtk.ListStore(bool, str)
        self.list = gtk.TreeView(self.model)
        cr_toggle = gtk.CellRendererToggle()
        cr_text = gtk.CellRendererText()
        col_toggle = gtk.TreeViewColumn(_('State'), cr_toggle, active=0)
        col_text = gtk.TreeViewColumn(column_name, cr_text, text=1)
        cr_toggle.connect('toggled', self.active_toggled)
        self.list.append_column(col_toggle)
        self.list.append_column(col_text)
        self.scroll.add(self.list)
        self.scroll.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.b_select_all = gtk.Button(_('Select all'))
        self.b_deselect_all = gtk.Button(_('Deselect all'))
        self.b_select_all.connect('clicked', self.on_b_select_all)
        self.b_deselect_all.connect('clicked', self.on_b_deselect_all)
        hbox = gtk.HBox()
        hbox.set_border_width(2)
        hbox.pack_start(self.b_select_all, True, False)
        hbox.pack_start(self.b_deselect_all, True, False)
        self.pack_start(self.scroll, True, True)
        self.pack_start(hbox, False)
        self.show_all()
    def active_toggled(self, cell, path):
        '''called when a row is toggled'''
        iterator = self.model.get_iter_from_string(path)
        value_bool = self.model.get_value(iterator, 0)
        value_str = self.model.get_value(iterator, 1)
        self.model.set_value(iterator, 0, not value_bool)
        self.emit('filter-changed', not value_bool, value_str)
    def get_all_items(self):
        '''return a list of tuples with the values'''
        item_list = []
        for row in self.model:
            item_list.append((row[0], row[1]))
        return item_list
    def get_by_status(self, status):
        '''return a list of tuples with the values that have the status
        "status"'''
        item_list = []
        for row in self.model:
            if row[0] == status:
                item_list.append(row[1])
        return item_list
    def set_all(self, state):
        '''set all the checkboxes to state (True or False)'''
        for row in self.model:
            row[0] = state
    def clear(self):
        '''clear the items on the filter list'''
        self.model.clear()
    def append(self, row):
        '''append a row to the model'''
        self.model.append(row)
    def on_b_select_all(self, widget):
        '''called when the select all is clicked'''
        self.set_all(True)
        self.emit('all-selected')
    def on_b_deselect_all(self, widget):
        '''called when the deselect all is clicked'''
        self.set_all(False)
        self.emit('all-deselected')
gobject.type_register(LogViewerMenu)
if __name__ == '__main__':
    def test():
        log_viewer = LogViewer(True)
        log_viewer.show()
        gtk.main()
    test()
