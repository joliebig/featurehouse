'''A window to show debug messages'''
import gtk
import pango
from DebugManager import DebugLevel
class DebugWindow:
    '''The window containing the debug info'''
    def __init__(self, debug_manager):
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.set_title("Debug Console")
        self.window.connect("delete_event", self.on_delete)
        self.debug_manager = debug_manager
        self.store = DebugStore(debug_manager)
        self.view = DebugView(self.store)
        self.scroll_view = gtk.ScrolledWindow()
        self.scroll_view.add(self.view)
        self.category_tbtn = {}
        self.vbox = gtk.VBox()
        self.filter_box = gtk.HBox()
        self.category_box = gtk.HBox()
        self.filter_entry = gtk.Entry()
        self.filter_box.pack_start(self.filter_entry)
        self.level_model = gtk.TreeStore(str, int)
        for level in sorted([x for x in dir(DebugLevel) if x.isalpha()],
            key=lambda x:getattr(DebugLevel,x)):
            self.level_model.append(None, (level, getattr(DebugLevel, level)))
        self.level_combo = gtk.ComboBox(self.level_model)
        cell = gtk.CellRendererText()
        self.filter_box.pack_start(self.level_combo)
        self.level_combo.pack_start(cell, True)
        self.level_combo.add_attribute(cell, 'text', 0)
        self.vbox.pack_start(self.filter_box, False)
        self.category_tbtn['protocol'] = gtk.ToggleButton(_('Protocol'))
        self.category_box.pack_start(self.category_tbtn['protocol'])
        self.category_tbtn['core'] = gtk.ToggleButton(_('Core'))
        self.category_box.pack_start(self.category_tbtn['core'])
        self.category_tbtn['plugin'] = gtk.ToggleButton(_('Plugins'))
        self.category_box.pack_start(self.category_tbtn['plugin'])
        self.vbox.pack_start(self.category_box, False)
        self.vbox.pack_start(self.scroll_view)
        self.window.add(self.vbox)
        self.category_tbtn['core'].set_active(True)
        self.category_tbtn['plugin'].set_active(True)
        self.window.get_settings().set_property('gtk-button-images', True)
        self.category_tbtn['protocol'].set_image(
            gtk.image_new_from_stock(gtk.STOCK_YES, gtk.ICON_SIZE_BUTTON))
        for tbtn in self.category_tbtn.values():
            tbtn.connect('clicked', self.on_category_toggled)
            self.on_category_toggled(tbtn)
        self.filter_entry.connect('changed', self.on_filter_changed)
        self.level_combo.connect('changed', self.on_filter_changed)
        self.on_filter_changed()
    def show( self ):
        '''shows the window'''
        self.window.show_all()
    def get_filter_state(self):
        categories = []
        for category in self.category_tbtn:
            if self.category_tbtn[category].get_active():
                categories.append(category)
        if self.level_combo.get_active_iter():
            level = self.level_model.get_value(
                self.level_combo.get_active_iter(), 1)
        else:
            level = -1
        return [self.filter_entry.get_text(), categories, level]
    def on_category_toggled(self, button, *args):
        if button.get_active():
            button.set_image(gtk.image_new_from_stock(
                    gtk.STOCK_YES, gtk.ICON_SIZE_BUTTON))
        else:
            button.set_image(gtk.image_new_from_stock(
                    gtk.STOCK_NO, gtk.ICON_SIZE_BUTTON))
        self.on_filter_changed()
    def on_filter_changed(self, *args, **kwargs):
        '''used when the filter button is clicked'''
        self.view.filter_caller(self.get_filter_state())
    def on_close(self, button, data=None):
        self.destroy()
    def on_delete(self, *args):
        return False
class DebugView( gtk.TextView ):
    '''A TextView optimized for debug consoles'''
    def __init__(self, store):
        gtk.TextView.__init__(self)
        self.store = store
        self.buffer = DebugBuffer(store)
        self.set_buffer(self.buffer)
        self.set_editable(False)
    def filter_caller(self, filter):
        self.store.filter_caller(filter)
        self.buffer = DebugBuffer(self.store.filter)
        self.set_buffer(self.buffer)
class DebugBuffer( gtk.TextBuffer ):
    '''A TextBuffer based on a ListStore'''
    def __init__(self, store):
        gtk.TextBuffer.__init__(self)
        self.store = store
        self.create_tag("caller", weight=pango.WEIGHT_BOLD)
        self.create_tag("message")
        self.iter = self.get_start_iter()
        for row in store:
            self.insert_with_tags_by_name(self.iter, row[0], "caller")
            self.insert_with_tags_by_name(self.iter, ": " + row[1], "message")
            self.insert(self.iter, '\n')
        store.connect("row-changed", self.on_store_insert)
    def on_store_insert(self, model, path, iter):
        caller = model.get_value(iter, 0)
        message =  model.get_value(iter, 1)
        level = model.get_value(iter, 2)
        line_tags = []
        if level == DebugLevel.error:
            line_tags.append('error')
        if level == DebugLevel.warning:
            line_tags.append('warning')
        if level == DebugLevel.info:
            line_tags.append('info')
        if caller and message:
            self.insert_with_tags_by_name(self.iter, caller, 
                                          "caller", *line_tags)
            self.insert_with_tags_by_name(self.iter, ": " + message + '\n',
                                          "message", *line_tags)
class DebugStore( gtk.ListStore ):
    '''A ListStore with filtering and more, optimized for debug'''
    def __init__( self, debug_manager ):
        '''constructor'''
        gtk.ListStore.__init__(self, str, str, int) #category, message, level
        self.debug_manager = debug_manager
        self.filter = self.filter_new()
        for message in debug_manager.get_all():
            self.on_message_added(message)
    def connect_to_manager(self):
        self.debug_manager.connect('message-added', self.on_message_added)
    def on_message_added(self, message):
        self.append([message['category'], message['message'], message['level']])
    def filter_caller( self, filter ):
        '''displays only the messages that agrees with filter'''
        del self.filter
        self.filter = self.filter_new()
        self.filter.set_visible_func(filter_func, filter)
def filter_func(model, iter, filter):
    '''returns true if the caller column matches name'''
    category = model.get_value(iter, 0)
    msg = model.get_value(iter, 1)
    level = model.get_value(iter, 2)
    if msg.find(filter[0]) == -1:
        return False
    if category not in filter[1]:
        return False
    if level < filter[2]:
        return False
    return True
