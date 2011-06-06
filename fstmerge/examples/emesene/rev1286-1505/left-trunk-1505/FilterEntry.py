import gtk
try: import sexy
except: hasSexy = False
else: hasSexy = True
class FilterEntry(gtk.HBox):
    def __init__(self, callback):
        '''the callback is a function that receive
        the string typed here as only parameter'''
        gtk.HBox.__init__(self)
        self.set_border_width(2)
        self.callback = callback
        if hasSexy:
            self.entry = sexy.IconEntry()
            self.entry.set_icon(sexy.ICON_ENTRY_PRIMARY,gtk.image_new_from_stock(gtk.STOCK_FIND,gtk.ICON_SIZE_BUTTON))
            self.entry.add_clear_button()
        else:
            self.entry = gtk.Entry()
            iconInfo = gtk.icon_theme_get_default().lookup_icon( \
                gtk.STOCK_FIND, 22, 0)
            self.icon = None
            if iconInfo:
                self.icon = gtk.Image()
                self.icon.set_from_pixbuf(iconInfo.load_icon())
                self.pack_start(self.icon, False, False)
        self.entry.connect('changed', self.entryChanged)
        self.entry.connect('key_press_event', self.entryKeypressEvent)
        self.pack_start(self.entry)
        self.show_all()
    def entryChanged(self, *args):
        self.callback(self.entry.get_text())
    def entryKeypressEvent(self, widget, event):
        keyval = gtk.gdk.keyval_name(event.keyval)
        if keyval == 'Escape':
            self.entry.props.text = ''
            return True
        return False
