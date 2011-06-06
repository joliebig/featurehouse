import gtk
import Plugin
from gobject import timeout_add, source_remove
class PlusColorPanel(gtk.Window):  
    def __init__(self):
        gtk.Window.__init__(self)
        self.set_decorated(False)
        self.set_type_hint(gtk.gdk.WINDOW_TYPE_HINT_DIALOG)        
        self.set_default_size(300, 300)
        self.set_border_width(5)
        self.set_position(gtk.WIN_POS_MOUSE)
        self.connect('delete-event' , self.on_delete_event)
        self.connect('leave-notify-event' , self.on_leave_notify_event)
        self.connect('enter-notify-event' , self.on_enter_notify_event)
        self.vbox = gtk.VBox()
        self.colorCodes = [
'ffffff', '000000', '00007D', '009100', 'FF0000', '7D0000', '9A009A', 'FC7D00', 
'FFFF00', '00FC00', '009191', '00FFFF', '1E1EFC', 'FF00FF', '7D7D7D', 'D2D2D2', 
'E7E6E4', 'cfcdd0', 'ffdea4', 'ffaeb9', 'ffa8ff', 'c1b4fc', 'bafbe5', 'ccffa3', 
'fafda2', 'b6b4b7', 'a2a0a1', 'f9c152', 'ff6d66', 'ff62ff', '6c6cff', '68ffc3', 
'000000', 'f9ff57', '858482', '6e6d7b', 'ffa01e', 'F92411', 'FF1EFF', '1E29FF', 
'7dffa5', '60f913', 'fff813', '5e6464', '4b494c', 'd98812', 'eb0505', 'de00de', 
'0000d3', '03cc88', '59d80d', 'd4c804', '333335', '18171c', '944e00', '9b0008', 
'980299', '01038c', '01885f', '389600', '9a9e15', '473400', '4d0000', '5f0162', 
'000047', '06502f', '1c5300', '544d05']
        self.toolbar = gtk.Toolbar()
        self.toolbar.set_orientation(gtk.ORIENTATION_HORIZONTAL)
        self.toolbar.set_style(gtk.TOOLBAR_ICONS)
        self.toolbar.connect('enter-notify-event' , self.on_enter_notify_event)
        self.tooltips = gtk.Tooltips()
        self.btnBold = gtk.ToolButton(gtk.STOCK_BOLD)
        self.btnBold.connect("clicked", self.select_format, 'b')
        self.btnBold.set_tooltip(self.tooltips, _("Bold"))
        self.toolbar.add(self.btnBold)
        self.btnItalic = gtk.ToolButton(gtk.STOCK_ITALIC)
        self.btnItalic.connect("clicked", self.select_format, 'i')
        self.btnItalic.set_tooltip(self.tooltips, _("Italic"))
        self.toolbar.add(self.btnItalic)
        self.btnUnderline = gtk.ToolButton(gtk.STOCK_UNDERLINE)
        self.btnUnderline.connect("clicked", self.select_format, 'u')
        self.btnUnderline.set_tooltip(self.tooltips, _("Underline"))
        self.toolbar.add(self.btnUnderline)
        self.btnStrike = gtk.ToolButton(gtk.STOCK_STRIKETHROUGH)
        self.btnStrike.connect("clicked", self.select_format, 's')
        self.btnStrike.set_tooltip(self.tooltips, _('Strike'))
        self.toolbar.add(self.btnStrike)
        self.toolbar.add(gtk.SeparatorToolItem())
        self.btnSelectColor = gtk.ToggleToolButton(gtk.STOCK_COLOR_PICKER)
        self.btnSelectColor.connect('clicked', self.on_color_select_clicked)
        self.btnSelectColor.set_tooltip(self.tooltips, _("Select custom color"))
        self.toolbar.add(self.btnSelectColor)
        self.toolbar.add(gtk.SeparatorToolItem())
        self.btnBackground = gtk.ToggleToolButton(gtk.STOCK_SELECT_COLOR)
        self.btnBackground.set_tooltip(self.tooltips, _("Enable background color"))
        self.toolbar.add(self.btnBackground)
        self.tooltips.enable()
        self.vbox.pack_start(self.toolbar, False)
        self.table = gtk.Table(8)
        self.table.set_row_spacings(3)
        self.table.set_col_spacings(3)
        x = 0
        y = 0
        for i in self.colorCodes:
            if x == 8:
                x = 0
                y += 1
            try:
                self.btnColor = gtk.Button()
                color = self.btnColor.get_colormap().alloc_color("#"+ i)
                style = self.btnColor.get_style().copy()
                style.bg[gtk.STATE_NORMAL] = color
                self.btnColor.set_style(style)
                self.btnColor.connect('clicked', self.select_color, self.colorCodes.index(i))
                self.btnColor.connect('enter-notify-event' , self.on_enter_notify_event)
                self.table.attach(self.btnColor, x, x+1, y, y+1)
            except:
                x -= 1
            x += 1
        self.vbox.pack_start(self.table)
        self.buffer = gtk.TextBuffer()
        self.closed = False
        self.add(self.vbox)
        self.vbox.show_all()
        self.tag = None
    def on_leave_notify_event(self, *args):
        if not self.tag and not self.closed:
            self.tag = timeout_add(500, self.hide)
    def on_enter_notify_event(self, *args):
        if self.tag:
            source_remove(self.tag)
            self.tag = None
    def on_delete_event(self , *args):
        self.hide()
        self.closed = True
        return True
    def hide(self):
        gtk.Window.hide(self)
        if (self.tag):
            source_remove(self.tag)
            self.tag = None
        self.closed = True
    def show(self):
        gtk.Window.show(self)
        self.closed = False
    def select_color(self, widget=None, color=None):
        if self.btnBackground.get_active(): tag = 'a'
        else: tag = 'c'
        if self.buffer.get_has_selection():
            start, end = self.buffer.get_selection_bounds()
            selectedtext = self.buffer.get_text(start, end)
            self.buffer.delete(start, end)
            self.buffer.insert(start, "[%s=%s]%s[/%s]" %(tag, color, selectedtext, tag))
        else:
            self.buffer.insert_at_cursor("[%s=%s][/%s]" %(tag, color, tag))
            cursor=self.buffer.get_iter_at_mark(self.buffer.get_insert())
            cursor.backward_chars(4)
            self.buffer.place_cursor(cursor)
        self.btnBackground.set_active(False)
        self.hide()
    def select_format(self, widget, tag):
        if self.buffer.get_has_selection():
            start, end = self.buffer.get_selection_bounds()
            selectedtext = self.buffer.get_text(start, end)
            self.buffer.delete(start, end)
            self.buffer.insert(start, "[%s]%s[/%s]" %(tag, selectedtext, tag))
        else:
            self.buffer.insert_at_cursor("[%s][/%s]" %(tag, tag))
            cursor=self.buffer.get_iter_at_mark(self.buffer.get_insert())
            cursor.backward_chars(4)
            self.buffer.place_cursor(cursor)
        self.hide()
    def on_color_select_clicked(self, *args):
        colorDialog = gtk.ColorSelectionDialog('Choose a color')
        colorDialog.colorsel.set_has_palette(True)
        response = colorDialog.run()
        if response == gtk.RESPONSE_OK:
            color = colorDialog.colorsel.get_current_color()
            red = color.red >> 8
            green = color.green >> 8
            blue = color.blue >> 8
            color = '#%02X%02X%02X' % (red , green , blue)
            self.select_color(color=color)
        colorDialog.destroy()
class PlusButton(gtk.ToolButton):
    def __init__(self, conversation):
        gtk.ToolButton.__init__(self, gtk.STOCK_COLOR_PICKER)
        self.pluswindow = PlusColorPanel()
        self.pluswindow.buffer = conversation.ui.input.inputBuffer
    def show_plus_window(self, widget=None, data=None):
        self.pluswindow.show()
    def clean(self):
        self.pluswindow.destroy()
        self.destroy()
class MainClass(Plugin.Plugin):
    def __init__(self, controller, msn):
        Plugin.Plugin.__init__(self, controller, msn)
        self.description = _('A panel for applying Messenger Plus formats to the text') 
        self.authors = { 'mg' : 'themgzzy at gmail dot com' }
        self.displayName = _('Plus Color Panel')
        self.name = 'PlusColorPanel'
    def start(self):
        '''start the plugin'''
        for conversation in self.getOpenConversations():
            self.add_button(conversation=conversation)
        self.signalopen = self.controller.conversationManager.connect('new-conversation-ui', self.add_button)
        self.signalclose = self.controller.conversationManager.connect('close-conversation-ui', self.remove_button)
        self.enabled = True
    def stop(self):
        for conversation in self.getOpenConversations():
            self.remove_button(conversation=conversation)
        self.controller.conversationManager.disconnect(self.signalopen)
        self.controller.conversationManager.disconnect(self.signalclose)
        self.enabled = False
    def check(self):
        '''
        check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )
        '''
        return (True, 'Ok')
    def add_button(self, conversationmanager=None, conversation=None, win=None):
        button = PlusButton(conversation)
        button.connect('clicked', button.show_plus_window)
        button.show()
        toolbar = conversation.ui.input.toolbar
        toolbar.add(button)
    def remove_button(self, conversationmanager=None, conversation=None, win=None):
        for button in conversation.ui.input.toolbar.get_children():
            if type(button) == PlusButton:
                conversation.ui.input.toolbar.remove(button)
                button.pluswindow.destroy()
                button.destroy()
