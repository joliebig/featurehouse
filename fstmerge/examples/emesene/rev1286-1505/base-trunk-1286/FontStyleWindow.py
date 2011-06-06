import os
import gtk
from gobject import timeout_add, source_remove
import stock
import dialog
class FontStyleWindow(gtk.Window):
    '''this is the window that opens when you press the font button on the
    conversation window'''
    def __init__(self, controller, parent):
        '''class constructor'''
        gtk.Window.__init__(self)
        self.controller = controller
        self.config = self.controller.config
        self.parentUI = parent
        self.set_decorated(False)
        self.set_type_hint(gtk.gdk.WINDOW_TYPE_HINT_DIALOG)
        self.set_position(gtk.WIN_POS_MOUSE)
        self.set_resizable(False)
        self.set_title(_('Font styles'))
        self.set_border_width(5)
        self.vbox = gtk.VBox()
        self.closed = True
        self.buttonBold = gtk.CheckButton()
        self.buttonBold.connect('toggled', self.toggled, 'bold')
        self.buttonBold.set_active(self.config.user['fontBold'])
        self.buttonBoldLabel = gtk.Label('<span weight="bold">' + \
                                            _('Bold') + '</span>')
        self.buttonBoldLabel.set_use_markup(True)
        self.buttonBold.add(self.buttonBoldLabel)
        self.buttonItalic = gtk.CheckButton()
        self.buttonItalic.connect('toggled', self.toggled, 'italic')
        self.buttonItalic.set_active(self.config.user['fontItalic'])
        self.buttonItalicLabel = gtk.Label('<span style="italic">' + \
                                            _('Italic') + '</span>')
        self.buttonItalicLabel.set_use_markup(True)
        self.buttonItalic.add(self.buttonItalicLabel)
        self.buttonUnderline = gtk.CheckButton()
        self.buttonUnderline.connect('toggled', self.toggled, 'underline')
        self.buttonUnderline.set_active(self.config.user['fontUnderline'])
        self.buttonUnderlineLabel = gtk.Label('<span underline="single">' + \
                                            _('Underline') + '</span>')
        self.buttonUnderlineLabel.set_use_markup(True)
        self.buttonUnderline.add(self.buttonUnderlineLabel)
        self.buttonStrike = gtk.CheckButton()
        self.buttonStrike.connect('toggled', self.toggled, 'strike')
        self.buttonStrike.set_active(self.config.user['fontStrike'])
        self.buttonStrikeLabel = gtk.Label('<span strikethrough="true">' + \
                                            _('Strike') + '</span>')
        self.buttonStrikeLabel.set_use_markup(True)
        self.buttonStrike.add(self.buttonStrikeLabel)
        self.buttonReset = gtk.Button(_('Reset'))
        self.buttonReset.connect('clicked', self.resetFormat)
        self.vbox.pack_start(self.buttonBold)
        self.vbox.pack_start(self.buttonItalic)
        self.vbox.pack_start(self.buttonUnderline)
        self.vbox.pack_start(self.buttonStrike)
        self.vbox.pack_start(gtk.HSeparator())
        self.vbox.pack_start(self.buttonReset)
        self.add(self.vbox)
        self.vbox.show_all()
        self.connect('leave-notify-event', self.on_leave_notify_event)
        self.tag = None
    def toggled(self, button, option):
        '''callback for the toggled signal'''
        if option == 'bold':
            self.config.user['fontBold'] = button.get_active()
        elif option == 'italic':
            self.config.user['fontItalic'] = button.get_active()
        elif option == 'underline':
            self.config.user['fontUnderline'] = button.get_active()
        elif option == 'strike':
            self.config.user['fontStrike'] = button.get_active()
        if hasattr(self.parentUI, 'input'):
           self.parentUI.input.applyAttrsToInput()
    def resetFormat(self, *args):
        self.config.user['fontBold'] = False
        self.buttonBold.set_active(0)
        self.config.user['fontItalic'] = False
        self.buttonItalic.set_active(0)
        self.config.user['fontUnderline'] = False
        self.buttonUnderline.set_active(0)
        self.config.user['fontStrike'] = False
        self.buttonStrike.set_active(0)
        if hasattr(self.parentUI, 'input'):
           self.parentUI.input.applyAttrsToInput()
    def on_leave_notify_event(self, *args):
        if not self.tag and not self.closed:
            self.tag = timeout_add(500, self.hide)
    def show(self):
        gtk.Window.show(self)
        self.tag = None
        self.closed = False
    def hide(self):
        gtk.Window.hide(self)
        if (self.tag):
            source_remove(self.tag)
            self.tag = None
        self.closed = True
