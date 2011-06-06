'''a module that defines the api of objects that display dialogs'''
import os
import gtk
import gobject
import Avatar
import abstract.stock as stock
_avatar_chooser = None
def window_add_image(window, stock_id):
    '''add a stock image as the first element of the window.hbox'''
    image = gtk.image_new_from_stock(stock_id, gtk.ICON_SIZE_DIALOG)
    window.hbox.pack_start(image, False)
    image.show()
    return image
def window_add_button(window, stock_id, label=''):
    '''add a button to the window'''
    button = gtk.Button(label, stock=stock_id)
    window.bbox.pack_start(button, True, True)
    button.show()
    return button
def window_add_label(window, text):
    '''add a label with the text (as pango) on the window'''
    label = gtk.Label()
    label.set_use_markup(True)
    label.set_markup('<span>' + \
        text + "</span>")
    window.hbox.pack_start(label, True, True)
    label.show()
    return label
def close_cb(widget, event, window, response_cb, *args):
    '''default close callback, call response_cb with args if it's not
    None'''
    if response_cb:
        response_cb(*args)
    window.hide()
def default_cb(widget, window, response_cb, *args):
    '''default callbacks, call response_cb with args if it's not
    None'''
    if response_cb:
        response_cb(*args)
    window.hide()
def entry_cb(widget, window, response_cb, *args):
    '''callback called when the entry is activated, it call the response
    callback with the stock.ACCEPT and append the value of the entry
    to args'''
    args = list(args)
    args.append(window.entry.get_text())
    if response_cb:
        if type(widget) == gtk.Entry:
            response_cb(stock.ACCEPT, *args)
        else:
            response_cb(*args)
    window.hide()
def add_contact_cb(widget, window, response_cb, response):
    '''callback called when a button is selected on the add_contact dialog'''
    contact = window.entry.get_text()
    group = window.combo.get_model().get_value(window.combo.get_active_iter(), 0)
    window.hide()
    response_cb(response, contact, group)
def set_picture_cb(widget, window, response_cb, response, path):
    '''callback called when a button is selected on the set_picture dialog'''
    window.hide()
    response_cb(response, path)
def common_window(message, stock_id, response_cb, title):
    '''create a window that displays a message with a stock image'''
    window = new_window(title, response_cb)
    window_add_image(window, stock_id)
    window_add_label(window, message)
    return window
def message_window(message, stock_id, response_cb, title):
    '''create a window that displays a message with a stock image
    and a close button'''
    window = common_window(message, stock_id, response_cb, title)
    add_button(window, gtk.STOCK_CLOSE, stock.CLOSE, response_cb,
        default_cb)
    return window
def entry_window(message, text, response_cb, title, *args):
    '''create a window that contains a label and a entry with text set
    and selected, and two buttons, accept, cancel'''
    window = new_window(title, response_cb)
    window_add_label(window, message)
    entry = gtk.Entry()
    entry.set_text(text)
    entry.select_region(0, -1)
    entry.connect('activate', entry_cb, window, response_cb, *args)
    window.hbox.pack_start(entry, True, True)
    add_button(window, gtk.STOCK_CANCEL, stock.CANCEL, response_cb, 
        entry_cb, *args)
    add_button(window, gtk.STOCK_OK, stock.ACCEPT, response_cb, 
        entry_cb, *args)
    setattr(window, 'entry', entry)
    entry.show()
    return window
def add_button(window, gtk_stock, stock_id, response_cb,
    callback, *args):
    '''add a button and connect the signal'''
    button = gtk.Button(stock=gtk_stock)
    window.bbox.pack_start(button, True, True)
    button.connect('clicked', callback, window, response_cb,
        stock_id, *args)
    button.show()
    return button
def new_window(title, response_cb, *args):
    '''build a window with the default values and connect the common
    signals, return the window'''
    window = gtk.Window()
    window.set_title(title)
    window.set_default_size(150, 100)
    window.set_position(gtk.WIN_POS_CENTER)
    window.set_border_width(8)
    vbox = gtk.VBox(spacing=4)
    hbox = gtk.HBox(spacing=4)
    bbox = gtk.HButtonBox()
    bbox.set_spacing(4)
    bbox.set_layout(gtk.BUTTONBOX_END)
    vbox.pack_start(hbox, True, True)
    vbox.pack_start(bbox, False)
    window.add(vbox)
    setattr(window, 'vbox', vbox)
    setattr(window, 'hbox', hbox)
    setattr(window, 'bbox', bbox)
    args = list(args)
    args.insert(0, stock.CLOSE)
    window.connect('delete-event', close_cb, window,
        response_cb, *args)
    vbox.show_all()
    return window
def error(message, response_cb=None, title=_("Error!")):
    '''show an error dialog displaying the message, this dialog should
    have only the option to close and the response callback is optional
    since in few cases one want to know when the error dialog was closed,
    but it can happen, so return stock.CLOSE to the callback if its set'''
    message_window(message, gtk.STOCK_DIALOG_ERROR, response_cb, 
        title).show()
def warning(message, response_cb=None, title=_("Warning")):
    '''show a warning dialog displaying the messge, this dialog should
    have only the option to accept, like the error dialog, the response
    callback is optional, but you have to check if it's not None and
    send the response (that can be stock.ACCEPT or stock.CLOSE, if
    the user closed the window with the x)'''
    message_window(message, gtk.STOCK_DIALOG_WARNING, response_cb, 
        title).show()
def information(message, response_cb=None, 
                        title=_("Information"),):
    '''show a warning dialog displaying the messge, this dialog should
    have only the option to accept, like the error dialog, the response
    callback is optional, but you have to check if it's not None and
    send the response (that can be stock.ACCEPT or stock.CLOSE, if
    the user closed the window with the x)'''
    message_window(message, gtk.STOCK_DIALOG_INFO, response_cb, 
        title).show()
def exception(message, response_cb=None, title=_("Exception"),):
    '''show the message of an exception on a dialog, useful to
    connect with sys.excepthook'''
    window = new_window(title, response_cb)
    label = window_add_label(window, message)
    label.set_selectable(True)
    add_button(window, gtk.STOCK_CLOSE, stock.CLOSE, response_cb,
        default_cb)
    window.show()
def yes_no(message, response_cb, *args):
    '''show a confirm dialog displaying a question and two buttons:
    Yes and No, return the response as stock.YES or stock.NO or
    stock.CLOSE if the user closes the window'''
    window = common_window(message, gtk.STOCK_DIALOG_QUESTION, 
        response_cb, _("Confirm"))
    add_button(window, gtk.STOCK_YES, stock.YES, response_cb, 
        default_cb, *args)
    add_button(window, gtk.STOCK_NO, stock.NO, response_cb, 
        default_cb, *args)
    window.show()
def yes_no_cancel(message, response_cb, *args):
    '''show a confirm dialog displaying a question and three buttons:
    Yes and No and Cancel, return the response as stock.YES, stock.NO,
    stock.CANCEL or stock.CLOSE if the user closes the window'''
    window = common_window(message, gtk.STOCK_DIALOG_QUESTION, 
        response_cb, _("Confirm"))
    add_button(window, gtk.STOCK_YES, stock.YES, response_cb, 
        default_cb, *args)
    add_button(window, gtk.STOCK_NO, stock.NO, response_cb, 
        default_cb, *args)
    add_button(window, gtk.STOCK_CANCEL, stock.CANCEL, response_cb, 
        default_cb, *args)
    window.show()
def accept_cancel(message, response_cb, *args):
    '''show a confirm dialog displaying information and two buttons:
    Accept and Cancel, return stock.ACCEPT, stock.CANCEL or 
    stock.CLOSE'''
    window = common_window(message, gtk.STOCK_DIALOG_QUESTION, 
        response_cb, _("Confirm"))
    add_button(window, gtk.STOCK_OK, stock.ACCEPT, response_cb, 
        default_cb, *args)
    add_button(window, gtk.STOCK_CANCEL, stock.CANCEL, response_cb, 
        default_cb, *args)
    window.show()
def contact_added_you(accounts, response_cb, 
                            title=_("User invitation")):
    '''show a dialog displaying information about users
    that added you to their userlists, the accounts parameter is
    a tuple of mails that represent all the users that added you,
    the way you confirm (one or more dialogs) doesn't matter, but
    you should call the response callback only once with a tuple
    like: ((mail1, stock.YES), (mail2, stock.NO), (mail3, stock.CANCEL))
    YES means add him to your userlist, NO means block him, CANCEL
    means remind me later.'''
    raise NotImplementedError("This method isn't implemented")
def add_contact(groups, group, response_cb, title=_("Add user")):
    '''show a dialog asking for an user address, and (optional)
    the group(s) where the user should be added, the response callback
    receives the response type (stock.ADD, stock.CANCEL or stock.CLOSE)
    the account and a tuple of group names where the user should be 
    added (give a empty tuple if you don't implement this feature, 
    the controls are made by the callback, you just ask for the email, 
    don't make any control, you are just implementing a GUI! :P'''
    window = new_window(title, response_cb)
    label = gtk.Label(_("Account"))
    label_align = gtk.Alignment(0.0, 0.5)
    label_align.add(label)
    entry = gtk.Entry()
    group_label = gtk.Label(_("Group"))
    group_label_align = gtk.Alignment(0.0, 0.5)
    group_label_align.add(group_label)
    combo = gtk.combo_box_new_text()
    combo.append_text("")
    groups = list(groups)
    groups.sort()
    selected = 0
    for (index, group_name) in enumerate(groups):
        combo.append_text(group_name)
        if group == group_name:
            selected = index + 1
    combo.set_active(selected)
    table = gtk.Table(2, 2)
    table.attach(label_align, 0, 1, 0, 1)
    table.attach(entry, 1, 2, 0, 1)
    table.attach(group_label_align, 0, 1, 1, 2)
    table.attach(combo, 1, 2, 1, 2)
    table.set_row_spacings(2)
    table.set_col_spacings(8)
    window.hbox.pack_start(table, True, True)
    add_button(window, gtk.STOCK_CANCEL, stock.CANCEL, response_cb, 
        add_contact_cb)
    add_button(window, gtk.STOCK_OK, stock.ACCEPT, response_cb, 
        add_contact_cb)
    setattr(window, 'entry', entry)
    setattr(window, 'combo', combo)
    entry.connect('activate', add_contact_cb, window, response_cb,
        stock.ACCEPT)
    window.show_all()
def add_group(response_cb, title=_("Add group")):
    '''show a dialog asking for a group name, the response callback
    receives the response (stock.ADD, stock.CANCEL, stock.CLOSE)
    and the name of the group, the control for a valid group is made
    on the controller, so if the group is empty you just call the
    callback, to make a unified behaviour, and also, to only implement
    GUI logic on your code and not client logic
    cb args: response, group_name'''
    window = entry_window(_("Group name"), '', response_cb, title)
    window.show()
def set_nick(nick, response_cb, title=_("Change nick")):
    '''show a dialog asking for a new nick and displaying the current
    one, the response_cb receives the old nick, the new nick, 
    and the response (stock.ACCEPT, stock.CANCEL or stock.CLOSE)
    cb args: response, old_nick, new_nick'''
    window = entry_window(_("New nick"), nick, response_cb, title,
    nick)
    window.show()
def set_message(message, response_cb, 
    title=_("Change personal message")):
    '''show a dialog asking for a new personal message and displaying 
    the current one, the response_cb receives the old personal message
    , the new personal message and the response 
    (stock.ACCEPT, stock.CANCEL or stock.CLOSE)
    cb args: response, old_pm, new_pm'''
    window = entry_window(_("New personal message"), 
        message, response_cb, title, message)
    window.show()
def set_picture(path_current, cache_path,  
        response_cb, title=_("Change picture")):
    '''show a dialog asking for the display picture and return the selected
    one, path_current is the path of the current selected picture.
    cache_path is a folder where the avatars are loaded and saved
     Return the response and a new path or None if no new picture 
    is selected'''
    global _avatar_chooser
    def _on_hide(window):
        global _avatar_chooser
        _avatar_chooser = None
    if not _avatar_chooser:
        _avatar_chooser = AvatarChooser(response_cb, 
            path_current, cache_path)
        _avatar_chooser.connect('hide', _on_hide)
        _avatar_chooser.show()
    else:
        _avatar_chooser.grab_focus()
def set_custom_emoticon(ce_dir, response_cb):
    '''show a dalog to create a custome emotion, ce_dir is the
    directory where the image chooser dialog will open'''
    CEChooser(ce_dir, response_cb).show()
def rename_group(name, response_cb, title=_("Rename group")):
    '''show a dialog with the group name and ask to rename it, the
    response callback receives stock.ACCEPT, stock.CANCEL or stock.CLOSE
    the old and the new name.
    cb args: response, old_name, new_name
    '''
    window = entry_window(_("New group name"), name, response_cb, 
        title, name)
    window.show()
def set_contact_alias(account, alias, response_cb, 
                        title=_("Set alias")):
    '''show a dialog showing the current alias and asking for the new
    one, the response callback receives,  the response 
    (stock.ACCEPT, stock.CANCEL, stock.CLEAR <- to remove the alias 
    or stock.CLOSE), the account, the old and the new alias.
    cb args: response, account, old_alias, new_alias'''
    alias = alias or ''
    window = entry_window(_("Contact alias"), alias, response_cb, 
        title, account, alias)
    add_button(window, gtk.STOCK_CLEAR, stock.CLEAR, response_cb,
        entry_cb, account, alias)
    window.show()
def about_dialog(name, version, copyright, comments, license, website,
    authors, translators, logo_path):
    '''show an about dialog of the application:
    * title: the title of the window
    * name: the name of the appliaction
    * version: version as string
    * copyright: the name of the copyright holder
    * comments: a description of the application
    * license: the license text
    * website: the website url
    * authors: a list or tuple of strings containing the contributors
    * translators: a string containing the translators
    '''
    def close_about(widget, response_id):
        if response_id == gtk.RESPONSE_CANCEL:
            widget.destroy()
    about = gtk.AboutDialog()
    about.set_name(name)
    about.set_version(version)
    about.set_copyright(copyright)
    about.set_comments(comments)
    about.connect('response', close_about)
    about.set_license(license)
    about.set_website(website)
    about.set_authors(authors)
    about.set_translator_credits(translators)
    icon = gtk.image_new_from_file(logo_path)
    about.set_icon(icon)
    about.set_logo(icon)
    about.run()
def _callback(*args):
    '''a callback that print all the arguments'''
    print(args)
def _test():
    '''a test method'''
    '''error("Error!", _callback)
    warning("Warning!", _callback)
    information("Information", _callback)
    yes_no("to_be || !to_be", _callback, "foo")
    yes_no_cancel("to_be or not to_be", _callback, "bar")
    accept_cancel("Silly question", _callback, "baz")
    add_contact(('c', 'b', 'a'), _callback)
    add_group(_callback)
    set_nick("my other nick is a ferrari", _callback)
    set_message("too personal", _callback)
    rename_group("Groupal", _callback)
    set_contact_alias("dude@live.com", "some dude", _callback)
    exception("*implodes*")'''
    set_picture('', _callback)
    gtk.main()
import Widgets
class AddBuddy(gtk.Window):
    '''Confirm dialog informing that someone has added you
    ask if you want to add him to your contact list'''
    def __init__(self, controller):
        '''Constructor. Packs widgets'''
        gtk.Window.__init__(self)
        self.mails = []  # [(mail, nick), ...]
        self.pointer = 0
        self.controller = controller
        self.set_title(_("Add contact"))
        self.set_border_width(4)
        self.move(30, 30) # top-left
        self.connect('delete-event', self.cb_delete)
        self.vbox = gtk.VBox()
        self.hbox = gtk.HBox()
        self.hbox.set_spacing(4)
        self.hbox.set_border_width(4)
        self.image = gtk.Image()
        self.image.set_from_stock(gtk.STOCK_DIALOG_QUESTION, \
            gtk.ICON_SIZE_DIALOG)
        self.imagebox = gtk.HBox()
        self.imagebox.set_border_width(4)
        self.image.set_alignment(0.0, 0.5)
        self.vboxtext = gtk.VBox()
        self.pages = self._buildpages()
        self.text = gtk.Label()
        self.text.set_selectable(True)
        self.text.set_ellipsize(3) #pango.ELLIPSIZE_END
        self.text.set_alignment(0.0, 0.0) # top left
        self.text.set_width_chars(60)
        self.hboxbuttons = gtk.HBox()
        self.hboxbuttons.set_spacing(4)
        self.hboxbuttons.set_border_width(4)
        self.buttonbox = gtk.HButtonBox()
        self.buttonbox.set_layout(gtk.BUTTONBOX_END)
        self.later = gtk.Button()
        self.later.add(gtk.Label(_('Remind me later')))
        self.later.connect('clicked', self.cb_cancel)
        self.profile = gtk.Button()
        self.profile.add(gtk.Label(_('View profile')))
        self.profile.connect('clicked', self.cb_profile)
        self.addbutton = gtk.Button(stock=gtk.STOCK_ADD)
        self.addbutton.connect('clicked', self.cb_add)
        self.add(self.vbox)
        self.vbox.pack_start(self.hbox, True, True)
        self.vbox.pack_start(self.hboxbuttons, False, False)
        self.imagebox.pack_start(self.image)
        self.hbox.pack_start(self.imagebox, False, False)
        self.hbox.pack_start(self.vboxtext, True, True)
        self.vboxtext.pack_start(self.pages, False, False)
        self.vboxtext.pack_start(self.text, True, True)
        self.hboxbuttons.pack_start(self.later, False, False)
        self.hboxbuttons.pack_start(self.profile, False, False)
        self.hboxbuttons.pack_start(self.buttonbox)
        self.buttonbox.pack_start(self.addbutton)
    def _buildpages(self):
        '''Builds hboxpages, that is a bit complex to include in __init__'''
        hboxpages = gtk.HBox()
        arrowleft = Widgets.TinyArrow(gtk.ARROW_LEFT)
        self.buttonleft = gtk.Button()
        self.buttonleft.set_relief(gtk.RELIEF_NONE)
        self.buttonleft.add(arrowleft)
        self.buttonleft.connect('clicked', self.switchmail, -1)
        arrowright = Widgets.TinyArrow(gtk.ARROW_RIGHT)
        self.buttonright = gtk.Button()
        self.buttonright.set_relief(gtk.RELIEF_NONE)
        self.buttonright.add(arrowright)
        self.buttonright.connect('clicked', self.switchmail, 1)
        self.currentpage = gtk.Label()
        hboxpages.pack_start(gtk.Label(), True, True) # align to right
        hboxpages.pack_start(self.buttonleft, False, False)
        hboxpages.pack_start(self.currentpage, False, False)
        hboxpages.pack_start(self.buttonright, False, False)
        return hboxpages
    def append(self, nick, mail):
        '''Adds a new pending user'''
        self.mails.append((mail, gobject.markup_escape_text(nick)))
        self.update()
        self.show_all()
        self.present()
    def update(self):
        '''Update the GUI, including labels, arrow buttons, etc'''
        try:
            mail, nick = self.mails[self.pointer]
        except IndexError:
            self.hide()
            return
        if nick != mail:
            mailstring = "<b>%s</b>\n<b>(%s)</b>" % (nick, mail)
        else:
            mailstring = '<b>%s</b>' % mail
        self.text.set_markup(mailstring + _(' has added you.\n'
            'Do you want to add him/her to your contact list?'))
        self.buttonleft.set_sensitive(True)
        self.buttonright.set_sensitive(True)
        if self.pointer == 0:
            self.buttonleft.set_sensitive(False)
        if self.pointer == len(self.mails) - 1:
            self.buttonright.set_sensitive(False)
        self.currentpage.set_markup('<b>(%s/%s)</b>' % \
            (self.pointer + 1, len(self.mails)))
    def switchmail(self, button, order):
        '''Moves the mail pointer +1 or -1'''
        if (self.pointer + order) >= 0:
            if (self.pointer + order) < len(self.mails):
                self.pointer += order
            else:
                self.pointer = 0
        else:
            self.pointer = len(self.mails) - 1
        self.update()
    def hide(self):
        '''Called to hide the window'''
        gtk.Window.hide(self)
        self.controller.addBuddy = None
    def cb_delete(self, *args):
        '''Callback when the window is destroyed'''
        self.controller.addBuddy = None
        self.destroy()
    def cb_cancel(self, button):
        '''Callback when the cancel button is clicked'''
        self.mails.pop(self.pointer)
        self.switchmail(None, -1)
        self.update()
    def cb_profile(self, button):
        '''Callback when the view profile button is clicked'''
        self.controller.seeProfile(self.mails[self.pointer][0])
    def cb_add(self, button):
        '''Callback when the add button is clicked'''
        mail, nick = self.mails[self.pointer]
        self.controller.contacts.add(mail)
        self.cb_cancel(None)
class AvatarChooser(gtk.Window):
    '''A dialog to choose an avatar'''
    def __init__(self, response_cb, picture_path='', 
            cache_path='.'):
        '''Constructor, response_cb receive the response number, the new file 
        selected and a list of the paths on the icon view.
        picture_path is the path of the current display picture, 
        '''
        gtk.Window.__init__(self)
        self.response_cb = response_cb
        self.cache_path = cache_path
        self.set_title(_("Avatar chooser"))
        self.set_default_size(600, 400)
        self.set_border_width(4)
        self.set_position(gtk.WIN_POS_CENTER)
        self.model = gtk.ListStore(gtk.gdk.Pixbuf, str)
        self.view = gtk.IconView(self.model)
        self.view.enable_model_drag_dest([('text/uri-list', 0, 0)], gtk.gdk.ACTION_DEFAULT | gtk.gdk.ACTION_COPY)
        self.view.connect("drag-data-received", self._drag_data_received)
        self.view.set_pixbuf_column(0)
        self.view.connect("item-activated", self._on_icon_activated)
        scroll = gtk.ScrolledWindow()
        scroll.set_policy(gtk.POLICY_NEVER, gtk.POLICY_AUTOMATIC)
        scroll.set_shadow_type(gtk.SHADOW_IN)
        scroll.add(self.view)
        vbox = gtk.VBox(spacing=4)
        side_vbox = gtk.VBox(spacing=4)
        hbox = gtk.HBox(spacing=4)
        hbbox = gtk.HButtonBox()
        hbbox.set_spacing(4)
        hbbox.set_layout(gtk.BUTTONBOX_END)
        vbbox = gtk.VButtonBox()
        vbbox.set_spacing(4)
        vbbox.set_layout(gtk.BUTTONBOX_START)
        b_clear = gtk.Button(_("No picture"))
        b_add = gtk.Button(stock=gtk.STOCK_ADD)
        b_remove = gtk.Button(stock=gtk.STOCK_REMOVE)
        b_remove_all = gtk.Button(_("Remove all"))
        b_accept = gtk.Button(stock=gtk.STOCK_OK)
        b_cancel = gtk.Button(stock=gtk.STOCK_CANCEL)
        b_clear.connect('clicked', self._on_clear)
        b_add.connect('clicked', self._on_add)
        b_remove.connect('clicked', self._on_remove)
        b_remove_all.connect('clicked', self._on_remove_all)
        b_accept.connect('clicked', self._on_accept)
        b_cancel.connect('clicked', self._on_cancel)
        self.connect('delete-event', self._on_close)
        self.img_current = gtk.Image()
        self.img_current.set_size_request(96, 96)
        frame_current = gtk.Frame(_("Current"))
        frame_current.add(self.img_current)
        hbbox.pack_start(b_clear, False)
        hbbox.pack_start(b_cancel, False)
        hbbox.pack_start(b_accept, False)
        vbbox.pack_start(b_add, False)
        vbbox.pack_start(b_remove, False)
        vbbox.pack_start(b_remove_all, False)
        side_vbox.pack_start(frame_current, False)
        side_vbox.pack_start(vbbox)
        hbox.pack_start(scroll, True, True)
        hbox.pack_start(side_vbox, False, False)
        vbox.pack_start(hbox, True, True)
        vbox.pack_start(hbbox, False)
        vbox.show_all()
        self.add(vbox)
        self.fill()
        self.set_current_picture(picture_path)
    def set_current_picture(self, path):
        '''set the current picture on the frame'''
        if os.path.exists(path):
            pixbuf = gtk.gdk.pixbuf_new_from_file(path)
            self.img_current.set_from_pixbuf(pixbuf)
    def get_selected(self):
        '''return a tuple (pixbuf, path) of the selection, or None'''
        iter = self.get_selected_iter()
        if iter:
            return self.model[iter]
        return None
    def get_selected_iter(self):
        '''return the selected iter or None'''
        if len(self.view.get_selected_items()) > 0:
            item = self.view.get_selected_items()[0]
            return self.model.get_iter(item)
        return None
    def get_iter_from_filename(self, path):
        '''return the iter of a filename or None'''
        for row in self.model:
            (pixbuf, filename) = row
            if os.path.samefile(filename, path):
                return row.iter
        return None
    def fill(self):
        '''fill the IconView with avatars from the list of pictures'''
        for path in os.listdir(self.cache_path):
            if not os.path.splitext(path)[0].endswith('_thumb'):
                self.add_picture(os.path.join(self.cache_path, path))
    def is_in_view(self, filename):
        '''return True if filename already on the iconview'''
        if os.name == 'nt':
            return False
        for (pixbuf, path) in self.model:
            if os.path.samefile(filename, path):
                return True
        return False
    def add_picture(self, path):
        '''Adds an avatar into the IconView'''
        try:
            if os.path.exists(path) and os.access(path, os.R_OK)\
                    and not self.is_in_view(path):
                pixbuf = gtk.gdk.pixbuf_new_from_file(path)
                self.model.append([pixbuf, path])
            else:
                print path, 'not readable'
        except gobject.GError:
            print 'image at %s could not be loaded'
            print gobject.GError
    def remove(self, path):
        '''remove the avatar in path'''
        del self.model[self.get_iter_from_filename(path)]
        try:
            os.remove(path)
            parts = os.path.splitext(path)
            os.remove(parts[0] + "_thumb" + parts[1])
        except Exception, e:
            print "could not remove", path
    def remove_selected(self):
        '''Removes avatar from a TreeIter'''
        selected = self.get_selected()
        if selected:
            (pixbuf, path) = selected
            self.remove(path)
    def remove_all(self):
        '''remove all the items on the view'''
        for (pixbuf, path) in self.model:
            self.remove(path)
        self.model.clear()
    def _drag_data_received(self, treeview, context, x, y, selection, info, 
                             timestamp):
        '''method called on an image dragged to the view'''
        urls = selection.data.split('\n')
        for url in urls:
            path = url.replace('file://', '')
            path = path.replace('\r', '')
            try:
                if os.path.exists(path):
                    self.add_picture(path)
            except TypeError, e:
                error(_("Could not add picture:\n %s") % (str(e),))
    def _on_icon_activated(self, *args):
        '''method called when a picture is double clicked'''
        self._on_accept(None)
    def _on_add(self, button):
        '''called when the user select the add button'''
        def _on_image_selected(response, path):
            '''method called when an image is selected'''
            if response == stock.ACCEPT:
                try:
                    av = Avatar.Avatar(path, self.cache_path, resizeDialog=True)
                    filename = av.getImagePath()
                    self.add_picture(av.getImagePath())
                except Exception, e:
                    error(str(e))
        ImageChooser(os.path.expanduser('~'), _on_image_selected).show() 
    def _on_remove(self, event):
        '''Removes the selected avatar'''
        self.remove_selected()
    def _on_remove_all(self, button):
        '''Removes all avatars from the cache'''
        def on_response_cb(response):
            '''response callback for the confirm dialog'''
            if response == stock.YES:
                self.remove_all()
        yes_no(_("Are you sure you want to remove all items?"),
            on_response_cb)
    def _on_accept(self, button):
        '''method called when the user clicks the button'''
        selected = self.get_selected()
        filename = ''
        if selected:
            filename = selected[1]
            self.hide()
            self.response_cb(stock.ACCEPT, filename)
        else:
            error(_("No picture selected"))
    def _on_cancel(self, button):
        '''method called when the user clicks the button'''
        self.hide()
        self.response_cb(stock.CANCEL, '')
    def _on_clear(self, button):
        '''method called when the user clicks the button'''
        self.hide()
        self.response_cb(stock.CLEAR, '')
    def _on_close(self, window, event):
        '''called when the user click on close'''
        self.hide()
        self.response_cb(stock.CLOSE, '')
class ImageChooser(gtk.Window):
    '''a class to select images'''
    def __init__(self, path, response_cb):
        '''class constructor, path is the directory where the
        dialog opens'''
        gtk.Window.__init__(self)
        self.response_cb = response_cb
        self.set_title(_("Image Chooser"))
        self.set_default_size(600, 400)
        self.set_border_width(4)
        self.set_position(gtk.WIN_POS_CENTER)
        self.vbox = gtk.VBox(spacing=4)
        self.file_chooser = gtk.FileChooserWidget()
        self.file_chooser.set_current_folder(path)
        hbbox = gtk.HButtonBox()
        hbbox.set_spacing(4)
        hbbox.set_layout(gtk.BUTTONBOX_END)
        b_accept = gtk.Button(stock=gtk.STOCK_OK)
        b_cancel = gtk.Button(stock=gtk.STOCK_CANCEL)
        b_accept.connect('clicked', self._on_accept)
        b_cancel.connect('clicked', self._on_cancel)
        self.connect('delete-event', self._on_close)
        hbbox.pack_start(b_cancel, False)
        hbbox.pack_start(b_accept, False)
        vbox = gtk.VBox()
        self.vbox.pack_start(self.file_chooser, True, True)
        vbox.add(self.vbox)
        vbox.pack_start(hbbox, False)
        self.add(vbox)
        vbox.show_all()
        self._add_filters()
        self._add_preview()
    def _add_filters(self):
        '''
        Adds all the possible file filters to the dialog. The filters correspond
        to the gdk available image formats
        '''
        all_files = gtk.FileFilter()
        all_files.set_name(_('All files'))
        all_files.add_pattern('*')
        all_images = gtk.FileFilter()
        all_images.set_name(_( 'All images'))
        filters = []
        formats = gtk.gdk.pixbuf_get_formats()
        for format in formats:
            filter = gtk.FileFilter()
            name = "%s (*.%s)" % (format['description'], format['name'])
            filter.set_name(name)
            for mtype in format['mime_types']:
                filter.add_mime_type(mtype)
                all_images.add_mime_type(mtype)
            for pattern in format['extensions']:
                tmp = '*.' + pattern
                filter.add_pattern(tmp)
                all_images.add_pattern(tmp)
            filters.append(filter)
        self.file_chooser.add_filter(all_files)
        self.file_chooser.add_filter(all_images)
        self.file_chooser.set_filter(all_images)
        for filter in filters:
            self.file_chooser.add_filter(filter)
    def _add_preview(self):
        '''
        Adds a preview widget to the file chooser
        '''
        self.image = gtk.Image()
        self.image.set_size_request(128, 128)
        self.image.show()
        self.file_chooser.set_preview_widget(self.image)
        self.file_chooser.set_preview_widget_active(True)
        self.file_chooser.connect('update-preview', self._on_update_preview)
    def _on_accept(self, button):
        '''method called when the user clicks the button'''
        filename = get_filename(self)
        if os.path.isfile(filename):
            self.hide()
            self.response_cb(stock.ACCEPT, filename)
        else:
            error(_("No picture selected"))
    def _on_cancel(self, button):
        '''method called when the user clicks the button'''
        self.hide()
        self.response_cb(stock.CANCEL, get_filename(self))
    def _on_close(self, window, event):
        '''called when the user click on close'''
        self.hide()
        self.response_cb(stock.CLOSE, get_filename(self))
    def _on_update_preview(self, filechooser):
        '''
        Updates the preview image
        '''
        hasPreview = False
        path = get_preview_filename(self)
        if path:
            if os.path.isfile(path) and os.path.getsize(path) <= 1000000:
                pixbuf = gtk.gdk.pixbuf_new_from_file(get_filename(self))
                if pixbuf.get_width() > 128 and pixbuf.get_height() > 128:
                    pixbuf = pixbuf.scale_simple(128, 128, 
                        gtk.gdk.INTERP_BILINEAR)
                self.image.set_from_pixbuf(pixbuf)
            else:
                self.image.set_from_stock(gtk.STOCK_DIALOG_ERROR, 
                    gtk.ICON_SIZE_DIALOG)
class CEChooser(ImageChooser):
    '''a dialog to create a custom emoticon'''
    SMALL = _("small (16x16)")
    BIG = _("big (50x50)")
    def __init__(self, path, response_cb):
        '''class constructor'''
        ImageChooser.__init__(self, path, None)
        self.response_cb = response_cb
        label = gtk.Label(_("Shortcut"))
        self.shortcut = gtk.Entry()
        self.combo = gtk.combo_box_new_text()
        self.combo.append_text(CEChooser.SMALL)
        self.combo.append_text(CEChooser.BIG)
        self.combo.set_active(0)
        hbox = gtk.HBox()
        hbox.add(label)
        hbox.add(self.shortcut)
        hbox.add(self.combo)
        self.vbox.pack_start(hbox, False)
        hbox.show_all()
    def _on_accept(self, button):
        '''method called when the user clicks the button'''
        filename = get_filename(self)
        shortcut = self.shortcut.get_text()
        size = self.combo.get_model().get_value(self.combo.get_active_iter(), 0)
        if os.path.isfile(filename):
            if not shortcut:
                error(_("Empty shortcut"))
            else:
                self.hide()
                self.response_cb(stock.ACCEPT, filename, shortcut, size)
        else:
            error(_("No picture selected"))
    def _on_cancel(self, button):
        '''method called when the user clicks the button'''
        self.hide()
        self.response_cb(stock.CANCEL, None, None, None)
    def _on_close(self, window, event):
        '''called when the user click on close'''
        self.hide()
        self.response_cb(stock.CLOSE, None, None, None)
def get_filename(self):
    '''Shortcut to get a properly-encoded filename from a file chooser'''
    filename = self.file_chooser.get_filename()
    if filename and gtk.gtk_version >= (2, 10, 0):
        return gobject.filename_display_name(filename)
    else:
        return filename
def get_preview_filename(self):
    '''Shortcut to get a properly-encoded preview filename'''
    filename = self.file_chooser.get_preview_filename()
    if filename and gtk.gtk_version >= (2, 10, 0):
        return gobject.filename_display_name(filename)
    else:
        return filename
if __name__ == '__main__':
    ac = AvatarChooser(_callback, '/home/mariano/default/icon96.png', '/home/mariano/default/', [])
    ac.show()
    gtk.main()
