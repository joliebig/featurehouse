import os
import shutil
import time
import glob
import gtk
import gobject
from dialog import yes_no
from abstract import stock
def hasCachedAvatars(contactMail, cachePath):
    pattern = os.path.join(cachePath,contactMail.split("@")[0])
    if len(glob.glob(pattern+'*')) > 0:
        return True
def getCachedAvatars(contactMail, cachePath):
    '''Returns an ordered list containing [modified_time, path]
    for each cached avatar of a user. Last modified first'''
    pattern = os.path.join(cachePath,contactMail.split("@")[0])
    avatarList=[]
    for file in glob.glob(pattern+'*'):
        last_mod_date = os.stat(file)[8]
        avatarList.append([last_mod_date,file])
    avatarList.sort(reverse=True)
    return avatarList
def getLastCachedAvatar(contactMail, cache_path):
    '''Returns the path of the last cached avatar of a user'''
    avList = getCachedAvatars(contactMail, cache_path)
    if len(avList) > 0:
        return avList[0][1]
    else:
        return ""
class AvatarHistoryViewer(gtk.Window):
    '''A dialog to view old contact avatars'''
    def __init__(self, controller,
            cache_path,  contactMail):
        '''Constructor'''
        gtk.Window.__init__(self)
        self.controller = controller
        self.cache_path = cache_path
        self.contactMail = contactMail
        self.set_title(_("%s avatars" % self.contactMail))
        self.set_default_size(570, 400)
        self.set_border_width(4)
        self.set_position(gtk.WIN_POS_CENTER)
        self.model = gtk.ListStore(gtk.gdk.Pixbuf, str, str)
        self.fill()
        self.view = gtk.IconView(self.model)
        self.view.set_pixbuf_column(0)
	if os.name == "posix":
            self.view.set_tooltip_column(2)
        self.view.set_text_column(2)
        self.view.connect("button-press-event", self._on_button_press_event)
        self.tooltips = gtk.Tooltips()
        self.tooltips.enable()
        scroll = gtk.ScrolledWindow()
        scroll.set_policy(gtk.POLICY_NEVER, gtk.POLICY_AUTOMATIC)
        scroll.set_shadow_type(gtk.SHADOW_IN)
        scroll.add(self.view)
        comboEmails = gtk.combo_box_new_text()
        clist = sorted(set(self.controller.contacts.contacts))
        for contact in clist:
            comboEmails.append_text(contact)
        comboEmails.set_active(clist.index(self.contactMail))
        comboEmails.connect('changed',self._on_email_changed)
        vbox = gtk.VBox(spacing=4)
        hbox = gtk.HBox(spacing=4)
        hbbox = gtk.HButtonBox()
        b_close = gtk.Button(stock=gtk.STOCK_CLOSE)
        b_close.connect('clicked', self._on_close_button)
        hbbox.pack_start(comboEmails, False)
        hbbox.pack_end(b_close, False)
        hbox.pack_start(scroll, True, True)
        vbox.pack_start(hbox, True, True)
        vbox.pack_start(hbbox, False)
        vbox.show_all()
        self.add(vbox)
    def fill(self):
        '''fill the IconView with avatars from the list of pictures'''
        userAvatars = getCachedAvatars(self.contactMail, self.cache_path)
        for last_mod_date,file in userAvatars:
                self.add_picture(file,time.strftime("%d/%m/%Y", \
                    time.localtime(last_mod_date)))
    def add_picture(self, path, date):
        '''Adds an avatar into the IconView'''
        try:
            if os.path.exists(path) and os.access(path, os.R_OK)\
                    and not self.is_in_view(path):
                pixbuf = gtk.gdk.pixbuf_new_from_file(path)
                self.model.append([pixbuf, path, date])
            else:
                print path, 'not readable'
        except gobject.GError:
            print 'image at %s could not be loaded'
            print gobject.GError
    def samefile(self, path1, path2):
        '''return True if the files are the same file 
        this is a workaround to os.path.samefile that doesn't exist
        on windows'''
        path1 = os.path.abspath(os.path.normpath(path1))
        path2 = os.path.abspath(os.path.normpath(path2))
        return ((hasattr(os.path, 'samefile') and \
           os.path.samefile(path1, path2)) or \
           (path1.lower() == path2.lower()))
    def is_in_view(self, filename):
        '''return True if filename already on the iconview'''
        if os.name == 'nt':
            return False
        for (pixbuf, path,date) in self.model:
            if os.path.samefile(filename, path):
                return True
        return False
    def _on_button_press_event(self, treeview, event):
        '''callback called when the user press a button over an avatar'''
        if event.button == 3:
            model_path = self.view.get_path_at_pos(int(event.x), int(event.y))
            if model_path:
                iterator = self.model.get_iter(model_path)
                self.view.select_path(model_path)
                menu = AvatarHistoryMenu(self.controller,self.model,iterator)
                menu.popup(None, None, None, event.button, event.time)
    def _on_close_button(self, button):
        '''method called when the user clicks the button'''
        self.hide()
    def _on_close(self, window, event):
        '''called when the user click on close'''
        self.hide()
        self.response_cb(stock.CLOSE, '')
    def _on_email_changed(self, combobox):
        '''called when the user changes the email combobox'''
        newEmail = combobox.get_active_text()
        self.set_title(_("%s avatars" % newEmail))
        self.contactMail = newEmail
        self.view.set_model(None)
        self.model.clear()
        self.fill()
        self.view.set_model(self.model)
class AvatarHistoryMenu(gtk.Menu):
    '''This class represents the popup menu that is displayed when you right
    click an avatar'''
    def __init__(self,controller, model,iter):
        gtk.Menu.__init__(self)
        self.avatarIter = iter
        self.model = model
        self.controller = controller
        self.avatarPath = self.model.get_value(iter, 1)
        setAvatarMenuItem = self.newImageMenuItem(_("Set as your _avatar"),
            gtk.STOCK_ADD)
        self.add(setAvatarMenuItem)
        setAvatarMenuItem.connect("activate", self.on_set_yours)
        setAvatarMenuItem.show()
        saveAsMenuItem = self.newImageMenuItem(_("_Save image as"),
            gtk.STOCK_SAVE_AS)
        self.add(saveAsMenuItem)
        saveAsMenuItem.connect("activate", self.on_save_as)
        saveAsMenuItem.show()
        deleteMenuItem = self.newImageMenuItem( \
            _("_Delete this avatar"), gtk.STOCK_DELETE)
        self.add(deleteMenuItem)
        deleteMenuItem.show()
        deleteMenuItem.connect("activate", self.on_delete_avatar)
        deleteAllMenuItem = self.newImageMenuItem( \
            _("D_elete all avatars"), gtk.STOCK_DELETE)
        self.add(deleteAllMenuItem)
        deleteAllMenuItem.show()
        deleteAllMenuItem.connect("activate", self.on_delete_all)
        self.show_all()
    def newImageMenuItem(self, label, stock=None, img=None, animation=None):
        mi = gtk.ImageMenuItem(_(label))
        if stock:
            mi.set_image(gtk.image_new_from_stock(stock, gtk.ICON_SIZE_MENU))
        elif img:
            image = gtk.Image()
            image.set_from_pixbuf(img)
            mi.set_image(image)
        elif animation:
            image = gtk.Image()
            image.set_from_animation(animation)
            mi.set_image(image)
        return mi
    def on_set_yours(self, *args):
        self.controller.changeAvatar(self.avatarPath)
    def on_save_as(self, *args):
        dialog = gtk.FileChooserDialog(_('Save image as'),action=gtk.FILE_CHOOSER_ACTION_SAVE,
                             buttons=(gtk.STOCK_CANCEL,gtk.RESPONSE_CANCEL,gtk.STOCK_SAVE,gtk.RESPONSE_OK))
        dialog.set_current_name(os.path.split(self.avatarPath)[1])
        if dialog.run() == gtk.RESPONSE_OK:
            shutil.copy2(self.avatarPath, dialog.get_filename())
        dialog.destroy()
    def on_delete_avatar(self, *args):
        def on_response_cb(response):
            '''response callback for the confirm dialog'''
            if response == stock.YES:
                self.model.remove(self.avatarIter)
                os.remove(self.avatarPath)
        yes_no(_('Are you sure?'),on_response_cb)
    def on_delete_all(self, *args):
        def on_response_cb(response):
            '''response callback for the confirm dialog'''
            if response == stock.YES:
                for row in self.model:
                       os.remove(row[1])
                self.model.clear()
        yes_no(_('Are you sure?'),on_response_cb)
