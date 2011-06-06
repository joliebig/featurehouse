import gtk
class GroupMenu(gtk.Menu):
    '''This class represent the popup menu that is displayed when you right click a group on
    the userList'''
    def __init__(self, controller, group):
        gtk.Menu.__init__(self)
        self.controller = controller
        if group.id != 'nogroup':
            renameGroupMenuItem = self.newImageMenuItem\
                        (_('Re_name group...'), gtk.STOCK_EDIT)
            self.add(renameGroupMenuItem)
            deleteGroupMenuItem = self.newImageMenuItem\
                        (_('Re_move group'), gtk.STOCK_DELETE)
            self.add(deleteGroupMenuItem)
            self.add(gtk.SeparatorMenuItem())
            deleteGroupMenuItem.connect('activate', self.on_delete_group_activate)
            renameGroupMenuItem.connect('activate', self.on_rename_group_activate)
        addUserGroupMenuItem = self.newImageMenuItem\
                    (_('_Add contact...'), gtk.STOCK_ADD)
        self.add(addUserGroupMenuItem)
        addGroupMenuItem = self.newImageMenuItem\
                    (_('Add _group...'), gtk.STOCK_ADD)
        self.add(addGroupMenuItem)
        addUserGroupMenuItem.connect('activate', self.on_add_user_group_activate)
        addGroupMenuItem.connect('activate', self.on_add_group_activate)
        self.show_all()
        self.group = group
    def newImageMenuItem(self, label, stock = None, img = None):
        mi = gtk.ImageMenuItem(_(label))
        if stock != None:
            mi.set_image(gtk.image_new_from_stock(stock, gtk.ICON_SIZE_MENU))
        elif img != None:
            image = gtk.Image()
            image.set_from_pixbuf(img)
            mi.set_image(image)
        return mi
    def on_add_user_group_activate(self, *args):
        self.controller.addUserDialog(self.group.name)
    def on_add_group_activate(self, *args):
        self.controller.groups.add_dialog()
    def on_delete_group_activate(self, *args):
        self.controller.groups.remove(self.group.name)
    def on_rename_group_activate(self, *args):
        self.controller.groups.rename_dialog(self.group.name)
