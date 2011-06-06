import gtk
import emesenelib.common
class StatusMenu(gtk.Menu):
    '''This class represents the status menu where you can select a given
    status and it will be changed. This is a standalone class because
    it is used in two classes (MainMenu and TrayIcon)'''
    def __init__(self, controller):
        '''Contructor'''
        gtk.Menu.__init__(self)
        self.controller = controller
        self.theme = self.controller.theme
        j = 0
        for i in self.controller.status_ordered[0]:
            if i != "FLN":
                menuItem = self.newImageMenuItem (
                    self.controller.status_ordered[3][j], None,
                    self.controller.theme.statusToPixbuf(i))
                self.add(menuItem)
                menuItem.connect("activate", self.activate, i)
                j += 1
    def newImageMenuItem(self, label, stock = None, img = None):
        mi = gtk.ImageMenuItem(_(label))
        if stock:
            mi.set_image(gtk.image_new_from_stock(stock, gtk.ICON_SIZE_MENU))
        elif img:
            image = gtk.Image()
            image.set_from_pixbuf(img)
            mi.set_image(image)
        return mi
    def newCheckMenuItem(self, label, checked):
        mi = gtk.CheckMenuItem(_(label))
        mi.set_active(checked)
        return mi
    def activate(self, menuitem, status):
        '''change the status with the userparam'''
        self.controller.contacts.set_status(status)
