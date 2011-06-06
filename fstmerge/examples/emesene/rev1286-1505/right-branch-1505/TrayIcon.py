import os
import gtk
disabled = False
type_ = 'gtk'
if not gtk.check_version( 2, 10, 0 ) == None:
    if os.name == 'posix':
        try:
            import egg.trayicon
            type_ = 'egg'
            disabled = False
        except:
            print 'No tray icon library detected'
            disabled = True
    elif os.name == 'nt':
        try:
            from gtkwin32 import *
            WM_LBUTTONUP = 0x0202
            WM_RBUTTONUP = 0x0205
            type_ = 'win'
            disabled = False
        except:
            print 'No tray icon library detected'
            disabled = True
class TrayIcon:
    '''This class creates the tray icon notification - Pre-GTK 2.10'''
    def __init__(self, controller):
        '''Constructor'''
        global disabled
        self.controller = controller
        self.config = self.controller.config
        self.mainWindow = self.controller.mainWindow
        self.theme = self.controller.theme
        self.status = ''
        try:
            if disabled:
                pass
            elif type_ == 'gtk':
                self.tray = gtk.StatusIcon()
                self.tray.set_tooltip( 'emesene' )
                pixbuf = self.theme.getImage('trayicon')
                self.tray.set_from_pixbuf( pixbuf )
                self.buildMenu()
                self.tray.hide = lambda: self.tray.set_visible( False )
                self.tray.show = lambda: self.tray.set_visible( True )
                self.tray.connect( 'activate', self.on_activate )
                self.tray.connect( 'popup-menu', self.on_popup_menu )
            elif os.name == 'posix':
                self.tray = egg.trayicon.TrayIcon('emesene')
                self.buildTrayIconPosix()
                self.buildMenu()
            elif os.name == 'nt':
                self.mainWindow.realize()
                self.win32ext = GTKWin32Ext(self.mainWindow)
                self.buildMenu()
                self.buildTrayIconWin32()
        except Exception, e:
            print 'exception creating trayicon: ' + str( e )
            disabled = True
    def remove(self):
        '''remove the trayicon'''
        if os.name == 'nt': 
            self.win32ext.remove_notify_icon()
    def buildTrayIconPosix(self):
        '''Build the trayIcon for linux'''
        self.eventBox = gtk.EventBox()
        pixbuf = self.theme.getImage('trayicon')
        self.image = gtk.Image()
        self.image.set_from_pixbuf( pixbuf )
        self.eventBox.set_events(gtk.gdk.BUTTON_PRESS_MASK)
        self.eventBox.connect_object('button_press_event', self.iconClickPosix, self.eventBox)
        self.tooltips = gtk.Tooltips()
        self.tooltips.set_tip(self.eventBox, 'Emesene')
        self.eventBox.add(self.image)
        self.tray.add(self.eventBox)
        self.eventBox.show_all()
        self.tray.show_all()
    def update(self, newUserStatus, pixbuf=None):
        self.status = newUserStatus
        if type_ == 'gtk':
            func = self.tray.set_from_pixbuf
        elif type_ == 'egg':
            func = self.image.set_from_pixbuf
        if pixbuf == None:
            func(self.theme.statusToPixbuf(newUserStatus))
        else:
            func(pixbuf)
        if self.controller.msn and self.controller.userEmail:
            text = 'Emesene - ' + str(self.controller.userEmail)
            if type_ == 'egg':
                self.tooltips.set_tip(self.eventBox, text)
            elif type_ == 'gtk':
                self.tray.set_tooltip(text)
    def buildTrayIconWin32(self):
        '''Build the trayIcon for windows'''
        hicon = win32gui.ExtractIcon(0, 'themes\\' + \
            self.config.user['theme'] + '\\' + 'trayicon.ico', 0)
        self.win32ext.add_notify_icon(hicon, 'Emesene') # TODO: account
        self.win32ext.notify_icon.menu = self.menu
        self.win32ext.message_map({WM_TRAYMESSAGE: self.iconClickWin32})
    def iconClickWin32(self, hwnd, message, wparam, lparam):
        '''the event handler for windows'''
        if lparam == WM_RBUTTONUP:
            self.win32ext.notify_icon.menu.popup(None, None, None, 0, 0)
        elif lparam == WM_LBUTTONUP:
            self.win32ext.notify_icon.menu.popdown()
            self.showHide()
    def iconClickPosix(self, widget, event):
        '''the event handler for linux'''
        if event.type == gtk.gdk.BUTTON_PRESS: # Single click
            if event.button == 1: # Left Click
                self.showHide(self.eventBox)
            elif event.button == 3: # Right Click - Show popup
                self.menu.popup(None, None, None, event.button, event.time)
    def on_popup_menu( self, status_icon, button, activate_time ):
        self.menu.popup( None, None, None, button, activate_time )
    def on_activate( self, status_icon ):
        self.showHide()
    def buildMenu(self):
        '''Build the menu widget'''
        self.menu = gtk.Menu()
        menuItem = gtk.ImageMenuItem( gtk.STOCK_QUIT )
        menuItem.connect('activate', self.on_quit)
        self.menu.append(menuItem)
        self.menu.show_all()
    def on_quit( self, menuitem):
        self.controller.quit( 0 )
    def showHide(self, widget = None):
        '''Show or hide the main window'''
        if self.mainWindow.flags() & gtk.VISIBLE:
            self.mainWindow.hide()
        else:
            self.mainWindow.deiconify()
            self.mainWindow.show()
    def getNotifyObject( self ):
        if not disabled and not type_ == 'gtk':
            return self.tray
        return None
