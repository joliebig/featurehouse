import gtk
import gtkmozembed
class AppWin(gtk.Window):
    def __init__(self, title, iconFile):
        gtk.Window.__init__(self)
        self.connect('destroy', lambda win: gtk.main_quit())
        self.set_title(title)
        self.set_size_request(500, 300)
        self.vbox = gtk.VBox()
        self.add(self.vbox)
        self.statusbar = gtk.Statusbar()
        self.statusbar.set_has_resize_grip(True)
        self.vbox.pack_end(self.statusbar, expand=False)
IdeviceData  = "Will this work?<br/>\n"
IdeviceData += "<textarea cols=\"40\" rows=\"3\"></textarea> <br/>\n"
IdeviceData += "<a href=\"yes\">yes</a> \n"
IdeviceData += "<a href=\"no\">no</a> \n"
IdeviceData += "<br/>\n"
class ExeApp:
    """Main class for the eXe application
    Owns the main window and starts the various action dialogs
    """
    def __init__(self):
        self.data = IdeviceData
        self.window = AppWin("BurningRats", "img/rats.png")
        self.window.connect("delete-event", self.quit)
        hbox = gtk.HBox(spacing=12)
        self.window.vbox.pack_start(hbox)
        leftBox = gtk.VBox()                           
        leftBox.set_border_width(6)
        hbox.pack_start(leftBox, expand=False)
        leftBox.pack_start(gtk.Label("Tree goes here"))
        ideviceBtn = gtk.Button("Add Idevice")
        ideviceBtn.connect("clicked", self.addIdevice)
        leftBox.pack_start(ideviceBtn, expand=False)
        gtkmozembed.gtk_moz_embed_set_comp_path("c:\\djm\\mozilla\\dist\\bin")
        self.browser = gtkmozembed.MozEmbed()
        self.browser.connect("realize", self.render)
        self.browser.connect("open_uri", self.linkClicked)
        hbox.pack_start(self.browser, expand=True)
        self.window.show_all()
    def render(self, widget):
        data  = "<html><head><title>BurningRats</title></head>"
        data += "<body><h1>BurningRats</h1>"
        data += self.data
        data += "</body></html>"""
        self.browser.render_data(data, long(len(data)), "file://", "text/html")
    def addIdevice(self, widget):
        self.data += IdeviceData
        self.render(None)
        return True
    def linkHover(self, widget):
        print "linkHover", widget
        return True
    def linkClicked(self, widget, data):
        print "linkClicked", widget, data
        print self.browser.get_link_message()
        return True
    def quit(self, *ignore):
        gtk.main_quit()
        return False
if __name__ == "__main__":
    app = ExeApp()
    gtk.main()
