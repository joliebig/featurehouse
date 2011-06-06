import os
import gtk
from SimpleGladeApp import SimpleGladeApp
glade_dir = ""
import gtkmozembed
class Window1(SimpleGladeApp):
        def __init__(self, path="project2.glade", root="window1", domain=None, **kwargs):
                path = os.path.join(glade_dir, path)
                SimpleGladeApp.__init__(self, path, root, domain, **kwargs)
                self.browser.load_url('http://www.google.co.nz')
        def new(self):
                pass
        def on_browser_location(self, widget):
                self.entry1.set_text( self.browser.get_location() )
        def on_back(self, widget, *args):
                self.browser.go_back()
        def on_reload(self, widget, *args):
                self.browser.reload(gtkmozembed.FLAG_RELOADNORMAL)
        def on_forward(self, widget, *args):
                self.browser.go_forward()
        def on_load(self, widget, *args):
                self.browser.load_url( self.entry1.get_text() )
        def make_browser(self, str1, str2, int1, int2):
                widget = gtkmozembed.MozEmbed()
                widget.connect("location", self.on_browser_location)
                widget.load_url(str1)
                widget.show_all()
                return widget
def main():
        window1 = Window1()
        window1.run()
if __name__ == "__main__":
        main()
