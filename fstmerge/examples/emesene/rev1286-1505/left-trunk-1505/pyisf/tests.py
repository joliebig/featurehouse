import pyisf
import struct
import drawing
import Theme
import InkDrawDemo
import pygtk
import gtk
def Read( INPUT ):
    data = open(INPUT, 'rb').read()
    image = pyisf.IsfDecoder(open(INPUT, 'rb').read(),debug=False)
    image.image.print_info()
    return image
def Display( Isf ):
    win = gtk.Window(gtk.WINDOW_TOPLEVEL)
    im = gtk.Image()
    pixmap = Isf.image.get_pixmap()
    im.set_from_pixbuf(pixmap)
    win.add(im)
    win.connect('destroy', gtk.main_quit)
    win.show_all()
    gtk.main()
def Window( Isf ):
    theme = Theme.Theme(None)
    win = InkDrawDemo.InkDrawDemo(theme)
    win.connect('destroy', gtk.main_quit)
    gtk.main()
if __name__ == '__main__':
    image = Read('agilix.isf')
    Window(image.image)
