"""
Embeds mozilla gecko in a pygtk window
"""
import pygtk
pygtk.require('2.0')
import gtk
window = gtk.Window()
window.connect("delete-event", gtk.main_quit)
window.set_border_width(10)
window.set_default_size(320,240)
window.show_all()
from xpcom import components
from xpcomponents import *
import sys
iface = components.interfaces
create = lambda name, intfc=None: components.classes[name].createInstance(intfc)
wbc = components.classes['@mozilla.org/embedding/browser/nsWebBrowser;1']
wbi = iface['nsIWebBrowser']
sm = components.serviceManager
def initEmbedding():
    """Tell mozilla we'll be embedding it"""
    from ctypes import cdll, c_int
    embedlib = cdll.LoadLibrary('/home/matthew/work/downloads/mozilla/dist/firefox/libgtkembedmoz.so')
    fn = '/home/matthew/work/downloads/mozilla/dist/firefox'
    f = create('@mozilla.org/file/local;1', iface.nsILocalFile)
    f.initWithPath(fn)
    address = c_int(id(f._comobj_))
    address = c_int(id(f._interfaces_.items()[-1]))
    address = c_int(id(f))
    print address
    print embedlib.NS_InitEmbedding(None, None)
from ctypes import cdll, c_int
embedlib = cdll.LoadLibrary('/home/matthew/work/downloads/mozilla/dist/firefox/libgtkembedmoz.so')
def rubbish():
    print '**********************'
    cls = create('@mozilla.org/appshell/commandLineService;1')
    app = components.classes['@mozilla.org/appshell/appShellService;1'].getService(iface['nsIAppShellService'])
    print app
    native = app.hiddenWindow
    print native
    print native.start()
