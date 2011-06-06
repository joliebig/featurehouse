import os
import time
import gtk
import gobject
import time
import emesenelib.common
import desktop
try:
    import libmimic
except:
    libmimic = None
    print "Libmimic not found, webcam not available"
class Webcam:
    ''' this class represents a webcam session'''
    def __init__(self, p2p, conversation, session, sender):
        '''Constructor'''
        self.p2p = p2p
        self.conversation = conversation
        self.session = int(session)
        self.sender = sender
        self.timeAccepted = None
        self.signals = []
        sap = self.signals.append
        sap(self.p2p.connect('webcam-frame-received', self.on_webcam_frame))
        sap(self.p2p.connect('webcam-failed', self.on_webcam_failed))
        self.decoder = libmimic.new_decoder()
        self.init = False
        self.gc = None
        self.frames = 0
        self.win = gtk.Window()
        self.win.set_double_buffered(False)
        self.win.set_app_paintable(True)
        self.win.set_resizable(False)
        sap(self.win.connect('delete-event', self.on_close))
        self.pixmap = None
    def on_close(self, *args):
        self.conversation.appendOutputText(None, _('You have canceled the webcam session'), 'information')
        self.p2p.emit('webcam-canceled', self.session)
    def on_webcam_failed(self, p2p, session, reason):
        if session == self.session:
            self.conversation.appendOutputText(None, _('Webcam canceled'), 'error')
    def on_webcam_frame(self, p2p, session, sender, frame):
        try:
            width, height, data = libmimic.decode(self.decoder, frame)
        except:
            print "Decode error, stopping webcam"
            self.on_close()
            return
        if not self.init:
            print "Got first frame: %dx%d" % (width, height)
            self.win.set_size_request(width, height)
            self.win.show_all()
            self.gc = gtk.gdk.GC(self.win.window)
            self.pixmap = gtk.gdk.Pixmap(self.win.window, width, height, -1)
            self.init = True
        if self.win.window:
            self.win.window.draw_rgb_image(self.gc, 0, 0, width, height, \
                gtk.gdk.RGB_DITHER_NONE, data, width*3)
            self.win.queue_draw()
    def accept(self):
        self.p2p.emit('webcam-accepted', self.session, self.sender)
