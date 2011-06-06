import gtk
import cairo
from sys import platform
if platform == 'darwin':
    MAC = True
else:
    MAC = False
try:
    import Theme
    import Config
except:
    Theme = None
WIDTH=64
HEIGHT=64
class ImageAreaSelectorDialog(gtk.Dialog):
    def __init__(self, pixbuf, title = _("Select area of image"), parent = None):
        gtk.Dialog.__init__(self, title, parent,
                            gtk.DIALOG_DESTROY_WITH_PARENT,
                            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                             gtk.STOCK_OK, gtk.RESPONSE_OK ))
        self.set_default_response(gtk.RESPONSE_CANCEL)
        self.set_resizable(False)
        self.selector = ImageAreaSelector()
        self.selector.set_from_pixbuf(pixbuf)
        bbox = gtk.HButtonBox()
        if not MAC:
            self.button_rcw = gtk.Button(label=_("Rotate"))
            self.button_rcw.connect("clicked", self._on_rcw_clicked)
            self.button_rccw = gtk.Button(label=_("Rotate"))
            self.button_rccw.connect("clicked", self._on_rccw_clicked)
            if Theme:
                theme = Theme.Theme(Config.Config())  # FIXME ARRGH
                image_90 = gtk.Image()
                image_90.set_from_pixbuf(theme.getImage("rotate-90"))
                self.button_rcw.set_image(image_90)
                image_270 = gtk.Image()
                image_270.set_from_pixbuf(theme.getImage("rotate-270"))
                self.button_rccw.set_image(image_270)
            if gtk.gtk_version >= (2, 10, 0):
                bbox.pack_start(self.button_rccw)
                bbox.pack_end(self.button_rcw)
        self.vbox.pack_start(self.selector)
        self.vbox.pack_end(bbox)
        self.vbox.show_all()
    def run(self):
        response = gtk.Dialog.run(self)
        if response == gtk.RESPONSE_OK:
            pixbuf = self.selector.get_selected()
            self.destroy()
            return (response, pixbuf)
        else:
            self.destroy()
            return (gtk.RESPONSE_CANCEL, None)
    def _on_rcw_clicked(self, *args):
        self.selector.rotate(1)
    def _on_rccw_clicked(self, *args):
        self.selector.rotate(-1)
class ImageAreaSelector(gtk.DrawingArea):
    __gtype_name__ = "ImageAreaSelector"
    def __init__(self):
        gtk.DrawingArea.__init__(self)
        self.connect("expose_event", self.expose)
        self.image = None
        self.pixbuf = None
        self.connect("button_press_event", self.button_press)
        self.connect("button_release_event", self.button_release)
        self.connect("motion_notify_event", self.motion_notify)
        self.connect("configure_event", self.configure_event)
        self.add_events(gtk.gdk.BUTTON_PRESS_MASK |
                        gtk.gdk.BUTTON_RELEASE_MASK |
                        gtk.gdk.POINTER_MOTION_MASK)
        self._pressed = False
        self._moved = False
        self._state = gtk.gdk.LEFT_PTR
        self._has_overlay = False
        self._angle = 0
    def get_selected(self):
        if gtk.gtk_version >= (2, 10, 0):
            angle = self.get_angle()
            self._trans_pixbuf = self._trans_pixbuf.rotate_simple(angle)
        return self._trans_pixbuf.subpixbuf(*self.selection)
    def _get_selection(self):
        return (self._x, self._y, self._width, self._height)
    def _set_selection(self, (x, y, width, height)):
        self._x = x
        self._y = y
        self._width = width
        self._height = height
        self.redraw_canvas()
    selection = property(_get_selection, _set_selection)
    def _get_sel_coord(self):
        x1 = self._x
        y1 = self._y
        x2 = x1 + self._width
        y2 = y1 + self._height
        return (x1, y1, x2, y2)
    def _set_sel_coord(self, x1, y1, x2, y2):
        if x1 < 0 or y1 < 0 or x2 > self.pixbuf.get_width() \
               or y2 > self.pixbuf.get_height():
            return
        if x1 > x2:
            x2 = x1
        if y1 > y2:
            y2 = y1
        self.selection = map(int, (x1, y1, x2-x1, y2-y1))
    def reset_selection(self):
        self.selection = (0,0,0,0)
    def button_release(self, *args):
        self._pressed = False
        if not self._moved and self._state == gtk.gdk.LEFT_PTR:
            self.reset_selection()
    def expose(self, widget, event):
        if not self._has_overlay and not MAC:
            self.create_overlay()
        x , y, width, height = event.area
        widget.window.draw_drawable(widget.get_style().fg_gc[gtk.STATE_NORMAL],
                                    self.pixmap_shaded, 0, 0,
                                    0, 0,
                                    self.pixbuf.get_width(),
                                    self.pixbuf.get_height())
        widget.window.draw_pixbuf(None, pixbuf=self.pixbuf,
                                  src_x=self._x,
                                  src_y=self._y,
                                  dest_x= self._x,
                                  dest_y=self._y ,
                                  width=self._width, height=self._height,
                                  dither=gtk.gdk.RGB_DITHER_NORMAL,
                                  x_dither=0, y_dither=0)
        return False
    def rotate(self, angle):
        self._angle = (self._angle + angle) % 4
        angle = self.get_angle()
        self._init_pixbuf(angle, False)
    def get_angle(self):
        if not gtk.gtk_version >= (2, 10, 0):
            return 0
        if self._angle == 1:
            angle = gtk.gdk.PIXBUF_ROTATE_CLOCKWISE
        elif self._angle == 2:
            angle = 180
        elif self._angle == 3:
            angle = gtk.gdk.PIXBUF_ROTATE_COUNTERCLOCKWISE
        else:
            angle = 0
        return angle
    def update_selection(self, event):
        dx = event.x - self._mouse_x
        dy = event.y - self._mouse_y
        x1, y1, x2, y2 = self._get_sel_coord()
        if self._state == gtk.gdk.TOP_LEFT_CORNER:
            w = x2 - event.x
            h = y2 - event.y
            delta = max((w, h))
            x1 = x2 - delta
            y1 = y2 - delta
        elif self._state == gtk.gdk.TOP_RIGHT_CORNER:
            w = event.x - x1
            h = y2 - event.y
            delta = max((w, h))
            x2 = x1 + delta
            y1 = y2 - delta
        elif self._state == gtk.gdk.BOTTOM_RIGHT_CORNER:
            w = (event.x-x1)
            h = (event.y-y1)
            delta = max((w, h))
            x2 = x1 + delta
            y2 = y1 + delta
        elif self._state == gtk.gdk.BOTTOM_LEFT_CORNER:
            w = x2 - event.x
            h = (event.y-y1)
            delta = max((w, h))
            x1 = x2 - delta
            y2 = y1 + delta
        elif self._state == gtk.gdk.FLEUR:
            x1 += dx
            y1 += dy
            x2 += dx
            y2 += dy
        else:
            x1 = self._press_x
            y1 = self._press_y
            w = event.x - x1
            h = event.y - y1
            delta = max((w, h))
            x2 = x1 + delta
            y2 = y1 + delta
        self._mouse_x = event.x
        self._mouse_y = event.y
        self._set_sel_coord(x1, y1, x2, y2)
    def update_cursor(self, event):
        fuzz = max((5, int(self._width * 0.05)))
        if abs(event.y - self._y) < fuzz:
            if abs(event.x -self._x) < fuzz:
                self._state = gtk.gdk.TOP_LEFT_CORNER
            elif abs(event.x - (self._x + self._width)) < fuzz:
                self._state = gtk.gdk.TOP_RIGHT_CORNER
        elif abs(event.y - (self._y + self._height)) < fuzz:
            if abs(event.x - self._x) < fuzz:
                self._state = gtk.gdk.BOTTOM_LEFT_CORNER
            elif abs(event.x -(self._x + self._width)) < fuzz:
                self._state = gtk.gdk.BOTTOM_RIGHT_CORNER
        elif event.x > self._x and event.x < self._x + self._width \
                 and event.y > self._y and event.y < self._y + self._height:
            self._state = gtk.gdk.FLEUR
        else:
            self._state = gtk.gdk.LEFT_PTR
        self.window.set_cursor(gtk.gdk.Cursor(self._state))
    def motion_notify(self, widget, event):
        if self._pressed:
            self._moved = True
            self.update_selection(event)
        else:
            self.update_cursor(event)
    def button_press(self, widget, event):
        self._pressed = True
        self._moved = False
        self._mouse_x = event.x
        self._mouse_y = event.y
        self._press_x = event.x
        self._press_y = event.y
        if event.button == 3:
            self.reset_selection()
    def configure_event(self, widget, event):
        x, y, width, height = widget.get_allocation()
        self.pixmap = gtk.gdk.Pixmap(widget.window, width, height)
        self.pixmap.draw_rectangle(widget.get_style().white_gc,
                                   True, 0, 0, width, height)
        self.pixmap_shaded = gtk.gdk.Pixmap(widget.window, width, height)
        self.pixmap_shaded.draw_rectangle(widget.get_style().white_gc,
                                          True, 0, 0, width, height)
        self._has_overlay = False
        return True
    def create_overlay(self):
        context = self.pixmap_shaded.cairo_create()
        width = self.pixbuf.get_width()
        height = self.pixbuf.get_height()
        target  = context.get_target()
        overlay = target.create_similar(cairo.CONTENT_COLOR_ALPHA, width, height)
        punch   = target.create_similar(cairo.CONTENT_ALPHA, width, height)
        context.set_source_pixbuf(self.pixbuf, 0, 0)
        context.fill()
        context.paint()
        overlay_cr = cairo.Context (overlay)
        overlay_cr.set_source_rgba (0.4, 0.4, 0.4, 0.6)
        overlay_cr.rectangle(0, 0, width, height)
        overlay_cr.fill()
        context.set_source_surface (overlay, 0, 0)
        context.paint()
        self._has_overlay = True
    def redraw_canvas(self):
        if self.window:
            alloc = self.get_allocation()
            self.queue_draw_area(alloc.x, alloc.y, alloc.width, alloc.height)
    def do_size_request(self, requisition):
        if not self.pixbuf:
            return
        requisition.width = self.pixbuf.get_width()
        requisition.height = self.pixbuf.get_height()
    def set_from_pixbuf(self, pixbuf):
        h = pixbuf.get_height()
        w = pixbuf.get_width()
        edge = max(w, h)
        self._trans_pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, True, 8,
                                            edge, edge)
        self._disp_pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, True, 8,
                                           edge, edge)
        self._trans_pixbuf.fill(0xffffff00)
        self._disp_pixbuf.fill(0x66666666)
        dx = (edge-w)/2
        dy = (edge-h)/2
        pixbuf.composite(self._trans_pixbuf, dx, dy, w, h, dx, dy, 1, 1,
                         gtk.gdk.INTERP_BILINEAR, 255)
        pixbuf.composite(self._disp_pixbuf, dx, dy, w, h, dx, dy, 1, 1,
                         gtk.gdk.INTERP_BILINEAR, 255)
        maxw = int(0.75 * gtk.gdk.screen_width())
        maxh = int(0.75 * gtk.gdk.screen_height())
        if w > maxw or h > maxh:
            wscale = float(maxw) / edge
            hscale = float(maxh) / edge
            scale = min(wscale, hscale)
            edge = int(edge * scale)
            self._trans_pixbuf = self._trans_pixbuf.scale_simple(edge, edge,
                                                    gtk.gdk.INTERP_BILINEAR )
            self._disp_pixbuf = self._disp_pixbuf.scale_simple(edge, edge,
                                                    gtk.gdk.INTERP_BILINEAR )
        self._init_pixbuf()
    def _init_pixbuf(self, angle=None, create_selection=True):
        self.pixbuf = self._disp_pixbuf.copy()
        if angle and gtk.gtk_version >= (2, 10, 0):
            self.pixbuf = self.pixbuf.rotate_simple(angle)
            self.configure_event(self, None)
        self._has_overlay = False
        w = self.pixbuf.get_width()
        h = self.pixbuf.get_height()
        if create_selection:
            sw = min((w, h))
            x1 = int((w-sw)/2)
            y1 = int((h-sw)/2)
            self.selection = (x1, y1, sw, sw)
        else:
            self.selection = (0, 0, 0, 0)
