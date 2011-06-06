from __future__ import division
import math
import os
import sys
import warnings
def fn_name(): return sys._getframe(1).f_code.co_name
import gobject
import gtk; gdk = gtk.gdk
import pango
pygtk_version_required = (2,2,0)
if gtk.pygtk_version < pygtk_version_required:
    raise ImportError ("PyGTK %d.%d.%d is installed\n"
                      "PyGTK %d.%d.%d or later is required"
                      % (gtk.pygtk_version + pygtk_version_required))
del pygtk_version_required
import numpy as np
import matplotlib
from matplotlib._pylab_helpers import Gcf
from matplotlib.backend_bases import RendererBase, GraphicsContextBase, \
     FigureManagerBase, FigureCanvasBase
from matplotlib.cbook import is_string_like
from matplotlib.figure import Figure
from matplotlib.mathtext import MathTextParser
from matplotlib.transforms import Affine2D
from matplotlib.backends._backend_gdk import pixbuf_get_pixels_array
backend_version = "%d.%d.%d" % gtk.pygtk_version
_debug = False
IMAGE_FORMAT  = ['eps', 'jpg', 'png', 'ps', 'svg'] + ['bmp'] # , 'raw', 'rgb']
IMAGE_FORMAT.sort()
IMAGE_FORMAT_DEFAULT  = 'png'
class RendererGDK(RendererBase):
    fontweights = {
        100          : pango.WEIGHT_ULTRALIGHT,
        200          : pango.WEIGHT_LIGHT,
        300          : pango.WEIGHT_LIGHT,
        400          : pango.WEIGHT_NORMAL,
        500          : pango.WEIGHT_NORMAL,
        600          : pango.WEIGHT_BOLD,
        700          : pango.WEIGHT_BOLD,
        800          : pango.WEIGHT_HEAVY,
        900          : pango.WEIGHT_ULTRABOLD,
        'ultralight' : pango.WEIGHT_ULTRALIGHT,
        'light'      : pango.WEIGHT_LIGHT,
        'normal'     : pango.WEIGHT_NORMAL,
        'medium'     : pango.WEIGHT_NORMAL,
        'semibold'   : pango.WEIGHT_BOLD,
        'bold'       : pango.WEIGHT_BOLD,
        'heavy'      : pango.WEIGHT_HEAVY,
        'ultrabold'  : pango.WEIGHT_ULTRABOLD,
        'black'      : pango.WEIGHT_ULTRABOLD,
                   }
    layoutd = {}  # a map from text prop tups to pango layouts
    rotated = {}  # a map from text prop tups to rotated text pixbufs
    def __init__(self, gtkDA, dpi):
        self.gtkDA = gtkDA
        self.dpi   = dpi
        self._cmap = gtkDA.get_colormap()
        self.mathtext_parser = MathTextParser("Agg")
    def set_pixmap (self, pixmap):
        self.gdkDrawable = pixmap
    def set_width_height (self, width, height):
        """w,h is the figure w,h not the pixmap w,h
        """
        self.width, self.height = width, height
    def draw_path(self, gc, path, transform, rgbFace=None):
        transform = transform + Affine2D(). \
            scale(1.0, -1.0).translate(0, self.height)
        polygons = path.to_polygons(transform, self.width, self.height)
        for polygon in polygons:
            polygon = [(int(round(x)), int(round(y))) for x, y in polygon]
            if rgbFace is not None:
                saveColor = gc.gdkGC.foreground
                gc.gdkGC.foreground = gc.rgb_to_gdk_color(rgbFace)
                self.gdkDrawable.draw_polygon(gc.gdkGC, True, polygon)
                gc.gdkGC.foreground = saveColor
            if gc.gdkGC.line_width > 0:
                self.gdkDrawable.draw_lines(gc.gdkGC, polygon)
    def draw_image(self, gc, x, y, im):
        bbox = gc.get_clip_rectangle()
        if bbox != None:
            l,b,w,h = bbox.bounds
        im.flipud_out()
        rows, cols, image_str = im.as_rgba_str()
        image_array = np.fromstring(image_str, np.uint8)
        image_array.shape = rows, cols, 4
        pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB,
                                has_alpha=True, bits_per_sample=8,
                                width=cols, height=rows)
        array = pixbuf_get_pixels_array(pixbuf)
        array[:,:,:] = image_array
        gc = self.new_gc()
        y = self.height-y-rows
        try: # new in 2.2
            self.gdkDrawable.draw_pixbuf (gc.gdkGC, pixbuf, 0, 0,
                                          int(x), int(y), cols, rows,
                                          gdk.RGB_DITHER_NONE, 0, 0)
        except AttributeError:
            pixbuf.render_to_drawable(self.gdkDrawable, gc.gdkGC, 0, 0,
                                  int(x), int(y), cols, rows,
                                  gdk.RGB_DITHER_NONE, 0, 0)
        im.flipud_out()
    def draw_text(self, gc, x, y, s, prop, angle, ismath):
        x, y = int(x), int(y)
        if x <0 or y <0: # window has shrunk and text is off the edge
            return
        if angle not in (0,90):
            warnings.warn('backend_gdk: unable to draw text at angles ' +
                          'other than 0 or 90')
        elif ismath:
            self._draw_mathtext(gc, x, y, s, prop, angle)
        elif angle==90:
            self._draw_rotated_text(gc, x, y, s, prop, angle)
        else:
            layout, inkRect, logicalRect = self._get_pango_layout(s, prop)
            l, b, w, h = inkRect
            self.gdkDrawable.draw_layout(gc.gdkGC, x, y-h-b, layout)
    def _draw_mathtext(self, gc, x, y, s, prop, angle):
        ox, oy, width, height, descent, font_image, used_characters = \
            self.mathtext_parser.parse(s, self.dpi, prop)
        if angle==90:
            width, height = height, width
            x -= width
        y -= height
        imw = font_image.get_width()
        imh = font_image.get_height()
        N = imw * imh
        Xall = np.zeros((N,1), np.uint8)
        image_str = font_image.as_str()
        Xall[:,0] = np.fromstring(image_str, np.uint8)
        Xs = np.amax(Xall,axis=1)
        Xs.shape = imh, imw
        pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, has_alpha=True,
                                bits_per_sample=8, width=imw, height=imh)
        array = pixbuf_get_pixels_array(pixbuf)
        rgb = gc.get_rgb()
        array[:,:,0]=int(rgb[0]*255)
        array[:,:,1]=int(rgb[1]*255)
        array[:,:,2]=int(rgb[2]*255)
        array[:,:,3]=Xs
        try: # new in 2.2
            self.gdkDrawable.draw_pixbuf (gc.gdkGC, pixbuf, 0, 0,
                                          int(x), int(y), imw, imh,
                                          gdk.RGB_DITHER_NONE, 0, 0)
        except AttributeError:
            pixbuf.render_to_drawable(self.gdkDrawable, gc.gdkGC, 0, 0,
                                  int(x), int(y), imw, imh,
                                  gdk.RGB_DITHER_NONE, 0, 0)
    def _draw_rotated_text(self, gc, x, y, s, prop, angle):
        """
        Draw the text rotated 90 degrees, other angles are not supported
        """
        gdrawable = self.gdkDrawable
        ggc = gc.gdkGC
        layout, inkRect, logicalRect = self._get_pango_layout(s, prop)
        l, b, w, h = inkRect
        x = int(x-h)
        y = int(y-w)
        if x < 0 or y < 0: # window has shrunk and text is off the edge
            return
        key = (x,y,s,angle,hash(prop))
        imageVert = self.rotated.get(key)
        if imageVert != None:
            gdrawable.draw_image(ggc, imageVert, 0, 0, x, y, h, w)
            return
        imageBack = gdrawable.get_image(x, y, w, h)
        imageVert = gdrawable.get_image(x, y, h, w)
        imageFlip = gtk.gdk.Image(type=gdk.IMAGE_FASTEST,
                                  visual=gdrawable.get_visual(),
                                  width=w, height=h)
        if imageFlip == None or imageBack == None or imageVert == None:
            warnings.warn("Could not renderer vertical text")
            return
        imageFlip.set_colormap(self._cmap)
        for i in range(w):
            for j in range(h):
                imageFlip.put_pixel(i, j, imageVert.get_pixel(j,w-i-1) )
        gdrawable.draw_image(ggc, imageFlip, 0, 0, x, y, w, h)
        gdrawable.draw_layout(ggc, x, y-b, layout)
        imageIn  = gdrawable.get_image(x, y, w, h)
        for i in range(w):
            for j in range(h):
                imageVert.put_pixel(j, i, imageIn.get_pixel(w-i-1,j) )
        gdrawable.draw_image(ggc, imageBack, 0, 0, x, y, w, h)
        gdrawable.draw_image(ggc, imageVert, 0, 0, x, y, h, w)
        self.rotated[key] = imageVert
    def _get_pango_layout(self, s, prop):
        """
        Create a pango layout instance for Text 's' with properties 'prop'.
        Return - pango layout (from cache if already exists)
        Note that pango assumes a logical DPI of 96
        Ref: pango/fonts.c/pango_font_description_set_size() manual page
        """
        key = self.dpi, s, hash(prop)
        value = self.layoutd.get(key)
        if value != None:
            return value
        size = prop.get_size_in_points() * self.dpi / 96.0
        size = round(size)
        font_str = '%s, %s %i' % (prop.get_name(), prop.get_style(), size,)
        font = pango.FontDescription(font_str)
        font.set_weight(self.fontweights[prop.get_weight()])
        layout = self.gtkDA.create_pango_layout(s)
        layout.set_font_description(font)
        inkRect, logicalRect = layout.get_pixel_extents()
        self.layoutd[key] = layout, inkRect, logicalRect
        return layout, inkRect, logicalRect
    def flipy(self):
        return True
    def get_canvas_width_height(self):
        return self.width, self.height
    def get_text_width_height_descent(self, s, prop, ismath):
        if ismath:
            ox, oy, width, height, descent, font_image, used_characters = \
                self.mathtext_parser.parse(s, self.dpi, prop)
            return width, height, descent
        layout, inkRect, logicalRect = self._get_pango_layout(s, prop)
        l, b, w, h = inkRect
        return w, h+1, h + 1
    def new_gc(self):
        return GraphicsContextGDK(renderer=self)
    def points_to_pixels(self, points):
        return points/72.0 * self.dpi
class GraphicsContextGDK(GraphicsContextBase):
    _cached = {}  # map: rgb color -> gdk.Color
    _joind = {
        'bevel' : gdk.JOIN_BEVEL,
        'miter' : gdk.JOIN_MITER,
        'round' : gdk.JOIN_ROUND,
        }
    _capd = {
        'butt'       : gdk.CAP_BUTT,
        'projecting' : gdk.CAP_PROJECTING,
        'round'      : gdk.CAP_ROUND,
        }
    def __init__(self, renderer):
        GraphicsContextBase.__init__(self)
        self.renderer = renderer
        self.gdkGC    = gtk.gdk.GC(renderer.gdkDrawable)
        self._cmap    = renderer._cmap
    def rgb_to_gdk_color(self, rgb):
        """
        rgb - an RGB tuple (three 0.0-1.0 values)
        return an allocated gtk.gdk.Color
        """
        try:
            return self._cached[tuple(rgb)]
        except KeyError:
            self._cached[tuple(rgb)] = \
                    self._cmap.alloc_color(
                        int(rgb[0]*65535),int(rgb[1]*65535),int(rgb[2]*65535))
            color = self._cached[tuple(rgb)]
            return color
    def set_capstyle(self, cs):
        GraphicsContextBase.set_capstyle(self, cs)
        self.gdkGC.cap_style = self._capd[self._capstyle]
    def set_clip_rectangle(self, rectangle):
        GraphicsContextBase.set_clip_rectangle(self, rectangle)
        if rectangle is None:
            return
        l,b,w,h = rectangle.bounds
        rectangle = (int(l), self.renderer.height-int(b+h)+1,
                     int(w), int(h))
        self.gdkGC.set_clip_rectangle(rectangle)
    def set_dashes(self, dash_offset, dash_list):
        GraphicsContextBase.set_dashes(self, dash_offset, dash_list)
        if dash_list == None:
            self.gdkGC.line_style = gdk.LINE_SOLID
        else:
            pixels = self.renderer.points_to_pixels(np.asarray(dash_list))
            dl = [max(1, int(round(val))) for val in pixels]
            self.gdkGC.set_dashes(dash_offset, dl)
            self.gdkGC.line_style = gdk.LINE_ON_OFF_DASH
    def set_foreground(self, fg, isRGB=False):
        GraphicsContextBase.set_foreground(self, fg, isRGB)
        self.gdkGC.foreground = self.rgb_to_gdk_color(self.get_rgb())
    def set_graylevel(self, frac):
        GraphicsContextBase.set_graylevel(self, frac)
        self.gdkGC.foreground = self.rgb_to_gdk_color(self.get_rgb())
    def set_joinstyle(self, js):
        GraphicsContextBase.set_joinstyle(self, js)
        self.gdkGC.join_style = self._joind[self._joinstyle]
    def set_linewidth(self, w):
        GraphicsContextBase.set_linewidth(self, w)
        if w == 0:
            self.gdkGC.line_width = 0
        else:
            pixels = self.renderer.points_to_pixels(w)
            self.gdkGC.line_width = max(1, int(round(pixels)))
def new_figure_manager(num, *args, **kwargs):
    """
    Create a new figure manager instance
    """
    FigureClass = kwargs.pop('FigureClass', Figure)
    thisFig = FigureClass(*args, **kwargs)
    canvas  = FigureCanvasGDK(thisFig)
    manager = FigureManagerBase(canvas, num)
    return manager
class FigureCanvasGDK (FigureCanvasBase):
    def __init__(self, figure):
        FigureCanvasBase.__init__(self, figure)
        self._renderer_init()
    def _renderer_init(self):
        self._renderer = RendererGDK (gtk.DrawingArea(), self.figure.dpi)
    def _render_figure(self, pixmap, width, height):
        self._renderer.set_pixmap (pixmap)
        self._renderer.set_width_height (width, height)
        self.figure.draw (self._renderer)
    filetypes = FigureCanvasBase.filetypes.copy()
    filetypes['jpg'] = 'JPEG'
    filetypes['jpeg'] = 'JPEG'
    def print_jpeg(self, filename, *args, **kwargs):
        return self._print_image(filename, 'jpeg')
    print_jpg = print_jpeg
    def print_png(self, filename, *args, **kwargs):
        return self._print_image(filename, 'png')
    def _print_image(self, filename, format, *args, **kwargs):
        width, height = self.get_width_height()
        pixmap = gtk.gdk.Pixmap (None, width, height, depth=24)
        self._render_figure(pixmap, width, height)
        pixbuf = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, 0, 8,
                                width, height)
        pixbuf.get_from_drawable(pixmap, pixmap.get_colormap(),
                                 0, 0, 0, 0, width, height)
        pixbuf.save(filename, format)
    def get_default_filetype(self):
        return 'png'
