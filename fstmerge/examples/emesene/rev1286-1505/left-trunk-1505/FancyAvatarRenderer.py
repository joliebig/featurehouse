import gtk
import gtk.gdk
import cairo
import gobject
class FancyAvatarRenderer(gtk.GenericCellRenderer):
    """Renderer for avatar """
    __gproperties__ = {
        'pixbuf': (gtk.gdk.Pixbuf, 'Pixbuf', '', gobject.PARAM_READWRITE),        
        'blocked': (bool, 'Contact Blocked', '', False, gobject.PARAM_READWRITE),        
        'dimention': (gobject.TYPE_INT, 'cell dimentions', 
                    'height width of cell', 0, 96, 32, gobject.PARAM_READWRITE),
        'status': (str, 'Contact status', '', 'FLN', gobject.PARAM_READWRITE),
        'radius_factor': (gobject.TYPE_FLOAT,'radius of pixbuf', 
                          '0.0 to 0.5 with 0.1 = 10% of dimention',
                          0.0, 0.5,0.11, gobject.PARAM_READWRITE),
         }
    def __init__(self, controller, cellDimention = 32, cellRadius = 0.11):
        self.__gobject_init__()
        self._pixbuf = None
        self._status = 'FLN'
        self._blocked = False
        self._dimention = cellDimention
        self._radius_factor = cellRadius
        self._icon_source = gtk.IconSource()
        self._icon_source.set_state(gtk.STATE_INSENSITIVE)
        self.set_property('xpad', 1)
        self.set_property('ypad', 1)   
        self._theme = controller.theme
        self._config = controller.config
        self._set_transformation(self._config.user['statusTransformation'])
        self.transId = self._config.connect('change::statusTransformation', \
            self._transformation_callback)
    def destroy(self):
        self._config.disconnect(self.transId)
        gtk.GenericCellRenderer.destroy(self)
    def _get_padding(self):
        return (self.get_property('xpad'), self.get_property('ypad'))
    def _set_transformation(self, setting):
        transformation = setting.split('|')
        self._pixalated = ('pixelate' in transformation)
        self._corner = ('corner' in transformation)
        self._alpha_status = ('alpha' in transformation)
        self._mini = ('mini' in transformation)
        self._gray = ('gray' in transformation)
    def _transformation_callback(self, config, newvalue, oldvalue):
        self._set_transformation(newvalue)
    def do_get_property(self, property):
        if property.name == 'pixbuf':
            return self._pixbuf
        elif property.name == 'dimention':
            return self._dimention
        elif property.name == 'radius-factor':
            return self._radius_factor
        elif property.name == 'blocked':
            return self._blocked
        elif property.name == 'status':
            return self._status                        
        else:
            raise AttributeError, 'unknown property %s' % property.name
    def do_set_property(self, property, value):
        if property.name == 'pixbuf':
            self._pixbuf = value
        elif property.name == 'dimention':            
            self._dimention = value
        elif property.name == 'radius-factor':
            self._radius_factor = value         
        elif property.name == 'blocked':
            self._blocked = value
        elif property.name == 'status':
            self._status = value      
        else:
            raise AttributeError, 'unknown property %s' % property.name
    def on_get_size(self, widget, cell_area=None):       
        """Requisition size"""
        xpad, ypad = self._get_padding()
        if self._dimention >= 32: width = self._dimention
        elif self._mini: width = self._dimention
        elif self._corner: width = self._dimention * 2            
        else: width = self._dimention            
        height = self._dimention + (ypad * 2)                       
        return (0, 0,  width, height)
    def on_render(self, window, widget, bg_area, cell_area, expose_area, flags):        
        """Prepare rendering setting for avatar"""
        xpad, ypad = self._get_padding()
        x, y, width, height = cell_area 
        ctx = window.cairo_create()          
        ctx.translate(x, y)
        avatar = self._pixbuf
        overlay = None
        alpha = 1
        dim = self._dimention
        if self._pixalated: 
            avatar = self._get_pixalate(self._pixbuf)            
        if self._corner: 
            overlay = self._get_overlay()
        if self._alpha_status and self._status in ('IDL', 'FLN'): 
            alpha = 0.75
        if self._gray and self._status == 'FLN':
            alpha = 1
            source = self._icon_source
            source.set_pixbuf(avatar)
            direction = widget.get_direction()
            avatar = widget.style.render_icon(source, direction, 
                                              gtk.STATE_INSENSITIVE, 
                                              -1, widget, "gtk-image")
        if avatar:
            self._draw_avatar(ctx, avatar, width - dim, ypad, dim, 
                                gtk.ANCHOR_CENTER, self._radius_factor, alpha)
        if overlay:       
            if self._dimention >= 32 :     
                self._draw_avatar(ctx, overlay, width - 16, 
                                  ypad + dim - 16, 16, gtk.ANCHOR_SW)
            elif self._mini:
                self._draw_avatar(ctx, overlay, width - 8, 
                                  ypad + dim - 8, 8, gtk.ANCHOR_SW)
            else:
                self._draw_avatar(ctx, overlay, 0, ypad, 16)
    def _get_overlay(self):
        """Return overlay pixbuf, 
           dependant on contacts status and block state"""
        overlay = None
        if self._blocked:
            overlay = self._theme.getImage('status-blocked')
        elif self._status in ('AWY', 'BRB', 'LUN', 'IDL'):
            overlay = self._theme.getImage('status-away')
        elif self._status in ('BSY','PHN'):
            overlay = self._theme.getImage('status-busy')
        return overlay       
    def _get_pixalate(self, pixbuf):
        """Pixalate and saturate values based on original renderer
           kept to retain compatibility"""
        pixalate = pixbuf.copy()        
        if not self._status == 'NLN':
            if contact.status == 'BSY':
                pixalate.saturate_and_pixelate(pixbuf, 1.0, True)
            else:
                pixalate.saturate_and_pixelate(pixbuf, 0.1, False)                
        return pixalate
    def _draw_avatar(self, ctx, pixbuf, x, y, dimention, 
                         position = gtk.ANCHOR_CENTER, 
                         radius = 0, alpha = 1):
        """Render avatar"""        
        ctx.save()
        ctx.set_antialias(cairo.ANTIALIAS_SUBPIXEL)
        ctx.translate(x, y)
        pix_width = pixbuf.get_width()
        pix_height = pixbuf.get_height()
        if (pix_width > dimention) or (pix_height > dimention): 
            scale_factor = float(dimention) / max (pix_width,pix_height)
        else: 
            scale_factor = 1        
        scale_width = pix_width* scale_factor
        scale_height = pix_height* scale_factor 
        if position in (gtk.ANCHOR_NW, gtk.ANCHOR_W, gtk.ANCHOR_SW):
            x = 0
        elif position in (gtk.ANCHOR_N, gtk.ANCHOR_CENTER, gtk.ANCHOR_S): 
            x = (dimention/2) - (scale_width/2)
        else: 
            x = dimention - scale_width         
        if position in (gtk.ANCHOR_NW, gtk.ANCHOR_N, gtk.ANCHOR_NE):
            y = 0
        elif position in (gtk.ANCHOR_E, gtk.ANCHOR_CENTER, gtk.ANCHOR_W): 
            y = (dimention/2) - (scale_height/2)
        else:
            y = dimention - scale_height         
        ctx.translate(x, y)
        if radius > 0 : 
            self._rounded_rectangle(ctx, 0, 0, scale_width, scale_height,
                                      self._dimention * radius) 
            ctx.clip()
        ctx.scale(scale_factor,scale_factor)
        ctx.set_source_pixbuf(pixbuf, 0, 0)
        ctx.paint_with_alpha(alpha)
        ctx.restore()
    def _rounded_rectangle(self, cr, x, y, w, h, radius=5):
        """Create rounded rectangle path"""
        ARC_TO_BEZIER = 0.55228475
        if radius > (min(w,h)/2):
            radius = (min(w,h)/2)
        c = ARC_TO_BEZIER * radius
        cr.new_path();
        cr.move_to ( x + radius, y)
        cr.rel_line_to ( w - 2 * radius, 0.0)
        cr.rel_curve_to ( c, 0.0, radius, c, radius, radius)
        cr.rel_line_to ( 0, h - 2 * radius)
        cr.rel_curve_to ( 0.0, c, c - radius, radius, -radius, radius)
        cr.rel_line_to ( -w + 2 * radius, 0)
        cr.rel_curve_to ( -c, 0, -radius, -c, -radius, -radius)
        cr.rel_line_to (0, -h + 2 * radius)
        cr.rel_curve_to (0.0, -c, radius - c, -radius, radius, -radius)
        cr.close_path ()
gobject.type_register(FancyAvatarRenderer)
