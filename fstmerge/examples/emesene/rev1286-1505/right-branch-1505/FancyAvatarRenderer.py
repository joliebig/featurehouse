import gtk
import gtk.gdk
import cairo
import gobject
class FancyAvatarRenderer ( gtk.GenericCellRenderer ):
    """Renderer for avatar """
    __gproperties__ = {
        'pixbuf': (gtk.gdk.Pixbuf, 'Pixbuf','Pixbuf',gobject.PARAM_READWRITE),        
        'blocked': (bool , 'User Blocked status', '',False, gobject.PARAM_READWRITE),        
        'dimention' : (gobject.TYPE_INT, 'cell dimentions', 'height width of cell', 0, 96, 32, gobject.PARAM_READWRITE),
        'status': (str, 'Contact status', '','FLN', gobject.PARAM_READWRITE),
        'radius_factor' : (gobject.TYPE_FLOAT,'radius of pixbuf', 
        '0.0 to 0.5 with 0.1 = 10% of dimention',0.0, 0.5,0.11, gobject.PARAM_READWRITE),
         }         
    def __init__(self, controller, cellDimention = 32, cellRadius = 0.11):
        self.__gobject_init__()
        self._pixbuf = None
        self._status = 'FLN'
        self._blocked = False
        self._dimention = cellDimention
        self._radius_factor = cellRadius
        self.set_property('xpad', 1)
        self.set_property('ypad', 1)   
        self._theme = controller.theme
        self._config = controller.config
        self.__set_transformation(self._config.user['statusTransformation'])
        self.transId = self._config.connect('change::statusTransformation', \
            self.__transformation_callback)
    def destroy(self):
        self._config.disconnect(self.transId)
        gtk.GenericCellRenderer.destroy(self)
    def __set_transformation(self, setting):
        transformation = setting.split('|')                   
        if 'pixelate' in transformation: self._pixalated = True  
        else: self._pixalated = False
        if 'corner' in transformation: self._corner = True  
        else: self._corner = False        
        if 'alpha' in transformation: self._alpha_status = True  
        else: self._alpha_status = False
        if 'mini' in transformation: self._mini = True  
        else: self._mini = False
    def __transformation_callback(self, config, newvalue, oldvalue):
        self.__set_transformation(newvalue)
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
    def on_get_size( self, widget, cell_area=None ):       
        xpad,ypad = self.get_property('xpad'), self.get_property('ypad')
        if self._dimention >= 32: width = self._dimention
        elif self._mini: width = self._dimention
        elif self._corner: width = self._dimention * 2            
        else: width = self._dimention        
        height = self._dimention + (ypad *2)                       
        return ( 0, 0,  width, height) 
    def on_render( self, window, widget, background_area, cell_area, expose_area, flags ):        
        avatar = self._pixbuf
        overlay = None
        alpha = 1
        if self._pixalated: avatar = __getPixalate(self._pixbuf)            
        if self._corner: overlay = self.__getOverlay()
        if self._alpha_status: alpha = self.__getAlpha()
        xpad,ypad = self.get_property('xpad'), self.get_property('ypad')
        cell_x, cell_y, cell_width, cell_height = cell_area 
        ctx = window.cairo_create()          
        ctx.translate(cell_x,cell_y)
        if not avatar == None:
            self.__drawScalePixbuf( ctx,avatar,cell_width - self._dimention,ypad ,self._dimention, 
                gtk.ANCHOR_CENTER, self._radius_factor, alpha)
        if not overlay == None:       
            if self._dimention >= 32 :     
                self.__drawScalePixbuf( ctx,overlay,cell_width - 16, ypad + self._dimention - 16,16, position = gtk.ANCHOR_SW)
            elif self._mini:
                self.__drawScalePixbuf( ctx,overlay,cell_width - 8, ypad + self._dimention - 8,8, position = gtk.ANCHOR_SW)
            else:
                self.__drawScalePixbuf( ctx,overlay,0,ypad  ,16)
    def __getAlpha(self):
        if self._status in ['IDL', 'FLN']: return 0.75
        else: return 1
    def __getOverlay(self):
        if self._blocked:
            return self._theme.getImage('status-blocked')
        elif self._status in ['AWY','BRB','LUN','IDL' ] :
            return self._theme.getImage('status-away')
        elif self._status in ['BSY','PHN' ] :
            return self._theme.getImage('status-busy')
        else:
            return None       
    def __getPixalate( self, pixbuf ):
        pixbuf_to_modify = pixbuf.copy()        
        if not self._status == 'NLN':
            if contact.status == 'BSY':
                pixbuf_to_modify.saturate_and_pixelate(pixbuf,1.0, True)
            else:
                pixbuf_to_modify.saturate_and_pixelate(pixbuf,0.1, False)                
        return pixbuf_to_modify
    def __drawScalePixbuf(self, ctx, pixbuf, x,y , dimention, position = gtk.ANCHOR_CENTER, radius = 0, alpha = 1 ):
        ctx.save()
        ctx.set_antialias(cairo.ANTIALIAS_SUBPIXEL)
        ctx.translate(x,y)
        pix_width = pixbuf.get_width()
        pix_height = pixbuf.get_height()
        if (pix_width > dimention) or (pix_height > dimention): 
            scale_factor = float(dimention) / max (pix_width,pix_height)
        else: 
            scale_factor = 1        
        scale_width = pix_width* scale_factor
        scale_height = pix_height* scale_factor 
        if position in [gtk.ANCHOR_NW,gtk.ANCHOR_W,gtk.ANCHOR_SW] : x = 0
        elif position in [gtk.ANCHOR_N,gtk.ANCHOR_CENTER,gtk.ANCHOR_S] : x = (dimention/2) - (scale_width/2)
        else: x = dimention - scale_width        
        if position in [gtk.ANCHOR_NW,gtk.ANCHOR_N,gtk.ANCHOR_NE] : y = 0
        elif position in [gtk.ANCHOR_E,gtk.ANCHOR_CENTER,gtk.ANCHOR_W] : y = (dimention/2) - (scale_height/2)
        else: y = dimention - scale_height         
        ctx.translate(x, y)
        if radius > 0 : 
            self.__roundedrecMoonlight(ctx,0,0,scale_width,scale_height, self._dimention * radius) 
            ctx.clip()
        ctx.scale(scale_factor,scale_factor)
        ctx.set_source_pixbuf(pixbuf,0,0)
        ctx.paint_with_alpha(alpha)
        ctx.restore()
    def __roundedrecMoonlight(self, cr,x,y,w,h,radius=5):
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
gobject.type_register( FancyAvatarRenderer )
