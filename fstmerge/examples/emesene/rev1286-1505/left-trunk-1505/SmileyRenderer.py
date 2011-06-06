import sys
import gtk
import pango
import cairo
import gtk.gdk
import gobject
from gobject import PARAM_READWRITE, TYPE_PYOBJECT
from Parser import Smiley
from emesenelib.ContactData import Contact, Group
class SmileyRenderer(gtk.GenericCellRenderer):
    '''Cellrenderer with smiley support. '''
    __gproperties__ = {
        'markup': (TYPE_PYOBJECT, 'Smiley markup',
        'A list with strs, Smileys, and \ns', PARAM_READWRITE),
        'obj': (TYPE_PYOBJECT, 'Smiley markup',
        'A list with strs, Smileys, and \ns', PARAM_READWRITE),
        'ellipsize': (bool, 'Ellipsize True/False', 
        '',False, gobject.PARAM_READWRITE),
        'wrap': (bool, 'Wrap True/False', '',False, gobject.PARAM_READWRITE),
    }
    def __init__(self, cache = {}):
        self.__gobject_init__()
        self._markup = ['']
        self._ellipsize = True
        self._wrap = True
        self._cached_markup = {}
        self._cached_layout = {}
        self._style_handler_id = None
        self.xpad = 2
        self.ypad = 1
        self._selected_flgs = (int(gtk.CELL_RENDERER_SELECTED), \
                int(gtk.CELL_RENDERER_SELECTED) + int(gtk.CELL_RENDERER_PRELIT))
    def do_get_property(self, prop):
        return getattr(self, prop.name)
    def do_set_property(self, prop, value):
        setattr(self, prop.name, value)
    def on_render(self, win, widget, bgnd_area, cell_area, expose_area, flags):
        '''Called by gtk to render the cell.'''
        x, y, width, height = cell_area
        x += self.xpad
        y += self.ypad
        width -= self.xpad
        ctx = win.cairo_create()        
        layout = self._get_layout(widget)
        layout.set_width(width  * pango.SCALE)
        layout.set_in_color_override_mode(flags in self._selected_flgs)            
        layout.draw(ctx, (x, y, width, height))
    def on_get_size(self, widget, cell_area):
        '''Returns the size of the cellrenderer'''
        if not self._style_handler_id:
            self._style_handler_id = widget.connect('style-set', self._style_set)
        layout = self._get_layout(widget)
        width, height = layout.get_pixel_size()
        return (0,0, -1, height + (self.ypad * 2))
    def _get_layout(self, widget):
        if type(self.obj) == Contact: 
            obj_id = self.obj.email
        elif type(self.obj) == Group: 
            obj_id = self.obj.id
        else: 
            obj_id = ''    
        if obj_id in self._cached_markup and \
                    self._cached_markup[obj_id] == self.markup:
            layout = self._cached_layout[obj_id]
        else:
            layout = SmileyLayout(widget.create_pango_context(), 
                                  self.markup,
                                  widget.style.text[gtk.STATE_NORMAL],
                                  widget.style.text[gtk.STATE_SELECTED]) 
            layout.set_ellipsize(pango.ELLIPSIZE_END)
            self._cached_markup[obj_id] = self.markup
            self._cached_layout[obj_id] = layout
        return layout
    def _style_set(self, widget, previous_style):
        self._cached_markup = {}
        self._cached_layout = {}        
        widget.queue_resize()
class SmileyLabel(gtk.Widget):
    '''Label with smiley support. '''
    __gsignals__ = { 'size_request' : 'override',
                     'size-allocate' : 'override', 
                     'expose-event' : 'override'}
    def __init__(self, smiley_theme, style):
        gtk.Widget.__init__(self)
        self._text = ['']     
        self._ellipsize = True
        self._wrap = True
        self._smiley_layout = None            
        self.set_flags(self.flags() | gtk.NO_WINDOW)
        self._smiley_layout = SmileyLayout(self.create_pango_context())
    def set_ellipsize(self, ellipsize):
        ''' Sets the ellipsize behavior '''
        self._ellipsize = ellipsize   
        self.queue_resize()
    def set_wrap(self, wrap):
        ''' Sets the wrap behavior '''
        self._wrap = wrap
        self.queue_resize()
    def set_markup(self, text=['']):
        self.set_text(text)
    def set_text(self, text=['']):
        ''' Sets widget text '''
        self._text = text
        self.setup_smiley_layout()
        self.queue_resize()
    def set_smiley_scaling(self, smiley_scaling):
        self._smiley_layout.set_smiley_scaling(smiley_scaling)
        self.queue_resize()
    def setup_smiley_layout(self):
        self._smiley_layout.set_element_list(self._text)
    def do_realize(self):
        gtk.Widget.do_realize(self)
        self.set_flags(self.flags() | gtk.REALIZED)
        self.window = self.get_parent().window        
    def do_style_set(self, prev_style):
        self._smiley_layout.set_colors(self.style.text[gtk.STATE_NORMAL])
        self.queue_draw()
    def do_size_request(self, requisition):             
        self._smiley_layout.set_width(-1)
        width, height = self._smiley_layout.get_pixel_size()
        requisition.height = height
        if self._ellipsize or self._wrap:
            requisition.width = 0
        else: 
            requisition.width = width
    def do_size_allocate(self, allocation):
        if not (self._ellipsize or self._wrap):
            self._smiley_layout.set_width(-1)
            width, height = self._smiley_layout.get_pixel_size()
            self.set_size_request(width, height)
        else:
            if self._ellipsize: 
                self._smiley_layout.set_ellipsize(pango.ELLIPSIZE_END)
            else: 
                self._smiley_layout.set_ellipsize(pango.ELLIPSIZE_NONE)
            self._smiley_layout.set_width(allocation.width * pango.SCALE)
            self.set_size_request(-1, self._smiley_layout.get_pixel_size()[1])
        gtk.Widget.do_size_allocate(self, allocation)
    def do_expose_event(self, event):
        area = self.get_allocation()
        ctx = event.window.cairo_create()    
        self._smiley_layout.draw(ctx, area)
gobject.type_register(SmileyLabel)
class SmileyLayout(pango.Layout):
    def __init__(self, context, 
                 parsed_elements_list = [''], 
                 color = gtk.gdk.Color(), 
                 override_color = gtk.gdk.Color(),
                 scaling=1.0):                    
        pango.Layout.__init__(self, context)
        self._width = -1
        self._ellipsize = True
        self._elayout = pango.Layout(context)
        self._elayout.set_text('___') #¿
        self._elayout.set_ellipsize(pango.ELLIPSIZE_END)
        self._elayout.set_width(0)
        self._in_override = False # color override mode, used for selected text
        self._base_to_center = 0
        self._text_height = 0
        self._smilies = {} # key: (index_pos), value(pixbuf)
        self._base_attrlist = None # no color             
        self._attrlist = None # with color
        self._override_attrlist = None # with override color        
        self._color = color
        self._override_color = override_color
        self._smilies_scaled = {} # key: (index_pos), value(pixbuf)
        self._scaling = scaling # relative to ascent + desent, -1 for natural
        self._is_rtl = False
        self.set_element_list(parsed_elements_list)
        self._update_layout()
    def set_element_list(self, parsed_elements_list=['']):
        ''' Sets Layout Text based on parsed elements '''
        self._update_base(parsed_elements_list)
    def set_text(self, text):
        ''' Sets Layout Text '''
        self.set_element_list(text)
    def set_markup(self, markup):
        ''' Same as set_text() '''
        self.set_element_list(text)
    def set_width(self, width):
        ''' Set width of layout in pixels, -1 for natural width '''
        self._width = width
        self._update_layout()
    def get_width(self):
        return self._width 
    def set_ellipsize(self, value):
        ''' Turns Ellipsize ON/OFF '''
        if value == pango.ELLIPSIZE_END:
            self._ellipsize = True
        else:
            self._ellipsize = False
        self._update_layout()
    def set_smiley_scaling(self, smiley_scaling):
        '''
        Set smiley scalling relative to ascent + desent, 
        -1 for natural size
        '''
        self._scaling = smiley_scaling
        self._update_smilies()
    def set_in_color_override_mode(self, in_override):
        if not in_override == self._in_override:
            self._in_override = in_override
            self._update_attributes()
    def set_colors(self, color = gtk.gdk.Color(), 
                 override_color = gtk.gdk.Color()):
        self._color = color
        self._override_color = override_color
        self._update_attrlists()
    def _update_base(self, elements_list=['']):
        self._smilies = {}
        self._base_attrlist = pango.AttrList()
        text = ''
        if type(elements_list) in (str, unicode): 
            elements_list = [elements_list]
        for element in elements_list:
            if type(element) in (str, unicode):
                try:
                    attrl, ptxt, ac = pango.parse_markup(str(element), u'\x00')
                except:
                    attrl, ptxt = pango.AttrList(), str(element)
                shift = len(text)
                itter = attrl.get_iterator()
                while True:
                    attrs = itter.get_attrs()
                    for attr in attrs:
                        attr.end_index += shift
                        attr.start_index += shift
                        self._base_attrlist.insert(attr)
                    if not itter.next(): break
                text += ptxt
            elif type(element) == Smiley:
                self._smilies[len(text)] = element.getPixbuf(animated=False)                    
                text += '_'
        pango.Layout.set_text(self, text)
        if hasattr(pango, 'find_base_dir'):
            for line in text.splitlines():
                if (pango.find_base_dir(line,-1) == pango.DIRECTION_RTL):
                    self._is_rtl = True
                    break
        else:
            self._is_rtl = False
        logical = self.get_line(0).get_pixel_extents()[1]
        ascent = pango.ASCENT(logical)
        decent = pango.DESCENT(logical)
        self._text_height =  ascent + decent
        self._base_to_center = (self._text_height / 2) - decent
        self._update_smilies()
    def _update_smilies(self):    
        self._base_attrlist.filter(lambda attr: attr.type == pango.ATTR_SHAPE)
        self._smilies_scaled = {}
        if self._scaling >= 0: 
            max_height = self._text_height * self._scaling
        else: 
            max_height = sys.maxint                
        for index, pixbuf in self._smilies.iteritems():
            height, width = pixbuf.get_height(), pixbuf.get_width()
            npix = pixbuf.copy()
            if height > max_height:
                cairo_scale = float(max_height) / float(height)
                height = int(height * cairo_scale)
                width = int(width * cairo_scale)
                npix = npix.scale_simple(width, height, gtk.gdk.INTERP_BILINEAR)
            self._smilies_scaled[index] = npix
            rect = (0, -1 * (self._base_to_center + (height /2)) * pango.SCALE,\
                                     width * pango.SCALE, height * pango.SCALE)
            self._base_attrlist.insert(pango.AttrShape((0,0,0,0), rect, index, index + 1))
        self._update_attrlists()
    def _update_attrlists(self):
        clr = self._color
        oclr = self._override_color
        norm_forground = pango.AttrForeground( clr.red, 
                clr.green, clr.blue, 0, len(self.get_text()))
        override_forground = pango.AttrForeground( oclr.red, 
                oclr.green, oclr.blue, 0, len(self.get_text()))
        self._attrlist = pango.AttrList()
        self._attrlist.insert(norm_forground)
        self._override_attrlist = pango.AttrList()
        self._override_attrlist.insert(override_forground)                                
        itter = self._base_attrlist.get_iterator()
        while True:
            attrs = itter.get_attrs()
            for attr in attrs:
                self._attrlist.insert(attr.copy())
                if not (attr.type in (pango.ATTR_FOREGROUND, pango.ATTR_BACKGROUND)):
                    self._override_attrlist.insert(attr.copy())
            if not itter.next(): break
        self._update_attributes()            
    def _update_attributes(self):
        if self._in_override: 
            self.set_attributes(self._override_attrlist)
        else: 
            self.set_attributes(self._attrlist)
    def _update_layout(self):
        if self._width >= 0 and self._ellipsize == False: # if true, then wrap
            pango.Layout.set_width(self, self._width)
        else:
            pango.Layout.set_width(self, -1)
    def get_size(self):
        natural_width, natural_height = pango.Layout.get_size(self)        
        if self._width >= 0 and self._ellipsize : # if ellipsize
            return self._width, natural_height
        else: 
            return natural_width, natural_height   
    def get_pixel_size(self):
        natural_width, natural_height = pango.Layout.get_pixel_size(self)        
        if self._width >= 0 and self._ellipsize : # if ellipsize
            return pango.PIXELS(self._width), natural_height
        else: 
            return natural_width, natural_height     
    def draw(self, ctx, area):
        x, y, width, height = area
        pxls = pango.PIXELS
        ctx.rectangle(x, y , width, height)
        ctx.clip()
        if self._is_rtl:
            layout_width = pango.Layout.get_pixel_size(self)[0]
            ctx.translate(x + width - layout_width, y)
        else:        
            ctx.translate(x,y)        
            if self._width >= 0:        
                INLINE, BYTE, GRAPH = 0, 1, 2 
                X, Y, W, H = 0, 1, 2, 3       
                layout_width = self._width
                lst = self.get_attributes()
                e_ascent = pango.ASCENT(self._elayout.get_line(0).get_pixel_extents()[1])    
                coords = [] # of path in px
                for i in range(self.get_line_count()):
                    line = self.get_line(i)
                    edge = line.x_to_index(layout_width)                                        
                    if edge[INLINE]: 
                        attrlist = pango.AttrList()
                        itter = lst.get_iterator()
                        while True:
                            attrs = itter.get_attrs()
                            for attr in attrs:
                                if not attr.type == pango.ATTR_SHAPE:
                                    start, end = itter.range()
                                    if start <= edge[BYTE] < end:
                                        n_attr = attr.copy()
                                        n_attr.start_index = 0                
                                        n_attr.end_index = 3
                                        attrlist.insert(n_attr)
                            if not itter.next(): break
                        self._elayout.set_attributes(attrlist)
                        ellipsize_width = self._elayout.get_size()[0]          
                        edge = line.x_to_index(layout_width - ellipsize_width)                        
                        char = self.index_to_pos(edge[BYTE])
                        char_x, char_y, char_h = pxls(char[X]), pxls(char[Y]), pxls(char[H])
                        y1, y2 = char_y, char_y + char_h
                        if edge[INLINE]:
                            x1 = char_x
                        else:
                            x1 = 0         
                        coords.append((x1, y1))
                        coords.append((x1, y2))                 
                        line_ascent = pango.ASCENT(line.get_pixel_extents()[1])                                      
                        ctx.move_to(x1, y1 + line_ascent - e_ascent)
                        ctx.show_layout(self._elayout)
                    else:
                        char = self.index_to_pos(edge[BYTE])
                        coords.append((pxls(char[X] + char[W]), pxls(char[Y])))
                        coords.append((pxls(char[X] + char[W]), pxls(char[Y] + char[H])))
                if coords:
                    ctx.move_to(0, 0)
                    for x, y in coords:
                        ctx.line_to(x, y)
                    ctx.line_to(0, coords[-1][1])
                    ctx.close_path()
                    ctx.clip()
        ctx.move_to(0,0)
        ctx.show_layout(self)
        for index  in self._smilies.keys():
            x, y, width, height = self.index_to_pos(index)                        
            pixbuf = self._smilies_scaled[index] 
            tx = pxls(x) 
            ty = pxls(y) + (pxls(height)/2) - (pixbuf.get_height()/2)
            ctx.set_source_pixbuf(pixbuf, tx, ty)
            ctx.paint()
