import gtk
import cairo
import gobject
from math import pi
from pyisf import pyisf
PAINTBRUSH, ERASER = range(2)
GRID_SIZE = {0: -1, 1: 10, 2: 20, 3: 30}
class InkCanvas(gtk.DrawingArea):
    __gsignals__ = {
        'expose_event' : 'override',
        'motion_notify_event' : 'override',
        'button_press_event' : 'override',
        'button_release_event' : 'override',
        'realize': 'override',
        'configure-event': 'override',
    }
    def __init__(self, pointer_pixbuf=None, paintbrush_pixbuf=None,
                       erasor_pixbuf=None):
        gtk.DrawingArea.__init__(self)
        self.set_events(gtk.gdk.EXPOSURE_MASK | gtk.gdk.LEAVE_NOTIFY_MASK |
            gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK |
            gtk.gdk.POINTER_MOTION_MASK| gtk.gdk.POINTER_MOTION_HINT_MASK)
        self.history = []
        self.paths = []
        self.tool_type = PAINTBRUSH
        self.stroke_size = 8
        self.stroke_color = gtk.gdk.Color(0, 0, 0)
        self.grid_type = 0
        self.pointer_surface = None
        self.cached_grid_surface = None
        self.cached_painting_surface = None
        self.tool_type_to_surface = {}
        self.is_pressed = False
        self.intermediate_path = None
        self.pointer_surface = image_surface_from_pixbuf(pointer_pixbuf)
        self.tool_type_to_surface = {
            PAINTBRUSH: image_surface_from_pixbuf(paintbrush_pixbuf),
            ERASER: image_surface_from_pixbuf(erasor_pixbuf), 
        }
    def set_tool_type(self, tool_type=PAINTBRUSH):
        self.tool_type = tool_type
        self.update_cursor()
    def set_stroke_size(self, stroke_size= 10):
        if stroke_size < 1:
            self.stroke_size = 1
        else:
            self.stroke_size = stroke_size
        self.update_cursor()
    def set_grid_type(self, grid_type):
        self._grid_type = grid_type
        self.update_grid_surface(self.allocation.width, self.allocation.height)
        self.queue_draw()
    def get_grid_type(self):
        return self._grid_type
    grid_type = property(fset=set_grid_type, fget=get_grid_type)
    def undo(self):
        if len(self.paths) > 0:
            self.history.append(self.paths.pop())
            self.cached_painting_surface = self.build_paths_surface()
            self.queue_draw()
    def redo(self):
        if len(self.history) > 0:
            self.paths.append(self.history.pop())
            self.cached_painting_surface = self.build_paths_surface()
            self.queue_draw()
    def clear_canvas(self):
        self.cached_painting_surface = None
        self.paths = []
        self.history = []
        self.queue_draw()
    def get_pixbuf(self):
        rec = gtk.gdk.Rectangle()
        for path in self.paths:
            rec = rec.union(path.path_rectangle)
        width = max(1, rec.width)
        height = max(1, self.allocation.height)
        surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)
        ctx = cairo.Context(surface)
        for path in self.paths:
            path.draw(ctx)
        return gtk.gdk.pixbuf_new_from_data(
            surface.get_data(), 
            gtk.gdk.COLORSPACE_RGB, True, 8, 
            surface.get_width(), surface.get_height(), 
            surface.get_stride())
    def do_button_press_event(self, event):
        if event.button == 1:
            self.is_pressed = True
            self.history = []
            path = BrushPath((event.x, event.y), self.stroke_size,
                self.stroke_color, self.tool_type)
            self.paths.append(path)
            rec = path.intermediate_rectangle
            self.intermediate_path = path
            self.queue_draw_area(rec.x, rec.y, rec.width, rec.height)
        return True
    def do_motion_notify_event(self, event):
        x,y =  event.get_coords()
        if self.is_pressed:
            path = self.paths[-1]
            path.add((x, y))
            rec = path.intermediate_rectangle
            self.queue_draw_area(rec.x, rec.y, rec.width, rec.height)
        return True
    def do_button_release_event(self, event):
        if self.is_pressed:
            self.is_pressed = False
            self.intermediate_path = None
            self.update_cached_painting_surface(self.paths[-1])
        return True
    def do_expose_event(self, event):
        x , y, width, height = event.area
        ctx = event.window.cairo_create()
        surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)
        cr = cairo.Context(surface)
        cr.translate(-x, -y)
        if self.cached_painting_surface:
            cr.set_source_surface(self.cached_painting_surface, 0, 0)
            cr.paint()
        if self.intermediate_path:
            self.intermediate_path.draw(cr, 0, 0)
        self.draw_grid(cr, x, y)
        self.draw_canvas(cr, x, y, width, height)
        ctx.set_source_surface(surface, x, y)
        ctx.paint()
        return False
    def do_realize(self):
        gtk.DrawingArea.do_realize(self)
        self.update_cursor()
    def do_configure_event(self, event):
        self.update_grid_surface(event.width, event.height)
    def draw_grid(self, ctx, x,y):
        if self.cached_grid_surface is not None:
            ctx.save()
            ctx.set_operator(cairo.OPERATOR_DEST_OVER)
            ctx.set_source_surface(self.cached_grid_surface, 0, 0)
            ctx.paint()
            ctx.restore()
    def draw_canvas(self, ctx, x,y, width, height, color=None):
        ctx.save()
        set_context_color(ctx, color or gtk.gdk.color_parse('#ffffff'))
        ctx.set_operator(cairo.OPERATOR_DEST_OVER)
        ctx.paint()
        ctx.restore()
    def update_cached_painting_surface(self, path):
        if not self.cached_painting_surface:
            self.cached_painting_surface = cairo.ImageSurface(
                cairo.FORMAT_ARGB32, 1, 1)
        surface_width = self.cached_painting_surface.get_width()
        surface_height = self.cached_painting_surface.get_height()
        rec = path.path_rectangle
        combined = rec.union( gtk.gdk.Rectangle(0,0,surface_width,surface_height))
        if (surface_width < combined.width) or (surface_height < combined.height):
            self.cached_painting_surface = resize_image_surface(
                self.cached_painting_surface, combined.width, combined.height)
        ctx = cairo.Context(self.cached_painting_surface)
        path.draw(ctx)
        self.queue_draw_area(rec.x, rec.y, rec.width, rec.height)
    def update_grid_surface(self, width, height, color = gtk.gdk.color_parse('#C0C0C0')):
        self.cached_grid_surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)
        ctx = cairo.Context(self.cached_grid_surface)
        space = GRID_SIZE[self.grid_type]
        if space < 1: return
        x, y = 0,0
        position = x + space
        end = x + width
        ctx.save()
        set_context_color(ctx, color)
        ctx.set_line_width(1)
        position = x + space
        end = x + width
        while position < end:
            ctx.move_to(position, y)
            ctx.line_to(position, y + height)
            position += space
        position = y + space
        end = y + height
        while position < end:
            ctx.move_to(x, position)
            ctx.line_to(x + width, position)
            position += space
        ctx.stroke()
        ctx.restore()
    def build_paths_surface(self):
        rec = gtk.gdk.Rectangle()
        for path in self.paths:
            rec = rec.union(path.path_rectangle)
        width = max(1, rec.width)
        height = max(1, rec.height)
        surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)
        ctx = cairo.Context(surface)
        for path in self.paths:
            path.draw(ctx)
        return surface
    def update_cursor(self):
        indicator_surface = self.tool_type_to_surface[self.tool_type]
        radius = int(self.stroke_size/2.0)
        shift = radius +1
        cursor_width = int(radius + max(self.pointer_surface.get_width(),  indicator_surface.get_width()))
        cursor_height = int(radius + max(self.pointer_surface.get_height(), indicator_surface.get_height()))
        cursor_surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, cursor_width, cursor_height)
        ctx = cairo.Context(cursor_surface)
        ctx.save()
        ctx.set_line_width(1.0), ctx.set_source_rgb(0,0,0), ctx.set_dash([1,1],0)
        ctx.arc(shift,shift,radius, 0, 2 * pi), ctx.stroke()
        ctx.set_source_surface(self.pointer_surface, shift, shift), ctx.paint()
        ctx.set_source_surface(indicator_surface, shift, shift), ctx.paint()
        ctx.restore()
        pixbuf = gtk.gdk.pixbuf_new_from_data(
            cursor_surface.get_data(),
            gtk.gdk.COLORSPACE_RGB, True, 8, 
            cursor_surface.get_width(), cursor_surface.get_height(), 
            cursor_surface.get_stride())
        cursor = gtk.gdk.Cursor(self.window.get_display(), pixbuf, shift, shift)
        self.window.set_cursor(cursor)
    def build_isf(self):
        enc = pyisf.IsfEncoder(self.paths)
        return enc
    def import_isf(self, data, debug=False):
        dec = pyisf.IsfDecoder(data, True)
        if debug:
            dec.print_info()
        self.paths = dec.paths
        self.cached_painting_surface = self.build_paths_surface()
        self.queue_draw()
class BrushPath(object):
    '''Represents a single brush path and has the ability to draw itself'''
    def __init__(self, start_point, stroke_size, color=None, tool_type=PAINTBRUSH):
        self.points = []
        self.stroke_size = stroke_size
        self.shift = int((stroke_size/2) + 1)
        self.color = color or gtk.gdk.Color(0,0,0)
        self.tool_type = tool_type
        if tool_type == PAINTBRUSH:
            self.operator = cairo.OPERATOR_OVER
        else: # tool_type == ERASER:
            self.operator = cairo.OPERATOR_CLEAR
        self.path_rectangle = gtk.gdk.Rectangle()
        self.intermediate_rectangle = gtk.gdk.Rectangle()
        self.add(start_point)
    def add(self, point):
        self.points.append(point)
        i_rec = self.create_point_rectangle(*point)
        self.path_rectangle = self.path_rectangle.union(i_rec)
        i_points = self.points[-2:len(self.points)]
        for pnt in i_points[0:len(i_points)-1]:
            z_rec = self.create_point_rectangle(*pnt)
            i_rec = i_rec.union(z_rec)
        self.intermediate_rectangle = i_rec
    def draw(self, ctx, x=0, y=0):
        points = self.points
        if not points: return
        ctx.save()
        ctx.set_operator(self.operator)
        ctx.translate(x,y)
        x, y, width, height = self.path_rectangle
        ctx.rectangle(x,y, width, height)
        ctx.clip()
        ctx.set_antialias(cairo.ANTIALIAS_SUBPIXEL)
        ctx.set_line_cap(cairo.LINE_CAP_ROUND)
        ctx.set_line_join(cairo.LINE_JOIN_ROUND)
        set_context_color(ctx, self.color)
        ctx.set_line_width(self.stroke_size)
        ctx.move_to(*points[0])
        for point in points[1:len(points)]:
            ctx.line_to(*point)
        if len(points) == 1:
            ctx.line_to(*points[0])
        ctx.stroke()
        ctx.restore()
    def create_point_rectangle(self, x, y):
        return gtk.gdk.Rectangle( 
            int(x - self.shift), int(y - self.shift), 
            int(self.shift * 2), int(self.shift * 2))
class ColorLabel(gtk.Widget):
    '''Square shaped, color label'''
    def __init__(self, color=None):
        gtk.Widget.__init__(self)
        self.set_flags(self.flags() | gtk.NO_WINDOW)
        self.dimention = 20
        self.color = color
    def do_size_request(self,requisition):
        requisition.width = self.dimention
        requisition.height = self.dimention
    def do_expose_event(self, event):
        x , y, width, height = event.area
        dimention = min(width,height)
        y += (height /2) - (dimention /2)
        width, height = dimention, dimention
        ctx = event.window.cairo_create()
        ctx.translate(x, y) 
        ctx.set_antialias(cairo.ANTIALIAS_SUBPIXEL)
        ctx.rectangle(0, 0, width, height)
        ctx.clip_preserve()
        set_context_color(ctx, self.color)
        ctx.fill_preserve()
        ctx.set_line_width(1.0)
        ctx.set_source_rgb(0, 0, 0)
        ctx.stroke()
gobject.type_register(ColorLabel)
class ColumnContainer(gtk.Box):
    ''' Container that positions widgets into a given column number'''
    def __init__(self, column_number=1):
        gtk.Box.__init__(self)
        if column_number < 1:
            self.cols = 1
        else:
            self.cols = column_number
    def update_child_info(self):
        visual_children_number = 0
        child_requisition_width = 0
        child_requisition_height = 0
        for child in self.get_children():
            if child.get_property('visible'):
                child_request_width, child_request_height = child.size_request()
                child_requisition_width = max(child_requisition_width, child_request_width)
                child_requisition_height = max(child_requisition_height, child_request_height)
                visual_children_number += 1
        self.cr_width = child_requisition_width
        self.cr_height = child_requisition_height
        self.vc_number = visual_children_number
    def do_size_request(self, requisition):
        self.update_child_info()
        width, height = 0, 0
        if self.vc_number > 0:
            width = self.cr_width * self.cols
            if self.vc_number <= self.cols:
                height = self.cr_height
            elif (self.vc_number % self.cols ) == 0 :
                height = self.cr_height * ( self.vc_number / self.cols)
            else:
                height = self.cr_height * (( self.vc_number / self.cols) + 1)
        requisition.width = width
        requisition.height = height
    def do_size_allocate(self, allocation):
        width = allocation.width / self.cols
        height = self.cr_height
        x, y = allocation.x, allocation.y
        col_spaces_remaining = self.cols
        for child in self.get_children():
            child.size_allocate(gtk.gdk.Rectangle(x, y, width, height))
            col_spaces_remaining -= 1
            if col_spaces_remaining == 0:
                y += height
                x = allocation.x
                col_spaces_remaining = self.cols
            else:
                x += width
gobject.type_register(ColumnContainer)
class DropdownWindow(gtk.Window):
    def __init__(self):
        gtk.Window.__init__(self, gtk.WINDOW_POPUP)
        self.set_type_hint(gtk.gdk.WINDOW_TYPE_HINT_MENU)
        self.set_decorated(False)
    def do_map_event(self, event):
        self.grab_focus()
        self.grab_add()
        time = gtk.get_current_event_time()
        gtk.gdk.pointer_grab(self.window, True, gtk.gdk.BUTTON_PRESS_MASK, None, None, time)
        gtk.gdk.keyboard_grab(self.window, True, time)
    def do_unmap_event(self, event):
        time = gtk.get_current_event_time()
        gtk.gdk.pointer_ungrab(time)
        gtk.gdk.keyboard_ungrab(time)
        self.grab_remove()
    def do_button_press_event(self, event):
        win_tuple = gtk.gdk.window_at_pointer()
        if not win_tuple or not win_tuple[0] == self.window:
            self.hide()
        return True
    def do_key_press_event(self, event):
        if not gtk.Window.do_key_press_event(self, event) \
           and event.keyval == gtk.keysyms.Escape:
            self.hide()
            return True
class SimpleMenuToggleButton(gtk.ToggleButton):
    def __init__(self, image):
        gtk.ToggleButton.__init__(self)
        self.set_property("can-default", False)
        self.set_property("can-focus", False)
        self.set_border_width(0)
        self.set_relief(gtk.RELIEF_NONE)
        self.hbox = gtk.HBox()
        self.hbox.pack_start(image)
        self.hbox.pack_start(gtk.Arrow(gtk.ARROW_DOWN, gtk.SHADOW_IN))
        self.add(self.hbox)
        self.selected = None
    def menu_position(self, menu):
        x, y = self.window.get_origin()
        alloc = self.allocation
        x, y = x + alloc.x, y + alloc.y
        menu_allocation = menu.allocation
        width, height = menu_allocation.width, menu_allocation.height
        screen = self.get_screen()
        if x + width > screen.get_width():
            x += alloc.width - width
        if y + alloc.height + height > screen.get_height():
            y -= height
        else:
            y += alloc.height
        return (x, y, True)
class ColorButton(SimpleMenuToggleButton):
    __gsignals__ = {
        'color-set' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,(gobject.TYPE_PYOBJECT,)),
        }
    tango_colors = ['#2e3436', '#babdb6', '#a40000', '#5c3566', '#204a87',
        '#4e9a06', '#8f5902', '#ce5c00', '#c4a000', '#555753', '#d3d7cf',
        '#cc0000', '#75507b', '#3465a4', '#73d216', '#c17d11', '#f57900',
        '#edd400', '#888a85', '#eeeeec', '#ef2929', '#ad7fa8', '#729fcf',
        '#8ae234', '#e9b96e', '#fcaf3e', '#fce94f']
    def __init__(self, widget):
        SimpleMenuToggleButton.__init__(self, widget)
        self.dropdown_win = DropdownWindow()
        self.dropdown_win.realize()
        setactive = lambda x, y, val: self.set_active(val)
        self.dropdown_win.connect("map-event", setactive, True)
        self.dropdown_win.connect("unmap-event", setactive, False)
        container = ColumnContainer(9)
        hbox = gtk.HBox()
        def create_button(hex_color='#000000'):
            label = ColorLabel(gtk.gdk.color_parse(hex_color))
            button = gtk.Button()
            button.set_relief(gtk.RELIEF_NONE)
            button.set_property("can-default", False)
            button.set_property("can-focus", False)
            button.set_border_width(0)
            button.add(label)
            button.connect('clicked', self.__color_selected, label.color)
            return button
        for hex_color in self.tango_colors:
            container.add(create_button(hex_color))
        for hex_color in ('#000000', '#ffffff'):
            hbox.pack_start(create_button(hex_color), False, False)
        vbox = gtk.VBox()
        vbox.pack_start(hbox, False, False)
        vbox.pack_start(container, False, False)
        self.dropdown_win.add(vbox)
        self.color = gtk.gdk.Color(0, 0, 0)
    def __color_selected(self, widget, color):
        self.color = color
        self.dropdown_win.hide()
        self.emit('color-set', self.color)
    def do_button_press_event(self, event):
        if not self.get_active():
            self.dropdown_win.move(*self.menu_position(self.dropdown_win)[:2])
            self.dropdown_win.show_all()
            self.dropdown_win.present()
        return True
class GridButton(SimpleMenuToggleButton):
    __gsignals__ = {
        'grid-type-set' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,(gobject.TYPE_PYOBJECT,)),
        }
    def __init__(self, button_icon, blank_icon, small_icon, medium_icon,
            large_icon, grid_type=0):
        SimpleMenuToggleButton.__init__(self, button_icon)
        self.menu = gtk.Menu()
        self._grid_type = grid_type
        self.items = []
        def item_activate(menuitem):
            self.selected = menuitem
            self.grid_type = self.items.index(menuitem)
            self.emit('grid-type-set', self.grid_type)
        def create_grid_item(label, icon):
            grid_item = gtk.ImageMenuItem(label)
            grid_item.set_image(icon)
            grid_item.connect('activate', item_activate)
            self.menu.append(grid_item)
            self.items.append(grid_item)
        create_grid_item('Blank', blank_icon)
        self.menu.append(gtk.SeparatorMenuItem())
        create_grid_item('Small grid', small_icon)
        create_grid_item('Medium grid', medium_icon)
        create_grid_item('Large grid', large_icon)
    def do_button_press_event(self, event):
        if not self.get_active():
            self.menu.popup(None, None, self.menu_position,
                event.button, event.time)
            self.set_active(True)
            self.menu.show_all()
            if self.selected:
                self.menu.select_item(self.selected)
        return True
def set_context_color(context, color):
    r = float(color.red) / 65535
    g = float(color.green) / 65535
    b = float(color.blue) / 65535
    context.set_source_rgb(r,g,b)
def image_surface_from_pixbuf( pixbuf=None):
    if pixbuf is not None:
        return cairo.ImageSurface.create_for_data(
            pixbuf.get_pixels_array(),
            cairo.FORMAT_ARGB32,
            pixbuf.get_width(),
            pixbuf.get_height(),
            pixbuf.get_rowstride())
    else:
        return cairo.ImageSurface(cairo.FORMAT_ARGB32, 1, 1)
def resize_image_surface(surface, new_width, new_height):
    new_surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, new_width, new_height)
    cr = cairo.Context(new_surface)
    cr.set_source_surface(surface, 0, 0)
    cr.paint()
    return new_surface
