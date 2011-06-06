import re
import gtk
import gtk.gdk
import pango
import cairo
import gobject
class cairoColor(object):
    '''An object that describes a color using
    red, green, blue attributes'''
    def __init__(self, gdkcolor):
        self.red = float(gdkcolor.red) / 65535
        self.green = float(gdkcolor.green) / 65535
        self.blue = float(gdkcolor.blue) / 65535
class FancyLabel( gtk.Widget ):
    '''Cairo-based label that renders (...) '''
    __gproperties__ = {
        'text': (str, '', '', '', gobject.PARAM_READWRITE),
        'markup': (bool, '', '', True, gobject.PARAM_READWRITE),
        'plusmarkup': (bool, '', '', True, gobject.PARAM_READWRITE),
        'smilies': (bool, '', '', True, gobject.PARAM_READWRITE),
        'max-lines': (int, '', '', -1, 10000, -1, gobject.PARAM_READWRITE),
        'max-line-height': (int, '', '', -1, 100, -1, gobject.PARAM_READWRITE),
        'wrap': (bool, '', '', True, gobject.PARAM_READWRITE),
        'wrap-by-char': (bool, '', '', False, gobject.PARAM_READWRITE),
        'ellipsize-end': (bool, '', '', True, gobject.PARAM_READWRITE),
    }
    __gsignals__ = {
        'size_request' : 'override',
        'expose-event' : 'override',
        'size_allocate' : 'override' ,
    }
    _colorCodes = (
    'ffffff','000000','00007D','009100','FF0000','7D0000','9A009A','FC7D00',
    'FFFF00','00FC00','009191','00FFFF','1E1EFC','FF00FF','7D7D7D','D2D2D2',
    'E7E6E4','cfcdd0','ffdea4','ffaeb9','ffa8ff','c1b4fc','bafbe5','ccffa3',
    'fafda2','b6b4b7','a2a0a1','f9c152','ff6d66','ff62ff','6c6cff','68ffc3',
    '000000','f9ff57','858482','6e6d7b','ffa01e','F92411','FF1EFF','1E29FF',
    '7dffa5','60f913','fff813','5e6464','4b494c','d98812','eb0505','de00de',
    '0000d3','03cc88','59d80d','d4c804','333335','18171c','944e00','9b0008',
    '980299','01038c','01885f','389600','9a9e15','473400','4d0000','5f0162',
    '000047','06502f','1c5300','544d05')
    _bbmarkupRE = re.compile( \
    r'(?P<bOpen>\[b\])|(?P<bClose>\[/b\])|(?P<iOpen>\[i\])|(?P<iClose>\[/i\])|'
    r'(?P<uOpen>\[u\])|(?P<uClose>\[/u\])|(?P<sOpen>\[s\])|(?P<sClose>\[/s\])|'
    r'(\[c=(?P<cOpen>([\w]{3,20})|(#([0-9a-f]{6,6}))|([\d]{1,2}))\])|'   
    r'(\[/c(?P<cClose>(=[\w]{3,20})|(=#([0-9a-f]{6,6}))|(=[\d]{1,2})|())\])|'
    r'(\[a=(?P<aOpen>([\w]{3,20})|(#([0-9a-f]{6,6}))|([\d]{1,2}))\])|'
    r'(\[/a(?P<aClose>(=[\w]{3,20})|(=#([0-9a-f]{6,6}))|(=[\d]{1,2})|())\])|',
    re.IGNORECASE)
    _smiliesRE = None
    _smilieDescent = 0.25   # percentage of smilie to use as descent
    _text = ''
    _markup = True          # render pango markup
    _plusmarkup = True      # render emesenePlus markup
    _smilies = True         # render smilies 
    _max_lines = -1         # set to -1 to render all lines
    _max_line_height = -1   # set to -1 for natural height,
    _wrap = True            # wrap text when true 
    _wrap_by_char = False   # False: wrap word, True: wrap characters 
    _ellipsize_end = True   # self explanitory
    _cachedSmilieDict = {}
    _smilieStack = []    
    _foregroundStack = []
    _backgroundStack = []
    _cairoForegroundStack = []
    _cairoBackgroundStack = []
    _layout = None
    _ellipsize_width = 0
    _cairo_scale = 1
    _fixed_layout_height = 0    # set on allocate, used for expose
    def __init__(self, controller, text=''):
        gobject.GObject.__init__(self)
        gtk.Widget.__init__(self)
        self.set_flags(self.flags() | gtk.NO_WINDOW )
        self._theme = controller.theme
        self._config = controller.config
        self._smiliesRE = self.__createSmiliesRE(self._theme.getSmileysList())
        self._text = text
        self.__setup_layout()
    def do_get_property(self, prop):
        if prop.name == 'text':
            return self._text
        elif prop.name == 'markup':
            return self._markup
        elif prop.name == 'plusmarkup':
            return self._plusmarkup
        elif prop.name == 'smilies':
            return self._smilies
        elif prop.name == 'max-lines':
            return self._max_lines
        elif prop.name == 'max-line-height':
            return self._max_line_height
        elif prop.name == 'wrap':
            return self._wrap
        elif prop.name == 'wrap-by-char':
            return self._wrap_by_char
        elif prop.name == 'ellipsize-end':
            return self._ellipsize_end
        else:
            raise AttributeError, 'unknown property %s' % prop.name
    def do_set_property(self, prop, value):
        if prop.name == 'text':
            self._text = value
            self.__setup_layout()
            self.queue_resize()
        elif prop.name == 'markup':
            self._markup = value
            self.__setup_layout()
            self.queue_resize()
        elif prop.name == 'plusmarkup':
            self._plusmarkup  = value
            self.__setup_layout()
            self.queue_resize()  
        elif prop.name == 'smilies':
            self._smilies = value
            self.__setup_layout()
            self.queue_resize()
        elif prop.name == 'max-lines':
            self._max_lines = value
            self.queue_resize()
        elif prop.name == 'max-line-height':
            self._max_line_height = value
            self.__setup_cairo_vars()
            self.queue_resize()
        elif prop.name == 'wrap':
            self._wrap = value
            self.queue_resize()   
        elif prop.name == 'wrap-by-char':
            self._wrap_by_char = value
            self.queue_resize()
        elif prop.name == 'ellipsize-end':
            self._ellipsize_end = value
            self.queue_resize()
        else:
            raise AttributeError, 'unknown property %s' % prop.name
    def set_text(self, text):
        self.set_property('text', text)
    def set_markup(self, text):
        self.set_property('text', text)
    def get_text(self):
        return self.get_property('text')
    def set_max_line_height(self, height):
        self.set_property('max-line-height', height)
    def get_max_line_height(self):
        return self.get_property('max-line-height')
    def set_use_markup(self, setting):
        self.set_property('markup', setting)
    def get_use_markup(self):
        return self.get_property('markup')
    def set_use_plusmarkup(self, setting):
        self.set_property('plusmarkup', setting)
    def get_use_plusmarkup(self):
        return self.get_property('plusmarkup')
    def set_use_smilies(self, setting):
        self.set_property('smilies', setting)
    def get_use_smilies(self):
        return self.get_property('smilies')
    def set_ellipsize_end(self, setting):
        self.set_property('ellipsize-end', setting)
    def get_ellipsize_end(self):
        return self.get_property('ellipsize-end')
    def set_max_lines(self, maxlines):
        self.set_property('max-lines', maxlines)
    def get_max_lines(self):
        return self.get_property('max-lines')
    def set_wrap(self, setting):
        self.set_property('wrap', setting)
    def get_wrap(self):
        return self.get_property('wrap')
    def set_wrap_by_char(self, setting):
        self.set_property('wrap-by-char', setting) 
    def get_wrap_by_char(self):
        return self.get_property('wrap-by-char')
    def __compare_attr(self, attribute):
        if attribute.type == pango.ATTR_FOREGROUND: 
            return True
        else:
            return False
    def __setup_layout(self):
        self._layout = self.create_pango_layout('')
        if self._markup:
            self._layout.set_markup(self._text)
        else:
            self._layout.set_text(self._text)
        attributes = self._layout.get_attributes()
        if attributes == None:
            attributes = pango.AttrList()
        plainText = self._layout.get_text()
        if self._plusmarkup:
            self.__parse_plusmarkup(attributes, plainText)
        if self._smilies:
            self.__parse_smilies(attributes, plainText)
        self._layout.set_attributes(attributes)
        self.__setup_cairo_vars()
    def __setup_cairo_vars(self):
        line_height = 0
        for i in range(self._layout.get_line_count()):
            current = self._layout.get_line(i).get_pixel_extents()[1][3]
            line_height = max(line_height, current)
        if 0 < self._max_line_height < line_height:
            self._cairo_scale = float(self._max_line_height) / line_height
        else:
            self._cairo_scale = 1
        ellipsize_layout = self.create_pango_layout('')
        ellipsize_layout.set_markup('<b>...</b>')
        self._ellipsize_width = ellipsize_layout.get_pixel_size()[0]
    def __parse_plusmarkup(self, attributes, text):
        self._foregroundStack = []
        self._backgroundStack = []
        bStack, iStack, uStack, sStack, cStack, aStack = [], [], [], [], [], []
        iterator = self._bbmarkupRE.finditer(text)
        for match in iterator:
            tags = match.groupdict()
            if not tags['bOpen'] == None:
                bStack.append(match.span()) 
            elif not tags['bClose'] == None and len(bStack)> 0:
                openTag, closeTag = bStack.pop(), match.span()
                attributes.insert(pango.AttrWeight(pango.WEIGHT_BOLD, \
                    openTag[1], closeTag[0]))
                self.__hideTag(attributes, openTag)
                self.__hideTag(attributes, closeTag)
            elif not tags['iOpen'] == None:
                iStack.append(match.span())
            elif not tags['iClose'] == None and len(iStack)> 0:
                openTag, closeTag = iStack.pop(), match.span()
                attributes.insert(pango.AttrStyle(pango.STYLE_ITALIC, \
                    openTag[1], closeTag[0]))
                self.__hideTag(attributes, openTag)
                self.__hideTag(attributes, closeTag)
            elif not tags['uOpen'] == None:
                uStack.append(match.span()) 
            elif not tags['uClose'] == None and len(uStack)> 0:
                openTag, closeTag = uStack.pop(), match.span()
                attributes.insert(pango.AttrUnderline(pango.UNDERLINE_SINGLE, \
                    openTag[1], closeTag[0]))
                self.__hideTag(attributes, openTag)
                self.__hideTag(attributes, closeTag)
            elif not tags['sOpen'] == None: sStack.append(match.span()) 
            elif not tags['sClose'] == None and len(sStack)> 0:
                openTag, closeTag = sStack.pop(), match.span()
                attributes.insert(pango.AttrStrikethrough(True, \
                    openTag[1], closeTag[0]))
                self.__hideTag(attributes, openTag)
                self.__hideTag(attributes, closeTag)
            elif not tags['cOpen'] == None:
                cStack.append((match.span(), \
                    self.__colorFromParser(tags['cOpen'])))
            elif not tags['cClose'] == None and len(cStack)> 0:
                openTag, colora = cStack.pop()
                closeTag = match.span()
                if len(tags['cClose']) > 0:
                    colorb = self.__colorFromParser(tags['cClose'])
                else:
                    colorb = colora
                self._foregroundStack.append(( openTag[1], closeTag[0], \
                    cairoColor(colora), cairoColor(colorb) ))
                self.__hideTag(attributes, openTag)
                self.__hideTag(attributes, closeTag)
            elif not tags['aOpen'] == None:
                aStack.append((match.span(), \
                    self.__colorFromParser(tags['aOpen'])))
            elif not tags['aClose'] == None and len(aStack)> 0:
                openTag, colora = aStack.pop()
                closeTag = match.span()
                if len(tags['aClose']) > 0:
                    colorb = self.__colorFromParser(tags['aClose'])
                else:
                    colorb = colora
                self._backgroundStack.append(( openTag[1], closeTag[0], \
                    cairoColor(colora), cairoColor(colorb) ))
                self.__hideTag(attributes, openTag)
                self.__hideTag(attributes, closeTag)
    def __hideTag(self, attrlist, span):
        attrlist.insert(pango.AttrShape((0, 0, 0, 0), (0, 0, 0, 0), \
            span[0], span[1]))
    def __colorFromParser(self, code):  
        codeFix = code.replace('#', '').replace('=', '')
        if len(codeFix)> 2:
            return gtk.gdk.color_parse(codeFix)
        else:
            if int(codeFix) < 68:
                code = self._colorCodes[int(codeFix)]
            else:
                code = self._colorCodes[1]
            return gtk.gdk.color_parse('#' + code)
    def __parse_smilies(self, attributes, plainText):
        self._cachedSmilieDict = {}
        self._smilieStack = []    
        if self._smiliesRE:
            iterator = self._smiliesRE.finditer(plainText)
        else:
            return
        for match in iterator:
            code = match.group()
            start, end = match.span()            
            if not self._cachedSmilieDict.has_key(code):
                self._cachedSmilieDict[code] = \
                    self._theme.getSmiley(code, False)
            pixbuf = self._cachedSmilieDict[code]
            height, width = pixbuf.get_height() , pixbuf.get_width()
            descent = int(height * self._smilieDescent)
            rect = (0, \
                (descent - height) * pango.SCALE, \
                width * pango.SCALE, \
                height * pango.SCALE)
            attributes.insert(pango.AttrShape( (0, 0, 0, 0), \
                rect, start, start + 1 ))
            if end > (start + 1):
                attributes.insert( \
                    pango.AttrShape((0, 0, 0, 0), (0, 0, 0, 0), start + 1, end))
            self._smilieStack.append((start, code)) # index, key
    def __createSmiliesRE(self, smilieList):
        def compare(x, y):
            if len(x) == len(y):
                return 0
            elif len(x) < len(y):
                return 1
            else:
                return -1
        special_characters = [r'\.', r'\^', r'\$', r'\+', r'\?', \
            r'\{', r'\}', r'\[', r'\]', r'\\', r'\|', r'\(', r'\)', r'\*']
        items = smilieList[:]
        items.sort(compare)
        items_expression = []
        for item in items:
            for character in special_characters:
                item = re.sub(character, character, item)
            items_expression.append(item)
        return re.compile( '|'.join(items_expression) )
    def do_size_request(self, requisition):
        if not self._layout:
            self.__setup_layout()
        self._layout.set_width(-1)
        width, height = self._layout.get_pixel_size()
        width, height = width * self._cairo_scale, height * self._cairo_scale
        if not self._wrap and not self._ellipsize_end:
            requisition.width = width
        elif self._wrap and not self._ellipsize_end:
            self._layout.set_width(0)
            if self._wrap_by_char: self._layout.set_wrap(pango.WRAP_CHAR)
            else: self._layout.set_wrap(pango.WRAP_WORD)
            requisition.width = self._layout.get_pixel_size()[0] * \
                self._cairo_scale
        elif not self._wrap and self._ellipsize_end:
            if width < (self._ellipsize_width * self._cairo_scale):
                requisition.width = width
            else:
                requisition.width = self._ellipsize_width * self._cairo_scale
        requisition.height = height
    def do_size_allocate(self, allocation):
        scale = self._cairo_scale
        if self._wrap:
            self._layout.set_width( (allocation.width/scale) * pango.SCALE)
            if self._wrap_by_char:
                self._layout.set_wrap(pango.WRAP_CHAR)
            else:
                self._layout.set_wrap(pango.WRAP_WORD)
            request_width = -1
        else:
            self._layout.set_width(-1)
            if self._ellipsize_end:
                request_width = -1
            else:
                request_width = self._layout.get_pixel_extents()[1][2] * scale
        height = 0
        for i in range(self.__get_fixed_line_count()):
            height += self._layout.get_line(i).get_pixel_extents()[1][3]
        self._fixed_layout_height = height * scale
        self.set_size_request(request_width, self._fixed_layout_height)
        self._cairoForegroundStack = self.__create_cairo_gradients( \
            self._foregroundStack)
        self._cairoBackgroundStack = self.__create_cairo_gradients( \
            self._backgroundStack)
        gtk.Widget.do_size_allocate(self, allocation)
    def __get_gradient_rectangle(self, a, b):
        temp_a = self._layout.index_to_pos(a)
        temp_b = self._layout.index_to_pos(b)
        x = pango.PIXELS(temp_a[0])
        y = pango.PIXELS(temp_a[1])
        width =  pango.PIXELS(temp_b[0] - temp_a[0])
        height = pango.PIXELS(temp_a[3])
        return gtk.gdk.Rectangle(x, y, width, height)
    def __create_cairo_gradients(self, stack):
        cairo_stack = []
        for item in stack:
            rectangles = []
            start_i, end_i, color_a, color_b = item
            i = 0
            start_line_found = False
            end_line_found = False
            gradient_width = 0
            for i in range(self._layout.get_line_count()):
                line = self._layout.get_line(i)
                lstart = line.start_index
                if lstart <= start_i < lstart + line.length:
                    start_line_found = True
                    if lstart < start_i <= lstart + line.length:
                        end_line_found = True
                        rect = self.__get_gradient_rectangle(start_i, end_i )
                        gradient_width += rect.width
                        rectangles.append(rect)
                        break
            if not start_line_found:
                break
            elif not end_line_found:
                rect = self.__get_gradient_rectangle(start_i, \
                    line.start_index + line.length )
                gradient_width += rect.width
                rectangles.append(rect)
                for j in range( i + 1 , self._layout.get_line_count() - 1 ):
                    line = self._layout.get_line(j)
                    start = line.start_index
                    if start < end_i <= start + line.length:
                        end_line_found = True
                        rect = self.__get_gradient_rectangle( \
                            line.start_index, end_i )
                        gradient_width += rect.width
                        rectangles.append(rect)
                        break
                    else:
                        rect = self.__get_gradient_rectangle( \
                            line.start_index, line.start_index + \
                            line.length - 1)
                        gradient_width += rect.width
                        rectangles.append(rect)
            if not end_line_found:
                break
            shift = 0
            for rect in rectangles:
                gradient = cairo.LinearGradient(rect.x - shift, rect.y, \
                    rect.x - shift + gradient_width, rect.y)   
                gradient.add_color_stop_rgb(0, \
                    color_a.red, color_a.green, color_a.blue)
                gradient.add_color_stop_rgb(1, \
                    color_b.red, color_b.green, color_b.blue)
                shift += rect.width
                cairo_stack.append((rect, gradient))
        return cairo_stack
    def __line_number_from_start_index(self, index):
        for i in range(self._layout.get_line_count()):
            line = self._layout.get_line(i)
            if line.start_index <= index < line.start_index + line.length:
                return i
        return -1
    def __line_number_from_end_index(self, index):
        for i in range(self._layout.get_line_count()):
            line = self._layout.get_line(i)
            if line.start_index < index <= line.start_index + line.length:
                return i
        return -1 
    def __get_fixed_line_count(self):
        if self._max_lines >= 0:
            return min(self._layout.get_line_count(), self._max_lines)
        else: return self._layout.get_line_count()
    def do_expose_event(self, evnt):
        cell_area = self.get_allocation()
        ctx =  self.window.cairo_create()
        ctx.translate(cell_area.x, cell_area.y)
        ctx.scale(self._cairo_scale, self._cairo_scale)
        cairo_width = cell_area.width / self._cairo_scale
        ctx.set_antialias(cairo.ANTIALIAS_SUBPIXEL)
        if self._ellipsize_end:
            path_coordinates = []
            x = 0
            y = 0
            for i in range(self.__get_fixed_line_count()):  
                line = self._layout.get_line(i) 
                clip_test = line.x_to_index(cairo_width * pango.SCALE)
                char_position = self._layout.index_to_pos(clip_test[1])
                if clip_test[0]: #if cliped
                    clip_test = line.x_to_index((cairo_width * pango.SCALE) - \
                        (self._ellipsize_width * pango.SCALE))
                    char_position = self._layout.index_to_pos(clip_test[1])
                    if clip_test[0]:
                        line_rect = gtk.gdk.Rectangle(0, y, \
                            char_position[0] / pango.SCALE, \
                            char_position[3] / pango.SCALE)
                    else:
                        line_rect = gtk.gdk.Rectangle(0, y, \
                            self._ellipsize_width, \
                                char_position[3] / pango.SCALE)   
                    ctx.save()
                    ellipsize_layout = self.create_pango_layout('...')
                    z1 = line.get_pixel_extents()[1][1]
                    z2 = ellipsize_layout.get_line(0).get_pixel_extents()[1][1]
                    ctx.move_to(line_rect.width, (line_rect.y - z1  + z2))
                    ctx.show_layout(ellipsize_layout)
                    ctx.restore()
                else:
                    line_rect = gtk.gdk.Rectangle(0, y, \
                        line.get_pixel_extents()[1][2], \
                        char_position[3] / pango.SCALE)
                x = line_rect.width
                path_coordinates.append((x, y))
                y += line_rect.height
                path_coordinates.append((x, y))
            x = 0
            path_coordinates.append((x, y))
            ctx.move_to(0, 0)
            for coordinate in path_coordinates:
                ctx.line_to(coordinate[0], coordinate[1])
            ctx.close_path()
            ctx.clip()
        else:
            ctx.rectangle(0, 0, cairo_width, self._fixed_layout_height)
            ctx.clip()
        for background in self._cairoBackgroundStack:
            rect, gradient = background
            ctx.save() 
            ctx.rectangle(rect.x, rect.y  , rect.width, rect.height)
            ctx.clip_preserve()
            ctx.set_source(gradient)
            ctx.fill()
            ctx.restore()
        ctx.save()
        ctx.move_to(0, 0)
        ctx.layout_path(self._layout)
        ctx.clip()
        ctx.show_layout(self._layout)
        ctx.restore()
        for foreground in self._cairoForegroundStack:
            rect, gradient = foreground
            ctx.save() 
            ctx.rectangle(rect.x, rect.y  , rect.width, rect.height)
            ctx.clip()
            ctx.move_to(0, 0)
            ctx.layout_path(self._layout)
            ctx.set_source(gradient)
            ctx.fill()
            ctx.restore()
        for smilie in self._smilieStack:
            index, key = smilie
            x, y, width, height = self._layout.index_to_pos(index)
            if self._cachedSmilieDict.has_key(key):
                pixbuf = self._cachedSmilieDict[key]
                ctx.save()
                ctx.set_source_pixbuf(pixbuf, x/pango.SCALE, y/pango.SCALE)
                ctx.paint()
                ctx.restore()
gobject.type_register( FancyLabel )
