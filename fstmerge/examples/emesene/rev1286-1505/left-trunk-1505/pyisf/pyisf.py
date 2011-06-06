import struct
import drawing
import InkDraw
import gtk.gdk
class IsfBin( object ):
    '''Base class to encoder and decoder, represents
        the information read from or to be stored in
        a binary Isf file, as well as the functions
        necessary to do so '''
    def __init__( self, data = '' ):
        self.data = data
        self.pointer = 0
        self.bit = 0
    def pop( self, length = 1 ):
        '''gets the next $len chars from the data stack,
        and increment the pointer'''
        start = self.pointer
        end = self.pointer + length
        self.pointer += length
        return self.data[start:end]
    def pops( self, format ):
        '''Reads formatted data from the struct.  Supports, in
            in addition to anthing supported by struct, mbuint32/64,
            and mbsint32'''
        if format.startswith('mbuint32'):
            next = True
            temp = 0
            bitcount = 0
            while next:
                byte = struct.unpack( '<B', self.pop(1) )[0]
                num = byte & 0x7f
                temp *= pow(2, 7)
                temp = temp | num
                bitcount += 7
                if byte > 0x7f and bitcount < 29:
                    next = True
                else:
                    next = False
            return temp
        elif format.startswith('mbuint64'):
            next = True
            temp = 0
            bitcount = 0
            while next:
                byte = struct.unpack( '<B', self.pop(1) )[0]
                num = byte & 0x7f
                temp *= pow(2, 7)
                temp = temp | num
                bitcount += 7
                if byte > 0x7f and bitcount < 57:
                    next = True
                else:
                    next = False
            return temp
        elif format.startswith('weirdoint'):
            next = True
            temp = 0
            bitcount = 0
            while next:
                byte = struct.unpack( '<B', self.pop(1) )[0]
                num = byte & 0x7f
                temp = temp + num * pow(2, bitcount)
                bitcount += 7
                if byte > 0x7f:
                    next = True
                else:
                    next = False
            return temp
        elif format.startswith('mbsint32'):
            temp = self.pops('mbuint64')
            if temp & 1:
                temp *= -1
            return int(temp / 2)
        else:
            size = struct.calcsize(format) 
            return struct.unpack( format, self.pop(size) )
    def push( self, newdata, insert = False ):
        '''adds $newdata to the data stack, if insert is True then
            it adds it to the beginning of the stack'''
        if insert:
            self.data = newdata + self.data
        else:
            self.data += newdata
    def pushs( self, format, *vars ):
        '''Pushes formatted data onto the struct.  Supports, in
            in addition to anthing supported by struct, mbuint32/64,
            and mbsint32'''
        insert = False
        if format.startswith('mbuint32'):
            if len(vars) > 1:
                print 'Only one value per', format
            value = vars[0]
            if value > 0xfffffff or value < 0:
                print 'Value not within', format, 'range'
            new = True
            cont = False
            num = 0
            values = []
            while new or cont:
                for i in range(7):
                    bit = value % 2
                    value = int(value / 2)
                    if bit:
                        num += pow(2, i)
                    if value > 0:
                        cont = True
                    else:
                        cont = False
                if new:
                    new = False
                elif cont:
                    num += pow(2, 7)
                values.insert(0, num)
                num = 0
            if insert:
                for val in values.reverse():
                    self.push( struct.pack( '<B', val, insert = True ) )
            else:
                for val in values:
                    self.push( struct.pack( '<B', val ) )
        elif format.startswith('mbuint64'):
            if len(vars) > 1:
                print 'Only one value per', format
            value = vars[0]
            if value > 0xfffffffffffffff or value < 0:
                print 'Value not within', format, 'range'
            new = True
            cont = False
            num = 0
            values = []
            while new or cont:
                for i in range(7):
                    bit = value % 2
                    value = int(value / 2)
                    if bit and i < 7:
                        num += pow(2, i)
                    if value > 0:
                        cont = True
                    else:
                        cont = False
                if new:
                    new = False
                else:
                    num += pow(2, 7)
                values.insert(0, num)
                num = 0
            if insert:
                for val in values.reverse():
                    self.push( struct.pack( '<B', val, insert = True ) )
            else:
                for val in values:
                    self.push( struct.pack( '<B', val ) )
        elif format.startswith('mbsint32'):
            if len(vars) > 1:
                print 'Only one value per', format
            value = vars[0]
            if abs(value) > 0xfffffff:
                print 'Value not within', format, 'range'
            if value < 0:
                sign = 1
                value = abs(value)
            else:
                sign = 0
            value = value * 2 + sign
            self.pushs('mbuint64', value)
        elif format.startswith('weirdoint'):
            if len(vars) > 1:
                print 'Only one value per', format
            value = vars[0]
            new = True
            cont = False
            num = 0
            values = []
            while new or cont:
                for i in range(7):
                    bit = value % 2
                    value = int(value / 2)
                    if bit and i < 7:
                        num += pow(2, i)
                    if value > 0:
                        cont = True
                    else:
                        cont = False
                if new:
                    new = False
                else:
                    num += pow(2, 7)
                values.append(num)
                num = 0
            if insert:
                for val in values.reverse():
                    self.push( struct.pack( '<B', val, insert = True ) )
            else:
                for val in values:
                    self.push( struct.pack( '<B', val ) )
        else:
            self.push(struct.pack(format, *vars))
class ImageDescriptor( object ):
    '''A class that represents a single image'''
    taglist = [
    'INK_SPACE_RECT', 'GUID_TABLE', 'DRAW_ATTRS_TABLE',
    'DRAW_ATTRS_BLOCK', 'STROKE_DESC_TABLE', 'STROKE_DESC_BLOCK',
    'BUTTONS', 'NO_X', 'NO_Y', 'DIDX', 'STROKE', 'STROKE_PROPERTY_LIST',
    'POINT_PROPERTY', 'SIDX', 'COMPRESSION_HEADER', 'TRANSFORM_TABLE',
    'TRANSFORM', 'TRANSFORM_ISOTROPIC_SCALE', 'TRANSFORM_ANISOTROPIC_SCALE',
    'TRANSFORM_ROTATE', 'TRANSFORM_TRANSLATE', 'TRANSFORM_SCALE_AND_TRANSLATE',
    'TRANSFORM_QUAD', 'TIDX', 'METRIC_TABLE', 'METRIC_BLOCK', 'MIDX',
    'MANTISSA', 'PERSISTENT_FORMAT', 'HIMETRIC_SIZE', 'STROKE_IDS']
    tagformat = [
    'pass', 'V', 'V', 'V', 'V', 'V', 'pass', 'pass', 'pass', 'mbuint32', 'V',
    'pass', 'pass', 'mbuint32', 'pass', 'V', '<ffffff', '<f', '<ff', 'mbuint32',
    '<ff', '<ffff', '<IIIIII', 'mbuint32', 'V', 'V', 'pass', 'mbuint64', 'V', 
	'V', 'V']
	
    bitamounts = [
		[0, 1, 2, 4, 6, 8, 12, 16, 24, 32], [0, 1, 1, 2, 4, 8, 12, 16, 24, 32],
        [0, 1, 1, 1, 2, 4, 8, 14, 22, 32], [0, 2, 2, 3, 5, 8, 12, 16, 24, 32],
        [0, 3, 4, 5, 8, 12, 16, 24, 32], [0, 4, 6, 8, 12, 16, 24, 32],
        [0, 6, 8, 12, 16, 24, 32], [0, 7, 8, 12, 16, 24, 32]]
    def __init__( self ):
        self.paths = []
        self.isf_tag = 0
        self.isf_size = 0
        self.himetric_size = [0, 0]
        self.indices = {
            'DIDX':0,
            'TIDX':0,
            'MIDX':0,
            'SIDX':0 }
        self.transforms = []
        self.draw_attrs_table = []
        self.size = 8
    def print_info( self ):
        '''Prints some information about the isf file'''
        print 'ISFTag:', self.isf_tag
        print 'ISFSize:', self.isf_size
        print 'Image Size:', self.himetric_size[0], 'x', self.himetric_size[1]
        print 'Transforms:'
        for trans in self.transforms:
            print trans
        print 'Paths:'
        for path in self.paths:
            path.print_info()
        print 'Indices:', self.indices
class IsfEncoder( ImageDescriptor ):
    '''encodes an isf file'''
    def __init__( self, paths, debug=False):
        self.paths = paths
        self.enc = IsfBin()
        self.fill()
    def fill( self ):
        '''Fills the data array'''
        for path in self.paths:
            stroke = self.enc_stroke(path)
            self.enc.pushs('mbuint32', taglist.index('STROKE'))
            self.enc.pushs('mbuint64', len(stroke))
            self.enc.data += stroke
            self.enc.pointer += len(stroke)
        self.add_headers()
    def add_headers( self ):
        '''Adds basic information to the top of the file'''
        head = IsfBin()
        head.pushs('mbuint32', 0)
        head.pushs('mbuint64', len(self.enc.data))
        self.enc.data = head.data + self.enc.data
        self.enc.pointer += len(head.data)
    def enc_stroke(self, path, enc):
        num_pts = len(path._points)
        stroke = IsfBin()
        stroke.pushs('weirdoint', num_pts)
        guid_x, guid_y = [], []
        for point in path._points:
            guid_x.append(point.x)
            guid_y.append(point.y)
        if num_pts > 1:
            packet_x = enc_huffman(guid_x)
            packet_y = enc_huffman(guid_y)
            tag = '\x82'
        else:
            packet_x = enc_gorilla(guid_x)
            packet_y = enc_gorilla(guid_y)
        stroke.data += tag + packet_x + tag + packet_y
        stroke.data = struct.pack('<B', len(stroke.data)) + stroke.data
        return stroke.data
    def enc_huffman(self, guid):
        stream = Bitstream()
        enc_huffvalue(guid[0], stream)
        prevdelta = 0
        curdelta = 0
        i = 1
        while i < len(guid):
            curdelta = guid[i] - guid[i-1]
            diff = curdelta - prevdelta
            prevdelta = curdelta
            enc_huffvalue(diff, stream)
    def enc_huffvalue(self, val, stream):
        temp = val
        length = 0
        while temp:
            length += 1
            temp /=2
        while not bitamounts[2].count(length):
            length += 1
        index = bitamounts[2].index(length)
        stream.push_count(index)
        stream.push_int(val, length)
class transform( object ):
    def __init__( self):
        self.a = 1
class IsfDecoder( ImageDescriptor ):
    '''decodes an isf file'''
    def __init__( self, data, debug=False):
        ImageDescriptor.__init__( self )
        self.tagdict = {
        'STROKE':self.dec_stroke,
        'DIDX':self.dec_index,
        'SIDX':self.dec_index,
        'TIDX':self.dec_index,
        'MIDX':self.dec_index,
        'DRAW_ATTRS_BLOCK':self.dec_dablock,
        'DRAW_ATTRS_TABLE':self.dec_datable
        }
        dec = IsfBin(data)
        self.debug = debug
        self.fill( dec )
    def fill( self, dec ):
        '''reads the data and fills each field of the file'''
        dec.pointer = 0
        self.isf_tag = dec.pops('mbuint32')
        if self.debug:
            print 'ISFTag', self.isf_tag
        self.isf_size = dec.pops('mbuint64')
        if self.debug:
            print 'ISFSize', self.isf_size
        while True:
            try:
                tagnum = dec.pops('mbuint32')
                tag = self.taglist[tagnum]
                if self.debug:
                    print 'Tag', tag
                format = self.tagformat[tagnum]
            except IndexError:
                if self.debug:
                    print 'Uknown Tag Value'
            except struct.error:
                if self.debug:
                    print 'End of file'
                return
            except:
                raise
            else:
                prnt = 1
                if format == 'pass':
                    pass
                elif format == 'V':
                    payload_size = dec.pops('mbuint64')
                    if self.debug:
                        print '\tPayload Size', payload_size
                    payload = dec.data[dec.pointer:dec.pointer+payload_size]
                    if self.tagdict.has_key(tag):
                        prnt = 0
                        self.tagdict[tag](tag, payload)
                    if self.debug and prnt:
                        for char in payload:
                            print '\t', hex(struct.unpack('<B', char)[0])
                    dec.pointer += payload_size
                else:
                    payload = dec.pops(format)
                    if self.tagdict.has_key(tag):
                        prnt = 0
                        self.tagdict[tag](tag, payload)
                    if self.debug and prnt:
                        print '\tPayload', payload
    def process_tag(self, tag, payload):
        '''Performs specific operations with supported tags'''
        if tag == 'STROKE_IDS':
            dat = IsfBin(payload)
            num_pts = dat.pops('mbuint32')
            stream = Bitstream(payload[dat.pointer:])
            ids = self.dec_packet(stream, num_pts)
            if self.debug:
                print '\t', ids
        elif tag == 'HIMETRIC_SIZE':
            dat = IsfBin(payload)
            width = dat.pops('mbsint32')
            height = dat.pops('mbsint32')
            self.himetric_size = [width, height]
        else: return 1
    def dec_datable( self, tag, payload ):
        dat = IsfBin(payload)
        index = 0
        while True:
            try:
                block_size = dat.pops('mbuint32')
                self.dec_dablock(tag, dat.data[dat.pointer:dat.pointer + block_size], index)
                dat.pointer += block_size
            except struct.error:
                if self.debug:
                    print 'End of DRAW_ATTR_TABLE'
                    for table in self.draw_attrs_table:
                        print table.color.to_string()
                return
            except:
                raise
            index += 1
    def dec_dablock( self, tag, payload, index = 0 ):
        dat = IsfBin(payload)
        if index >= len(self.draw_attrs_table):
            self.draw_attrs_table.append(drawing.DrawAttributes())
            print 'Appended'
        color = gtk.gdk.Color(0, 0, 0)
        while True:
            try:
                guid = dat.pops('mbuint32')
            except struct.error:
                if self.debug:
                    print 'End of DRAW_ATTR_BLOCK'
                self.draw_attrs_table[index].color = color
                return
            except:
                raise
            else:
                print '\tGUID', hex(guid)
                if guid != 0x57:
                    data = dat.pops('mbuint32')
                    print '\t\tData', hex(data)
                else:
                    data = dat.pops('<BBBB')
                if guid == 0x44:
                    print hex(data)
                    red = data & 0x7f
                    red = (red * 65535) / 127
                    green = (data & 0x3f80) / 0x80
                    green = (green * 65535) / 127
                    blue = (data & 0x1f6000) / 0x4000
                    blue = (blue * 65535) / 127
                    color = gtk.gdk.Color(green, red, blue)
    def dec_index( self, tag, payload ):
        self.indices[tag] = payload
        print '\tIndices:', self.indices
    def dec_stroke( self, tag, payload ):
        '''Decodes stroke data from the payload, including
            the number of data points, the x values, and the
            y values'''
        dat = IsfBin(payload)
        num_pts = dat.pops('weirdoint')
        num_bytes = dat.pointer
        dat = None
        if self.debug:
            print '\tNumPoints:', num_pts
        stream = Bitstream( payload[num_bytes:] )
        guid_x = self.dec_packet(stream, num_pts)
        guid_y = self.dec_packet(stream, num_pts)
        if guid_x == -1 or guid_y == -1:
            guid_x = [0]
            guid_y = [0]
        dattrs = self.draw_attrs_table[self.indices['DIDX']]
        path = InkDraw.BrushPath((guid_x[0], guid_y[0]), dattrs.stroke_size,
            dattrs.color, dattrs.tool_type)
        for i in range(1, len(guid_x)):
            path.add((guid_x[i], guid_y[i]))
        self.paths.append(path)
    def dec_packet(self, stream, num_pts):
        '''Function that decodes *ISF/PACKET* formatted data from
            a Bitstream class object, and the number of points
            to be decoded'''
        tag = stream.pop_byte()
        if tag & 0xc0 == 0x80:
            if self.debug:
                print '\tAdaptive-Huffman Compression'
            index = tag & 0x1f
            try:
                pts = self.dec_huffman(stream, index, num_pts)
            except:
                if self.debug == 0:
                    pts = -1
                else:
                    raise
        else:
            if self.debug:
                print '\tGorilla Compression'
            transformation = tag & 0x20
            if transformation == 0x20:
                print "\tError: Gorilla transformation unimplemented", hex(tag)
            width = tag & 0x1f
            if not width:
                width = 32
            try:
                pts = self.dec_gorilla(stream, width, num_pts)
            except:
                if self.debug == 0:
                    pts = -1
                else:
                    raise
        return pts
    def dec_huffman( self, stream, index, num_pts ):
        '''Decodes num_pts values from a Bistream object, using
            bitamounts[index] to decode it using an Adaptive-Huffman
            coding scheme'''
        huffbase = []
        bitamount = self.bitamounts[index]
        huffbase = [0]
        base = 1
        for value in bitamount:
            if value != 0:
                huffbase.append(base)
                base += pow(2, value-1)
        n_count = 0
        value = []
        val = 0
        while len(value) < num_pts:
            bit = stream.pop_bit()
            if bit:
                n_count += 1
            else:
                if n_count == 0:
                    val = 0
                elif n_count < len(bitamount):
                    offset = stream.pop_bits_to_int(bitamount[n_count])
                    sign = offset & 0x1
                    offset /= 2
                    val = huffbase[n_count] + offset
                    if sign:
                        val = val * -1
                elif n_count == len(bitamount):
                    print '64-bit'
                    val = 0
                else:
                    print 'error'
                    val = 0
                n_count = 0
                value.append(val)
        if stream.bit != 0:
            stream.next_byte()
        curdelta = 0
        prevdelta = 0
        results = []
        for val in value:
            newdelta = curdelta*2 - prevdelta + val
            prevdelta = curdelta
            curdelta = newdelta
            results.append(newdelta)
        return results
    def dec_gorilla( self, stream, width, num_pts ):
        '''Decides num_pts values from the Bistream object 'stream',
            using a fixed width of width'''
        mask = ( 0xffffffff * pow(2, width-1) ) & 0xffffffff
        values = []
        while len(values) < num_pts:
            value = stream.pop_bits_to_int(width)
            if value & mask:
                value = value | mask
            string = struct.pack('q', value)
            values.append(struct.unpack('q', string)[0])
        stream.next_byte()
        return values
class Bitstream( object ):
    '''Takes a character string and turns it into a bitstream'''
    def __init__( self, data = '\x00'):
        self.data = data
        self.bit = 0
        self.index = 0
    def pop_byte( self ):
        '''Removes a byte from the stream, and increments the index'''
        byte = struct.unpack('<B', self.data[self.index])
        self.next_byte()
        return byte[0]
    def pop_bit( self ):
        '''Returns a bit from the stream, incrementing the counters'''
        mask = pow(2, 7-self.bit)
        bit = struct.unpack('<B', self.data[self.index])[0] & mask
        self.__increment()
        ret = False
        if bit:
            ret = True
        return ret
    def push_bit( self, bit ):
        '''Pushes the value of the bit passed to the stream'''
        if bit:
            val = ((0xff - (pow(2, 8 - self.bit) - 1)) & \
                struct.unpack('<B', self.data[self.index])[0]) + pow(2, 7 - self.bit)
            self.data = self.data[0:self.index] + struct.pack('<B', val)
        self.__increment()        
    def __increment( self ):
        '''Increments the bit counter, or the index if at last bit of byte'''
        if self.bit < 7:
            self.bit += 1
        else:
            self.bit = 0
            self.index += 1
            if self.index == len(self.data):
                self.data += '\x00'
        return
    def next_byte( self ):
        '''Resets the bit counter to 0, and increments the index'''
        self.index += 1
        self.bit = 0
        if self.index == len(self.data):
            self.data += '\x00'
    def pop_bits_to_int( self, length ):
        '''Pops length bits from the stream, and returns a little-
            endian decoded integer value'''
        bitcount = 0
        value = 0
        while bitcount < length:
            if self.pop_bit():
                value += pow(2, length-bitcount-1)
            bitcount += 1
        return value
    def push_int( self, value, length=0 ):
        '''Pushes the minimum number of bits possible to represent
            value to the stream, or a specific length if length set'''
        bitlist = []
        while value:
            bitlist.insert(0, value % 2)
            value /= 2
        if length >= len(bitlist):
            num = length - len(bitlist)
            for i in range(num):
                self.push_bit(0)
        else:
            raise ValueError
        for bit in bitlist:
            self.push_bit(bit)
    def push_count( self, count ):
        '''Pushes count # 1's to the bitstream, followed by a 0'''
        for count in range(count):
            self.push_bit(1)
        self.push_bit(0)
