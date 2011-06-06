'''pygif: gif implementation in python
I (dx) have designed this to encode the handwritten messages for emesene.
While learning the format, I had to write that simple decoder -- so, it isn't
a complete gif89a decoder. If anyone needs to use it, contact me for help:
dx dxzone com ar.
Special credit for Jan de Mooij that fixed the giflzw encoder.'''
import struct
import math
KNOWN_FORMATS = ('GIF87a', 'GIF89a')
class Gif(object):
    '''Base class to encoder and decoder'''
    FMT_HEADER = '<6sHHBBB'
    FMT_IMGDESC = '<HHHHB'
    IMAGE_SEPARATOR = 0x2C
    EXTENSION_INTRODUCER = 0x21
    GIF_TRAILER = 0x3b
    LABEL_GRAPHIC_CONTROL = 0xF9
    LABEL_COMMENT = 0xFE
    LABEL_PLAINTEXT = 0x01
    FMT_EXT_GRAPHIC_CONTROL = '<BBHB' #89a
    def __init__( self, data, debug ):
        self.data = data
        self.pointer = 0
        self.header = 'GIF87a'
        self.ls_width = 0
        self.ls_height = 0
        self.flags = 0
        self.color_resolution = 0
        self.sort_flag = 0
        self.color_table_flag = 0
        self.global_color_table_size = 0
        self.background_color = 0
        self.aspect_ratio = 0
        self.pallete = [(x, x, x) for x in range(0, 256)]
        self.images = []
        self.debug_enabled = debug
    def pop( self, length=1 ):
        '''gets the next $len chars from the data stack,
        and increment the pointer'''
        start = self.pointer
        end = self.pointer + length
        self.pointer += length
        return self.data[start:end]
    def pops( self, format ):
        '''pop struct: get size, pop(), unpack()'''
        size = struct.calcsize(format) 
        return struct.unpack( format, self.pop(size) )
    def push( self, newdata ):
        '''adds $newdata to the data stack'''
        if self.debug_enabled:
            print repr(newdata), len(newdata)
        self.data += newdata
    def pushs( self, format, *vars ):
        '''push struct: adds a packed struct to the data stack'''
        self.push(struct.pack(format, *vars))
    def pop_bits( self, length ):
        '''return a list of bytes represented as bit lists'''
        bytes = self.pops( '<' + str(length) + 'B' )
        plain = []
        for byte in bytes:
            for bit in get_bits(byte):
                plain.append(bit)
        return plain
    def print_info( self ):
        '''prints out some useful info (..debug?)'''
        print "Version: %s" % self.header
        print "Logical screen width: %d" % self.ls_width
        print "Logical screen height: %d" % self.ls_height
        print "Flags: %s" % repr(self.flags)
        print " "*6,"Color resolution: %d" % self.color_resolution
        print " "*6,"Sort flag: %s" % str(self.sort_flag)
        print " "*6,"Global color table flag: %s" % str(self.color_table_flag)
        print " "*22,"...size: %d (%d bytes)" % \
            (self.global_color_table_size, self.global_color_table_size * 3)
        print "Background color: %d" % self.background_color
        print "Aspect ratio info: %d" % self.aspect_ratio
    def new_image( self, header=None ):
        '''adds a new image descriptor'''
        image = ImageDescriptor(self, header)
        self.images.append(image)
        return image
class ImageDescriptor(object):
    '''A class that represents a single image'''
    def __init__( self, parent, header=None ):
        self.codesize = 0
        self.lzwcode = ''
        self.pixels = []
        self.left = self.top = 0
        self.width = parent.ls_width
        self.height = parent.ls_height
        self.flags = [False for x in range(8)]
        self.local_color_table_flag = False
        self.interlace_flag = False
        self.sort_flag = False
        self.local_color_table_size = 0
        self.pallete = []
        if header:
            self.setup_header(header)
    def setup_header( self, header ):
        '''takes a header tuple and fills the attributes'''
        self.left = header[0]
        self.top = header[1]
        self.width = header[2]
        self.height = header[3]
        self.flags = get_bits( header[4] )
        self.local_color_table_flag = self.flags[7]
        assert self.local_color_table_flag == False, \
            "Local color tables not implemented" # TODO
        self.interlace_flag = self.flags[6]
        self.sort_flag = self.flags[5]
        self.local_color_table_size =  2 ** (pack_bits(self.flags[:2]) + 1)
    def get_header(self):
        '''builds a header dynamically'''
        flags = [False for x in range(8)]
        flags[7] = self.local_color_table_flag
        flags[6] = self.interlace_flag
        flags[5] = self.sort_flag
        flags[2], flags[1], flags[0] = get_bits(len(self.pallete), bits=3)
        return (self.left, self.top, self.width, self.height, pack_bits(flags))
    header = property(fget=get_header)
class GifDecoder( Gif ):
    '''decodes a gif file into.. something.. else..'''
    def __init__( self, data, debug=False ):
        Gif.__init__( self, data, debug )
        self.fill()
    def fill( self ):
        '''reads the data and fills each field of the file'''
        self.pointer = 0
        data = self.pops( Gif.FMT_HEADER )
        self.header = data[0]
        self.ls_width = data[1]
        self.ls_height = data[2]
        self.background_color = data[4]
        self.aspect_ratio = data[5]
        self.flags = get_bits( data[3] )
        self.color_table_flag = self.flags[7]
        self.sort_flag = self.flags[3]
        self.color_resolution = pack_bits(self.flags[4:7]) # 7 not included
        self.global_color_table_size = 2 ** (pack_bits(self.flags[:3]) + 1)
        if self.color_table_flag:
            size = (self.global_color_table_size) * 3
            self.pallete = self.get_color_table(size)
        else:
            self.pallete = [(x, x, x) for x in range(256)]
        while True:
            try:
                nextbyte = self.pops('<B')[0]
            except:
                nextbyte = 0x3b # force end
            if nextbyte == Gif.IMAGE_SEPARATOR:
                descriptor = self.pops(Gif.FMT_IMGDESC)
                image = self.new_image(descriptor)
                image.codesize = self.pops('<B')[0]
                image.lzwcode = ''
                while True:
                    try:
                        blocksize = self.pops('<B')[0]
                    except:
                        break
                    if blocksize == 0:
                        break   # no more image data
                    lzwdata = self.pop(blocksize)
                    image.lzwcode += lzwdata
                if self.debug_enabled:
                    print 'LZW length:', len(image.lzwcode)
                image.pixels = self.lzw_decode(image.lzwcode, image.codesize, \
                    self.global_color_table_size)
            elif nextbyte == Gif.EXTENSION_INTRODUCER:
                pass
            elif nextbyte == Gif.GIF_TRAILER:
                return
            else:
                pass
    def get_color_table( self, size ):
        '''Returns a color table in the format [(r,g,b),(r,g,b), ...]'''
        raw_color_table = self.pops("<%dB" % size)
        pos = 0
        pallete = []
        while pos + 3 < (size+1):
            red = raw_color_table[pos]
            green = raw_color_table[pos+1]
            blue = raw_color_table[pos+2]
            pallete.append((red, green, blue))
            pos += 3
        return pallete
    def lzw_decode(self, input, initial_codesize, color_table_size):
        '''Decodes a lzw stream from input
        Returns list of ints (pixel values)'''
        string_table = {}
        output = []
        old = ''
        index = 0
        codesize = initial_codesize + 1
        clearcode, end_of_info = color_table_size, color_table_size + 1
        bits = string_to_bits(input)
        def pop(size):
            '''Pops <size> bits from <bits>'''
            out = []
            for i in range(size):
                out.append(bits.pop(0))
            return out
        def clear():
            '''Called on clear code'''
            string_table.clear()
            for index in range(color_table_size):
                string_table[index] = chr(index)
            index = end_of_info + 1
            return index
        index = clear()
        bits = bits[codesize:]
        code = bits_to_int(pop(codesize))
        output = [ord(string_table[code])]
        old = string_table[code]
        while len(bits) > 0:
            code = bits_to_int(pop(codesize))
            if code == clearcode:
                index = clear()
                codesize = initial_codesize + 1
                code = bits_to_int(pop(codesize))
                output.append(ord(string_table[code]))
                old = string_table[code]
                continue
            elif code == end_of_info:
                break
            if code in string_table:
                c = string_table[code]
                string_table[index] = old + c[0]
            else:
                c = old + old[0]
                string_table[code] = c
            index += 1
            old = c
            output += [ord(x) for x in c]
            if index == 2 ** codesize:
                codesize += 1
                if codesize == 13:
                    codesize = 12
                    print 'decoding error, missed a clearcode?'
                    print 'index:', index
        if self.debug_enabled:
            print 'Output stream len: %d' % len(output)
        return output
class GifEncoder(Gif):
    '''Encodes a *something* into a gif'''
    def __init__( self, basedecoder=None, debug=False ):
        Gif.__init__( self, '', debug )
        if basedecoder:
            self.clone(basedecoder)
    def clone( self, decoder ):
        '''moves decoder data into this class'''
        self.header = decoder.header
        self.ls_width = decoder.ls_width
        self.ls_height = decoder.ls_height
        self.flags = decoder.flags
        self.background_color = decoder.background_color
        self.aspect_ratio = decoder.aspect_ratio
        self.color_table_flag = decoder.color_table_flag
        self.pallete = decoder.pallete
        self.color_resolution = decoder.color_resolution
        self.global_color_table_size = decoder.global_color_table_size
        self.images = decoder.images
    def write( self, filename ):
        '''rebuilds the gif stream and writes it to filename'''
        self.data = ''
        if self.debug_enabled:
            print "header, screen descriptor"
        self.pushs( Gif.FMT_HEADER, self.header, self.ls_width, \
            self.ls_height, pack_bits(self.flags), \
            self.background_color, self.aspect_ratio )
        if self.debug_enabled:
            print "global color table"
        if self.color_table_flag:
            for red, green, blue in self.pallete:
                self.pushs("<BBB", red, green, blue)
        if self.debug_enabled:
            print "images"
        self.images = self.images[:1]
        for image in self.images:
            if self.debug_enabled:
                print "image:", image
            self.pushs('<B', Gif.IMAGE_SEPARATOR)
            self.pushs(Gif.FMT_IMGDESC, *image.header)
            self.pushs('<B', image.codesize)
            lzwcode = image.lzwcode
            while len(lzwcode) > 0:
                chunk, lzwcode = lzwcode[:254], lzwcode[254:]
                self.pushs('<B', len(chunk))
                self.push(chunk)
        self.pushs('<B', 0)
        self.pushs('<B', Gif.GIF_TRAILER)
        rc = open(filename, 'wb')
        rc.write(self.data)
    def build_flags(self):
        '''builds the flag integer'''
        flaglist = [False for x in range(8)]
        flaglist[7] = bool(self.color_table_flag)
        flaglist[3] = bool(self.sort_flag)
        colorbits = get_bits(self.color_resolution, bits=3)
        if self.debug_enabled:
            print 'reso: %d' % self.color_resolution
        flaglist[4], flaglist[5], flaglist[6] = colorbits
        if self.color_table_flag:
            if self.debug_enabled:
                print 'pallete: %d' % len(self.pallete)
                print 'FIXME: is there an easier way?'
            val = int(math.ceil(math.log(len(self.pallete))/math.log(2)-1))
            gctsbits = get_bits(val, bits=3)
            flaglist[0], flaglist[1], flaglist[2] = gctsbits
        self.flags = flaglist
    def lzw_encode(self, pixels):
        '''Encodes a pixel stream (list of ints) in lzw code
        Returns tuple (initial code size, lzw string)'''
        if self.debug_enabled:
            print 'encoding %d pixels...' % len(pixels)
        string_table = {}
        output = []
        old = ''
        index = 0
        color_table_size = self.global_color_table_size
        clearcode = color_table_size
        end_of_info = color_table_size + 1
        initial_codesize = \
            int(math.ceil( math.log(color_table_size) / math.log(2) - 1 )) + 1
        codesize = initial_codesize + 1
        bits = []
        def clear():
            string_table.clear()
            for index in range(color_table_size):
                string_table[chr(index)] = index
            index = end_of_info + 1
            return index
        index = clear()
        add = get_bits(clearcode, bits=codesize)
        output += add
        c = chr(pixels.pop(0))
        counter = 0
        maxcount = len(pixels)
        while counter < maxcount:
            p = chr(pixels[counter])
            s = c + p
            counter += 1
            if s in string_table:
                c += p
            else:
                string_table[s] = index
                index += 1
                output += get_bits(string_table[c], bits=codesize)
                c = p
                if index == ((2 ** codesize)+1):
                    codesize += 1
                    if codesize == 13:
                        output += get_bits(clearcode, bits=12)
                        index = clear()
                        codesize = initial_codesize + 1
        if c != '':
            add = get_bits(string_table[c], bits=codesize)
            output += add
        add = get_bits(end_of_info, bits=codesize)
        output += add
        o = bits_to_string(output)
        return (initial_codesize, o)
def bits_to_int(bits):
    '''high level bit list packer'''
    i = 0
    c = 0
    bits.reverse()
    while c < len(bits):
        if bits[c]:
            i += 2 ** (len(bits) - c - 1)
        c += 1
    return i
def get_bits( flags, reverse=False, bits=8 ):
    '''return a list with $bits items, one for each enabled bit'''
    mybits = [ 1 << x for x in range(bits) ]
    ret = []
    for bit in mybits:
        ret.append(flags & bit != 0)
    if reverse:
        ret.reverse()
    return ret
def pack_bits( bits ):
    '''convert a bit (bool or int) tuple into a int'''
    packed = 0
    level = 0
    for bit in bits:
        if bit:
            packed += 2 ** level    
        level += 1
    return packed
def string_to_bits(string):
    '''high level string unpacker'''
    bits = []
    for byte in string:
        for bit in get_bits(ord(byte)):
            bits.append(bit)
    return bits
def bits_to_string(bits):
    '''high level bit list packer'''
    string = ''
    while len(bits)>0:
        code = pack_bits(bits[:8])
        bits = bits[8:]
        string += chr(code)
    return string
def readable(bool_list):
    '''Converts a list of booleans to a readable list of ints
    Useful for debug only'''
    return [int(x) for x in bool_list]
