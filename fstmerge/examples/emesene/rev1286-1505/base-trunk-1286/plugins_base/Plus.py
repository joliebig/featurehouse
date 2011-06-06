VERSION = '0.1'
import Plugin
from Parser import TAGS_NONE, TAGS_PANGO, TAGS_HTML
import re
ERROR =''
colorCodes = (
'ffffff','000000','00007F','009300','FF0000','7F0000','9C009C','FC7F00',
'FFFF00','00FC00','009393','00FFFF','0000FC','FF00FF','7F7F7F','D2D2D2',
'E7E6E4','CFCDD0','FFDEA4','FFAEB9','FFA8FF','B4B4FC','BAFBE5','C1FFA3',
'FAFDA2','B6B4B7','A2A0A1','F9C152','FF6D66','FF62FF','6C6CFF','68FFC3',
'8EFF67','F9FF57','858482','6E6D7B','FFA01E','F92611','FF20FF','202BFF',
'1EFFA5','60F913','FFF813','5E6464','4B494C','D98812','EB0505','DE00DE',
'0000D3','03CC88','59D80D','D4C804','333335','18171C','944E00','9B0008',
'980299','01038C','01885F','389600','9A9E15','473400','4D0000','5F0162',
'000047','06502F','1C5300','544D05')
colorCodesHex = re.compile('\[[cC]=#([0-9A-Fa-f]{6})\](.*?)\[/[cC]\]')
colorCodesCode = re.compile('\[[cC]=([0-9]{1,2})\](.*?)\[/[cC]\]')
backColorCodesHex = re.compile('\[[aA]=#([0-9A-Fa-f]{6})\](.*?)\[/[aA]\]')
backColorCodesCode = re.compile('\[[aA]=([0-9]{1,2})\](.*?)\[/[aA]\]')
colorIrcCode = re.compile("\xc2\xb7\$([0-9]{1,2})?,?([0-9]{1,2})?")
formatBb = {}
formatIrc = {}
tmpBb = "bius"
tmpIrc = "#&@'"
for i in range(4):
    bb = tmpBb[i]
    irc = tmpIrc[i]
    doubleCase = (bb+bb.upper(), bb+bb.upper())
    doubleIrc = (irc,irc)
    formatBb[bb] = re.compile('(\[[%s]\](.*?)\[/[%s]\])' % doubleCase)
    formatIrc[bb] = re.compile("\xc2\xb7(\%s)(.*?)(\xc2\xb7\%s|$)" % doubleIrc)
del bb, irc, doubleCase, doubleIrc
span = re.compile('(<span.*?>)|(</span>)')
removeList = (colorCodesHex, colorCodesCode, backColorCodesHex, backColorCodesCode)
getTagDict = {
    'background': ('background="#%s" ','background-color: #%s; '),
    'foreground': ('foreground="#%s" ','color: #%s; '),
    'b': ('weight="bold"', 'font-weight: bold'), 
    'u': ('underline="single"', 'text-decoration:underline'),
    'i': ('style="italic"', 'font-style: italic'),
    's': ('strikethrough="true"', 'text-decoration: line-through'),
}
customStatus = re.compile("^(.*)\xc2\xa0{([^}]*)}$")
def getCustomStatus(nick):
    '''parse custom status, return tuple (nick, status)'''
    result = customStatus.search(nick)
    if result:
        return result.groups()
    else:
        return (nick, '')
class MsnPlusMarkup:
    def __init__( self ):
        self.isHtml = False
    def removeMarkup( self, text ):
        '''remove the [b][/b] etc markup for pango and html markup'''
        all = removeList + tuple(formatBb.values() + formatIrc.values())
        for i in all:
            text = re.sub( i, lambda x:x.groups()[1], text)
        text = re.sub( colorIrcCode, '', text )
        return text
    def replaceMarkup( self, text ):
        '''replace the [b][/b] etc markup for pango and html markup'''
        text = text.replace('\xc2\xb7&amp;','\xc2\xb7&').replace('\xc2\xb7&quot;','\xc2\xb7"')\
        .replace('\xc2\xb7&apos;','\xc2\xb7\'') #@ROGER: i hate this, i hate it too..
        self.more = 0
        self.backgroundColor = self.frontColor = ''
        self.openSpan = None
        def q(dictionary, tag, text): #quick handler
            def handler(x):
                return self.getTag({tag: ''}, x.groups()[1])
            return re.sub(dictionary[tag], handler, text)
        for tag in 'bius':
            text = q(formatBb, tag, text)
            text = q(formatIrc, tag, text)
        text = re.sub( colorCodesHex, self.hexToTag, text )
        text = re.sub( colorCodesCode, self.codeToTag, text )
        text = re.sub( backColorCodesHex, self.bHexToTag, text )
        text = re.sub( backColorCodesCode, self.bCodeToTag, text )
        text = re.sub( colorIrcCode, self.ircCodeToTag, text ) 
        if self.openSpan != None:
            text += '</span>'
        return text.replace('\xc2\xb7&','\xc2\xb7&amp;').replace('\xc2\xb7"','\xc2\xb7&quot;')\
        .replace('\xc2\xb7\'','\xc2\xb7&apos;')
    def ircCodeToTag( self, data ):
        text = ''
        color1, color2 = data.groups()
        if self.frontColor != '' and color1 == None:
            color1 = self.frontColor
        if self.backgroundColor != '' and  color2 == None:
            color2 = self.backgroundColor
        self.frontColor = color1
        self.backgroundColor = color2
        try:
            if color1 != None and color2 != None and \
               int(color1) <= len(colorCodes) and \
               int(color1) <= len(colorCodes):
                fghex = colorCodes[int(color1)].lower()
                bghex = colorCodes[int(color2)].lower()
                text = self.getTag({'foreground': fghex, 'background': bghex},
                    None)
            elif color1 != None and int(color1) <= len(colorCodes):
                fghex = colorCodes[int(color1)].lower()
                text = self.getTag({'foreground': fghex}, None)
            elif color2 != None and int(color2) <= len(colorCodes):
                bghex = colorCodes[int(color2)].lower()
                text = self.getTag({'background': bghex}, None)
        except IndexError:
            pass
        if self.openSpan != None:
            text = '</span>' + text
        if text:
            self.openSpan = True
            return text
    def hexToTag( self, data ):
        color, text = data.groups()
        return self.getTag({'foreground': color.lower()}, text)
    def codeToTag( self, data ):
        code, text = data.groups()
        if int(code) <= len(colorCodes):
            hex = colorCodes[int(code)].lower()
            return self.getTag({'foreground': hex}, text)
        else:
            return text
    def bHexToTag( self, data ):
        color, text = data.groups()
        return self.getTag({'background': color.lower()}, text) 
    def bCodeToTag( self, data ):
        code, text = data.groups()
        if int(code) <= len(colorCodes):
            hex = colorCodes[int(code)].lower()
            return self.getTag({'background': hex}, text)
        else:
            return text     
    def RGBToHTMLColor( self, rgb_tuple ):
        '''convert an (R, G, B) tuple to #RRGGBB'''
        return '#%02x%02x%02x' % rgb_tuple
    def HTMLColorToRGB( self, colorstring ):
        '''convert #RRGGBB to an (R, G, B) tuple'''
        colorstring = colorstring.strip()
        if colorstring.startswith('#'): 
            colorstring = colorstring[1:]
        r, g, b = colorstring[:2], colorstring[2:4], colorstring[4:]
        r, g, b = [int(n, 16) for n in (r, g, b)]
        return (r, g, b)
    def getTag( self, attrdict, text ):
        attrs = []
        for key in attrdict.keys():
            if key in getTagDict:
                attr = getTagDict[key][int(self.isHtml)]
            attr = attr.replace("%s", attrdict[key])
            attrs.append(attr)
        if self.isHtml:
            tagattr = 'style="'
            for attr in attrs:
                tagattr += attr
            tagattr += '"'
        else:
            tagattr = ''
            for attr in attrs:
                tagattr += attr
        if text == None:
            return '<span ' + tagattr + '>'
        else:
            return '<span ' + tagattr + '>' + text + '</span>'
class MainClass( Plugin.Plugin ):
    '''Main plugin class'''
    def __init__( self, controller, msn ):
        '''Contructor'''
        Plugin.Plugin.__init__( self, controller, msn )
        self.description = _( 'Messenser Plus like plugin for emesene' )
        self.authors = { 'Roger Duran' : 'RogerDuran at gmail dot com' }
        self.website = 'http://www.rogerpc.com.ar'
        self.displayName = _( 'Plus' )
        self.name = 'Plus'
        self.config = controller.config
        self.config.readPluginConfig(self.name)
        self.controller = controller
        self.plusMarkup = MsnPlusMarkup()
        self.parserFilterId = None
    def start( self ):
        '''start the plugin'''
        self.parserFilterId = self.controller.unifiedParser.connect( 'filter', self.plusParse )
        self.controller.emit( 'preferences-changed' )
        self.enabled = True
    def stop( self ):    
        '''stop the plugin'''
        self.controller.unifiedParser.disconnect( self.parserFilterId )
        self.controller.emit( 'preferences-changed' )
        self.enabled = False
    def plusParse(self, obj, parser, filterdata):
        format, objects = filterdata.serialize(filterdata.list)
        if parser and parser.tags != TAGS_NONE:
            self.plusMarkup.isHtml = False
            if parser.tags == TAGS_HTML:
                self.plusMarkup.isHtml = True
            format = self.plusMarkup.replaceMarkup(format)
        else:
            format = self.plusMarkup.removeMarkup(format)
        filterdata.list = filterdata.deserialize(format, objects)
    def action( self ):
        pass
    def check( self ):
        '''
        check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )
        '''
        if ERROR != '':
            return ( False, ERROR )
        return ( True, 'Ok' )
