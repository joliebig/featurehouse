VERSION = '0.9'
import Plugin
from Parser import TAGS_NONE, TAGS_PANGO, TAGS_HTML
import re
ERROR =''
METHODLIST = [ 'Plus', 'AntiPlus' ]
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
colorCodesHexGradient = re.compile\
    ('\[[cC]=#([0-9A-Fa-f]{6})\](.*?)\[/[cC]=#([0-9A-Fa-f]{6}|#{6})\]')
backColorCodesHexGradient = re.compile\
    ('\[[aA]=#([0-9A-Fa-f]{6})\](.*?)\[/[aA]=#([0-9A-Fa-f]{6}|#{6})\]')
tagInGradient = r'(<span[^>]*>|</span>|'\
    '\[[aA]=[0-9]{1,2}\]|\[[aA]=#[0-9A-Fa-f]{6}\]|\[/[aA]=#{7}\]|'\
    '\[/[aA]=[0-9]{1,2}\]|\[/[aA]=#[0-9A-Fa-f]{6}\])'
specialInGradient = r'(&[a-zA-Z]+\d{0,3};|\%.)'
allInGradient = r'(<span[^>]*>|</span>|'\
    '\[[aA]=[0-9]{1,2}\]|\[[aA]=#[0-9A-Fa-f]{6}\]|\[/[aA]=#{7}\]|'\
    '\[/[aA]=[0-9]{1,2}\]|\[/[aA]=#[0-9A-Fa-f]{6}\]|&[a-zA-Z]+\d{0,3};|\%.)'
openName = r'(?<=\[[cC]=|\[[aA]=)[0-9a-zA-Z]{3,6}(?=\])'
closeName = r'(?<=\[/[cC]=|\[/[aA]=)[0-9a-zA-Z]{3,6}(?=\])'
openCode = r'(?<=\[[cC]=|\[[aA]=)[0-9]{1,2}(?=\])'
closeCode = r'(?<=\[/[cC]=|\[/[aA]=)[0-9]{1,2}(?=\])'
colorIrcCode = re.compile("\xb7\$([0-9]{1,2})?,?([0-9]{1,2})?")
colorIrcCodeFG = re.compile("(?<=\xb7\$)[0-9]{1,2}")
colorIrcCodeBG = re.compile("(?<=\xb7\$,)[0-9]{1,2}")
colorIrcCodeFGBG = re.compile("(?<=\xb7\$#[0-9a-fA-F]{6},)[0-9]{1,2}")
colorIrcHex = re.compile("\xb7\$(#[0-9a-fA-F]{6})?,?(#[0-9a-fA-F]{6})?")
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
    formatIrc[bb] = re.compile("\xb7(\%s)(.*?)(\xb7\%s|$)" % doubleIrc)
del bb, irc, doubleCase, doubleIrc
span = re.compile('(<span.*?>)|(</span>)')
colorTags = re.compile('\[[cC]=#[0-9A-Fa-f]{6}\]|\[[cC]=[0-9]{1,2}\]|'\
    '\[/[cC]\]|\[/[cC]=[0-9]{1,2}\]|\[/[cC]=#[0-9A-Fa-f]{6}\]|'\
    '\[[cC]=[0-9A-Za-z]{1,6}\]|\[/[cC]=[0-9A-Za-z]{1,6}\]')
backColorTags = re.compile('\[[aA]=#[0-9A-Fa-f]{6}\]|\[[aA]=[0-9]{1,2}\]|'\
    '\[/[aA]\]|\[/[aA]=[0-9]{1,2}\]|\[/[aA]=#[0-9A-Fa-f]{6}\]|'\
    '\[[aA]=[0-9A-Za-z]{1,6}\]|\[/[aA]=[0-9A-Za-z]{1,6}\]')
colorIrcTags = re.compile('\xb7\$[0-9]{1,2},[0-9]{1,2}|'\
    '\xb7\$[0-9]{1,2}|\xb7\$,[0-9]{1,2}|\xb7\$#[a-fA-F0-9]{6},#[a-fA-F0-9]{6}|'\
    '\xb7\$#[a-fA-F0-9]{6}|\xb7\$,#[a-fA-F0-9]{6}')
BbTags = re.compile\
    ('\[[bB]\]|\[[iI]\]|\[[uU]\]|\[[ss]\]|'\
    '\[/[bB]\]|\[/[iI]\]|\[/[uU]\]|\[/[ss]\]')
IrcTags = re.compile("\xb7\#|\xb7&[a-zA-Z]+\d{0,3};|\xb7\@|\xb70")
removeList = (colorTags,backColorTags,colorIrcTags,BbTags,IrcTags)
getTagDict = {
    'background': ('background="#%s" ','background-color: #%s; '),
    'foreground': ('foreground="#%s" ','color: #%s; '),
    'b': ('weight="bold"', 'font-weight: bold'), 
    'u': ('underline="single"', 'text-decoration:underline'),
    'i': ('style="italic"', 'font-style: italic'),
    's': ('strikethrough="true"', 'text-decoration: line-through'),
}
customStatus = re.compile("^(.*)\xa0{([^}]*)}$")
def getCustomStatus(nick):
    '''parse custom status, return tuple (nick, status)'''
    result = unicode(customStatus.search(nick),'utf8')
    if result:
        return result.groups()
    else:
        return (nick, '')
class MsnPlusMarkup:
    def __init__( self ):
        self.isHtml = False
    def removeMarkup( self, text ):
        '''remove the [b][/b] etc markup for pango and html markup'''
	text = unicode(text,'utf8')
	all = removeList 
        for i in all:
            text = re.sub( i, '', text)
        all =tuple(formatBb.values() + formatIrc.values())
        for i in all:
            text = re.sub( i, lambda x:x.groups()[1], text)
        text = re.sub( colorIrcCode, '', text )
        return text
    def replaceMarkup( self, text ):
        '''replace the [b][/b] etc markup for pango and html markup'''
	text = unicode(text,'utf8')
        text = text.replace\
	    ('\xc2\xb7&amp;','\xc2\xb7&').replace('\xc2\xb7&quot;','\xc2\xb7"')\
	    .replace('\xc2\xb7&apos;','\xc2\xb7\'')
	self.more = 0
        self.backgroundColor = self.frontColor = ''
        self.openSpan = None
        def q(dictionary, tag, text): #quick handler
            def handler(x):
                return self.getTag({tag: ''}, x.groups()[1])
            return re.sub(dictionary[tag], handler, text)
	text = re.sub('\xb70','',text)
        for tag in 'bius':
            text = q(formatBb, tag, text)
            text = q(formatIrc, tag, text)
	text = re.sub( openName, self.nameToHex, text)
	text = re.sub( closeName, self.nameToHex, text)
	text = re.sub( openCode, self.codeToHex, text)
	text = re.sub( closeCode, self.codeToHex, text)
	text = re.sub( colorIrcCodeFG, self.codeToHex, text)
	text = re.sub( colorIrcCodeBG, self.codeToHex, text)
	text = re.sub( colorIrcCodeFGBG, self.codeToHex, text)
	text = re.sub( '\[/[cC]\]', '[/c=#######]',text)
	text = re.sub( '\[/[aA]\]', '[/a=#######]',text)
	
        text = re.sub( colorCodesHexGradient, self.hexToTagGrad, text )
        text = re.sub( backColorCodesHexGradient, self.bHexToTagGrad, text )
	text = re.sub( '\[[cC]=#[0-9a-fA-F]{6}\]|'\
		'\[/[cC]=#[0-9a-fA-F]{6}\]|\[/c=#######\]','',text)
	text = re.sub( '\[[aA]=#[0-9a-fA-F]{6}\]|'\
		'\[/[aA]=#[0-9a-fA-F]{6}\]|\[/a=#######\]','',text)
        text = re.sub( colorIrcHex, self.ircHexToTag, text ) 
        if self.openSpan != None:
            text += '</span>'
	text='&#173;'+text
        return text.replace('\xc2\xb7&','\xc2\xb7&amp;').replace\
	    ('\xc2\xb7"','\xc2\xb7&quot;')\
	    .replace('\xc2\xb7\'','\xc2\xb7&apos;')
    def codeToHex( self, data ):
        code=data.group()
	if int(code) <= len(colorCodes):
	    hex = '#'+colorCodes[int(code)].lower()
	    return hex
	else:
	    return code
    def nameToHex( self, data ):
        code=data.group()
        if code.lower() == 'white':
	    return '#ffffff'
        elif code.lower() == 'black':
	    return '#000000'
        elif code.lower() == 'marine':
            return '#00007F'
        elif code.lower() == 'green':
            return '#009300'
        elif code.lower() == 'red':
            return '#FF0000'
        elif code.lower() == 'brown':
            return '#7F0000'
        elif code.lower() == 'purple':
            return '#9C009C'
        elif code.lower() == 'orange' :
            return '#FC7F00'
        elif code.lower() == 'yellow':
            return '#FFFF00'
        elif code.lower() == 'lime':
            return '#00FC00'
        elif code.lower() == 'teal' :
            return '#009393'
        elif code.lower() == 'aqua' :
            return '#00FFFF'
        elif code.lower() == 'blue' :
            return '#0000FC'
        elif code.lower() == 'pink' :
            return '#FF00FF'
        elif code.lower() == 'gray' :
            return '#7F7F7F'
        elif code.lower() == 'silver':
            return '#D2D2D2'
        elif code.lower() == 'mohr':
            return '#ff00de'
        elif code.lower() == 'c10ud' :
            return '#313373'
        else:
            return code
    def ircHexToTag( self, data ):
        text = ''
        color1, color2 = data.groups()
	try:
	    color1 = color1[1:]
	except:
	    pass
	try:
	    color2 = color2[1:]
	except:
	    pass
        if self.frontColor != '' and color1 == None:
            color1 = self.frontColor
        if self.backgroundColor != '' and  color2 == None:
            color2 = self.backgroundColor
        self.frontColor = color1
        self.backgroundColor = color2
        try:
            if color1 != None and color2 != None:
                fghex = color1.lower()
                bghex = color2.lower()
                text = self.getTag({'foreground': fghex, 'background': bghex},
                    None)
            elif color1 != None:
                fghex = color1.lower()
                text = self.getTag({'foreground': fghex}, None)
            elif color2 != None:
                bghex = color2.lower()
                text = self.getTag({'background': bghex}, None)
        except IndexError:
            pass
        if self.openSpan != None:
            text = '</span>' + text
        if text:
            self.openSpan = True
            return text
    def dec2hex(self, n):
	"""return the hexadecimal string representation of integer n"""
	if n<16:
	    return '0' + "%X" % n
	else:
	    return "%X" % n
    def hex2dec(self, s):
	"""return the integer value of a hexadecimal string s"""
	return int('0x'+s, 16)
    def gradient(self, col1,col2,length):
	R1hex=col1[:2]
	G1hex=col1[2:4]
	B1hex=col1[4:6]
	R2hex=col2[:2]
	G2hex=col2[2:4]
	B2hex=col2[4:6]
	R1dec=self.hex2dec(R1hex.upper())
	G1dec=self.hex2dec(G1hex.upper())
	B1dec=self.hex2dec(B1hex.upper())
	R2dec=self.hex2dec(R2hex.upper())
	G2dec=self.hex2dec(G2hex.upper())
	B2dec=self.hex2dec(B2hex.upper())
	stepR =((float(R2dec)-float(R1dec))/(float(length)-float(1)))
	stepG =((float(G2dec)-float(G1dec))/(float(length)-float(1)))
	stepB =((float(B2dec)-float(B1dec))/(float(length)-float(1)))
	R = [0] * length 
	R[0]=self.dec2hex(R1dec)
	R[length-1]=self.dec2hex(R2dec)
	G = [0] * length 
	G[0]=self.dec2hex(G1dec)
	G[length-1]=self.dec2hex(G2dec)
	B = [0] * length 
	B[0]=self.dec2hex(B1dec)
	B[length-1]=self.dec2hex(B2dec)
	for i in range(1, length-1):
	    R[i] = self.dec2hex(int(R1dec+stepR * i))
	for i in range(1, length-1):
	    G[i] = self.dec2hex(int(G1dec+stepG * i))
	for i in range(1, length-1):
	    B[i] = self.dec2hex(int(B1dec+stepB * i))
	colors = [0] * length
	for i in range(0,length):
	   colors[i] = R[i] + G[i] + B[i]
	return colors
    def longTagged( self, colors, text):
	textSplit=filter(None,re.split(allInGradient,text))
	k=0
	for j in range(0,len(textSplit)):
	    if len(re.findall(tagInGradient,textSplit[j])) == 0:
		tagged=''
		if len(re.findall(specialInGradient,textSplit[j])) == 0:
		    for i in range(0,len(textSplit[j])):
			tagged=tagged + self.getTag\
				({'foreground': colors[k]},textSplit[j][i])
			k=k+1
		else:
		    tagged=tagged + self.getTag\
			    ({'foreground': colors[k]},textSplit[j])
		    k=k+1
		textSplit[j]=tagged
	textn=''
	for j in range(0,len(textSplit)):
	    textn=textn+textSplit[j]
	return textn
    def blongTagged( self, colors, text):
	textSplit=filter(None,re.split(allInGradient,text))
	k=0
	for j in range(0,len(textSplit)):
	    if len(re.findall(tagInGradient,textSplit[j])) == 0:
		tagged=''
		if len(re.findall(specialInGradient,textSplit[j])) == 0:
		    for i in range(0,len(textSplit[j])):
			tagged=tagged + self.getTag\
				({'background': colors[k]},textSplit[j][i])
			k=k+1
		else:
		    tagged=tagged + self.getTag\
			    ({'background': colors[k]},textSplit[j])
		    k=k+1
		textSplit[j]=tagged
	textn=''
	for j in range(0,len(textSplit)):
	    textn=textn+textSplit[j]
	return textn
    def hexToTagGrad( self, data ):
        color1 = data.group(1)
	text = data.group(2)
	text = re.sub(colorTags,'',text)
	text = re.sub(colorIrcTags,'',text)
	color2 = data.group(3)
	if color2 == '######' or color2 == color1:
	    return self.getTag({'foreground': color1}, text)
	else:
	    length = len(re.sub(specialInGradient,'a',\
		    re.sub(tagInGradient,'',text)))
	    if length>1:
		colors=self.gradient(color1,color2,length)
		tagged = self.longTagged( colors, text)
		return tagged
	    else:
		return text
    def bHexToTagGrad( self, data ):
        color1 = data.group(1)
	text = data.group(2)
	text = re.sub(backColorTags,'',text)
	text = re.sub(colorIrcTags,'',text)
	color2 = data.group(3)
	if color2 == '######' or color2 == color1:
	    return self.getTag({'background': color1}, text)
	else:
	    length = len(re.sub(specialInGradient,'a',\
		    re.sub(tagInGradient,'',text)))
	    if length>1:
		colors=self.gradient(color1,color2,length)
		tagged = self.blongTagged( colors, text)
		return tagged
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
    description = _( 'Messenser Plus like plugin for emesene' )
    authors = { 'Roger Duran' : 'RogerDuran at gmail dot com',
         'Mohr Tutchy' : 'mohr dot tutchy at gmail dot com'}
    website = 'http://www.rogerpc.com.ar'
    displayName = _( 'PlusPlus' )
    name = 'Plus'
    def __init__( self, controller, msn ):
        '''Contructor'''
        Plugin.Plugin.__init__( self, controller, msn )
        self.description = _( 'Messenser Plus like plugin for emesene' )
        self.authors = { 'Roger Duran' : 'RogerDuran at gmail dot com',
			 'Mohr Tutchy' : 'mohr dot tutchy at gmail dot com'}
        self.website = 'http://www.rogerpc.com.ar'
        self.displayName = _( 'PlusPlus' )
        self.name = 'Plus'
        self.config = controller.config
        self.config.readPluginConfig(self.name)
        self.controller = controller
        self.plusMarkup = MsnPlusMarkup()
        self.parserFilterId = None
        self.method = self.config.getPluginValue(self.name, 'method', '')
        if self.method == '':
            self.method = METHODLIST[0]
    def configure(self):
        '''Configuration Dialog'''
        l=[]
        l.append(Plugin.Option('method', list, _('Formatting:'), '', 
                 self.config.getPluginValue\
			 ( self.name, 'method', '' ), METHODLIST))
        response = Plugin.ConfigWindow(_('PlusPlus Configuration'), l).run()
        if response != None:
            self.method = response['method'].value
            self.config.setPluginValue(self.name,'method', self.method)
	    self.controller.refreshUserList()
        return True
    def start( self ):
        '''start the plugin'''
        self.parserFilterId = self.controller.unifiedParser.connect\
		( 'filter', self.plusParse )
        self.controller.emit( 'preferences-changed' )
        self.enabled = True
    def stop( self ):    
        '''stop the plugin'''
        self.controller.unifiedParser.disconnect( self.parserFilterId )
        self.controller.emit( 'preferences-changed' )
        self.enabled = False
    def plusParse(self, obj, parser, filterdata):
        format, objects = filterdata.serialize(filterdata.list)
	
	if self.method == 'Plus':
            if parser and parser.tags != TAGS_NONE:
                self.plusMarkup.isHtml = False
                if parser.tags == TAGS_HTML:
                    self.plusMarkup.isHtml = True
                format = self.plusMarkup.replaceMarkup(format)
            else:
                format = self.plusMarkup.removeMarkup(format)
            filterdata.list = filterdata.deserialize(format, objects)
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
