import re
import os
import gtk.gdk
from shutil import copyfile
import paths
import emesenelib.common
from AvatarHistory import getLastCachedAvatar
ALTERNATIVE_SMILEYS = {\
    '<:o)':['<:O)'],
    '(a)' : ['(A)'],\
    ':@' : [':-@'],\
    '(b)' : ['(B)'] ,\
    '(z)' : ['(Z)'] ,\
    '(u)' : ['(U)'] ,\
    '(p)' : ['(P)'] ,\
    '(o)' : ['(O)'] ,\
    '(c)' : ['(C)'] ,\
    ':s' : [':S', ':-S'] ,\
    ':|' : [':-|'] ,\
    '(d)' : ['(D)'] ,\
    '(e)' : ['(E)'] ,\
    ':$' : [':-$'] ,\
    '(g)' : ['(G)'] ,\
    '(x)' : ['(X)'] ,\
    '(h)' : ['(H)'] ,\
    '(i)' : ['(I)'] ,\
    '(m)' : ['(M)'] ,\
    ':D' : [':d', ':-d', ':-D', ':>'] ,\
    '(l)' : ['(L)'] ,\
    '(k)' : ['(K)'] ,\
    '(f)' : ['(F)'] ,\
    ':(' : [':-(', ':<'] ,\
    ':)' : [':-)'] ,\
    ':p' : [':P', ':-P'] ,\
    ':-O' : [':O', ':o', ':-o'] ,\
    '(t)' : ['(T)'] ,\
    '(n)' : ['(N)'] ,\
    '(y)' : ['(Y)'] ,\
    '(w)' : ['(W)'] ,\
    ';)' : [';-)'] ,\
    '(r)' : ['(R)'],\
    ':-[' : [':['],\
}
SMILEY_TO_NAME = {\
    '<:o)' : 'party_smiley',\
    '(ap)' : 'Airplane',\
    '(a)' : 'angel',\
    ':@' : 'angry',\
    '(au)' : 'Auto',\
    '8o|' : 'Baring_teeth_smiley',\
    '(b)' : 'beer',\
    '(^)' : 'Birthday_cake',\
    '(bah)' : 'Black_sheep',\
    '(nah)' : 'goat',\
    '(||)' : 'Bowl',\
    '(z)' : 'Boy',\
    '(u)' : 'brheart',\
    '(p)' : 'photo',\
    '(@)' : 'pussy',\
    '(ci)' : 'Cigarette',\
    '(o)' : 'Clock',\
    '(c)' : 'coffee',\
    '(co)' : 'computer',\
    ':s' : 'confused',\
    ':\'(' : 'cry',\
    '(6)' : 'devil',\
    ':|' : 'stare',\
    '(&)' : 'Dog_face',\
    ':-#' : 'Dont_tell_anyone',\
    '(d)' : 'drink',\
    '(e)' : 'mail',\
    ':$' : 'blushing',\
    '8-)' : 'Eye_rolling_smiley',\
    '(~)' : 'video-x-generic',\
    '(yn)' : 'Fingerscrossed',\
    '(g)' : 'Gift_with_a_bow',\
    '(x)' : 'Girl',\
    '(%)' : 'cuffs',\
    '(h5)' : 'Hi_five',\
    '(h)' : 'coolglasses',\
    ':^)' : 'andy',\
    '(ip)' : 'Island_with_a_palm_tree',\
    '({)' : 'hugleft',\
    '(i)' : 'lamp',\
    '(li)' : 'weather-storm',\
    '(m)' : 'Messenger',\
    '(mp)' : 'phone',\
    '(mo)' : 'Money',\
    '8-|' : 'face-glasses',\
    '(8)' : 'audio-x-generic',\
    ':D' : 'grin',\
    '(pi)' : 'Pizza',\
    '(pl)' : 'Plate',\
    '(r)' : 'rainbow',\
    '(st)' : 'weather-showers-scattered',\
    '(l)' : 'emblem-favorite',\
    '(k)' : 'kiss',\
    '(f)' : 'flower',\
    '(})' : 'hugright',\
    ':(' : 'unhappy',\
    '^o)' : 'Sarcastic_smiley',\
    ':-*' : 'Secret_telling_smiley',\
    '+o(' : 'sick',\
    '(S)' : 'weather-clear-night',\
    '|-)' : 'sleeping',\
    ':)' : 'smile',\
    ':p' : 'tongue',\
    '(sn)' : 'Snail',\
    '(so)' : 'Soccer_ball',\
    '(*)' : 'star',\
    '(#)' : 'weather-clear',\
    ':-O' : 'oh',\
    '(t)' : 'Telephone_receiver',\
    '*-)' : 'Thinking_smiley',\
    '(n)' : 'thumbdown',\
    '(y)' : 'thumbup',\
    '(tu)' : 'Turtle',\
    '(um)' : 'Umbrella',\
    ':-[' : 'bat',\
    '(w)' : 'brflower',\
    ';)' : 'wink',\
    '(xx)' : 'Xbox',\
    '*red+u' : 'im', \
    '*bgca' : 'im', \
    '*hsus' : 'im', \
    '*naf' : 'im', \
    '*mssoc' : 'im', \
    '*9mil' : 'im', \
    '*sierra' : 'im', \
    '*help' : 'im', \
    '*komen' : 'im', \
    '*unicef' : 'im', \
}
ACCEPTED_FORMATS = ['png', 'gif', 'svg', 'xpm']
def resizePixbuf(pixbuf, height, width):
    pWidth, pHeight = pixbuf.get_width(), pixbuf.get_height()
    if pWidth == width and pHeight == height:
        return pixbuf
    destX = destY = 0
    destWidth, destHeight = width, height
    if pWidth > pHeight:
        scale = float(width) / pWidth
        destHeight = int(scale * pHeight)
        destY = int((height - scale * pHeight) / 2)
    elif pHeight > pWidth:
        scale = float(height) / pHeight
        destWidth = int(scale * pWidth)
        destX = int((width - scale * pWidth) / 2)
    else:
        return pixbuf.scale_simple(width, height, gtk.gdk.INTERP_BILINEAR)
    if scale == 1:
        return pixbuf
    scaled = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, True, 8, width, height)
    pixbuf.scale(scaled, 0, 0, width, height, \
                    destX, destY, scale, scale, \
                    gtk.gdk.INTERP_BILINEAR)
    return scaled
def sortedKeysByLen(adict, reverse=True):
    keys = adict.keys()
    keys.sort(lambda left, right: cmp(len(left),len(right)), reverse=reverse)
    return keys
class BaseTheme:
    '''Common functions to both theme classes'''
    def setTheme(self, name, small=False):
        '''Sets the current theme to the name given.
        In case of error, it will directly fallback to the default theme
        Returns True if the specified theme was correctly set, otherwise
        False'''
        if name == 'default' and not small:
            self.pixbufs = self.defaults
            self.path = self.defaultPath
            return True
        else:
            self.pixbufs = {}
        path = None
        smallFound = False
        SMALL_PATH = self.HOME_PATH + os.sep + '.' + name + 'Small'
        if small and os.path.isdir(SMALL_PATH):
            path = SMALL_PATH
            smallFound = True
        elif os.path.isdir(self.HOME_PATH + os.sep + name):
            path = self.HOME_PATH + os.sep + name
        elif os.path.isdir(self.SYSTEM_WIDE_PATH + os.sep + name):
            path = self.SYSTEM_WIDE_PATH + os.sep + name
        else:
            emesenelib.common.debug('Theme '+ name + 
                      ' couldn\'t be found where it was supposed to be')
            emesenelib.common.debug('Falling back to the default theme')
            self.path = self.defaultPath
            return False
        if small and not smallFound:
            if not os.path.isdir(SMALL_PATH):
                path = self.makeSmall(path, SMALL_PATH)
            else:
                path = path + 'Small'
        self.small = small
        self.path = path + os.sep
        return True
    def makeSmall(self, path, spath):
        return path
    def searchPixbuf(self, path, name, isLocation=False, pixbufCreator=None):
        '''Finds name in path with ACCEPTED_FORMATS extensions
        Returns location if found and if isLocation
        Else, return pixbufCreator(location)'''
        for extension in ACCEPTED_FORMATS:
            location = path + name + '.' + extension
            if isLocation:
                if os.path.exists(location):
                    return location
            else:
                try:
                    pixbuf = pixbufCreator(location)
                    return pixbuf
                except:
                    pass
        return None
    def getImage(self, name, animated=False):
        '''Returns a pixbuf of the requested image from the current theme
        If the requested image name is not found in the current theme, the
        default theme's corresponding image will be returned.
        If the default theme doesn't have such a image, None is returned'''
        if name in self.pixbufs: return self.pixbufs[name]
        if animated:
            creator = gtk.gdk.PixbufAnimation
        else:
            creator = gtk.gdk.pixbuf_new_from_file
        pixbuf = self.searchPixbuf(self.path, name, pixbufCreator=creator)
        if pixbuf:
            self.pixbufs[name] = pixbuf
            return pixbuf
        if name in self.defaults:
            return self.defaults[name]
        self.defaults[name] = self.searchPixbuf(self.defaultPath, name, pixbufCreator=creator)
        return self.defaults[name]
    def getImageIcon(self, name, animated=False):
        '''Returns the location of the requested image from the current theme
        If the requested image name is not found in the current theme, the
        default theme's corresponding image will be returned.
        If the default theme doesn't have such a image, None is returned'''
        location = self.searchPixbuf(self.path, name, isLocation=True)
        if location:
            return location
        else:
            return self.searchPixbuf(self.defaultPath, name, isLocation=True)
class Theme(BaseTheme):
    '''This class contains all the data related to a theme:
    images, icons, animations, path to sounds etc'''
    def __init__(self, config, name=None, sname='default'):
        '''Constructor'''
        self.config = config
        self.defaults = {}
        self.pixbufs = {}
        self.HOME_PATH = paths.THEME_HOME_PATH 
        self.SYSTEM_WIDE_PATH = paths.THEME_SYSTEM_WIDE_PATH
        self.defaultPath = paths.DEFAULT_THEME_PATH
        if name:
            self.setTheme(name)
        else:
            self.path = self.defaultPath
        self.smilies = SmilieTheme(self, sname)
        self.getSmileysList = self.smilies.getSmileysList
        self.getSmiley = self.smilies.getSmiley
        self.getSmileyPath = self.smilies.getSmileyPath
        self.smileyParser = self.smilies.smileyParser
        self.getSmileysList = self.smilies.getSmileysList
        self.getSingleSmileysList = self.smilies.getSingleSmileysList
    def statusToPixbuf(self, status):
        '''Translates a status code in a pixbuf returning the according
        theme image'''
        name = None
        if status == 'NLN' or status == 'online':
            name = 'online'
        elif status == 'AWY' or status == 'away':
            name = 'away'
        elif status == 'BSY' or status == 'busy':
            name = 'busy'
        elif status == 'BRB' or status == 'brb':
            name = 'brb'
        elif status == 'PHN' or status == 'phone':
            name = 'phone'
        elif status == 'LUN' or status == 'lunch':
            name = 'lunch'
        elif status == 'HDN' or status == 'invisible':
            name = 'invisible'
        elif status == 'IDL' or status == 'idle':
            name = 'idle'
        elif status == 'FLN' or status == 'offline':
            name = 'offline'
        elif status == 'DCD' or status == 'disconnected':
            name = 'trayicon'
        elif status == 'LOG' or status == 'login':
            name = 'trayicon2'
        if name:
            return self.getImage(name)
        else:
            return None
    def hasUserDisplayPicture(self, contact):
        '''return True if the user has a cached Display picture'''
        imagePath = os.path.join(self.config.getCachePath(), \
            contact.displayPicturePath)
        if (os.path.exists(imagePath) and os.path.isfile(imagePath)):
            return True
        else:
            return False
    def getUserDisplayPicture(self, contact, width=96, height=96):
        '''return a pixbuf with the display picture of the user
        or the default image if not found'''
        imagePath = os.path.join(self.config.getCachePath(), \
            contact.displayPicturePath)
        try:
            if os.path.exists(imagePath) and os.path.isfile(imagePath):
                pixbuf = gtk.gdk.pixbuf_new_from_file(imagePath)
            else:
                lastCached=getLastCachedAvatar(contact.email, self.config.getCachePath())
                if os.path.isfile(lastCached):
                    pixbuf = gtk.gdk.pixbuf_new_from_file(lastCached)
                else:
                    pixbuf = self.getImage('login')
        except:
            pixbuf = self.getImage('login')
        if width == 96 and height == 96:
            return pixbuf
        else:
            return resizePixbuf(pixbuf, width, height)
    def makeSmall(self, path, spath):
        os.mkdir(spath)
        try:
            for root, dirs, files in os.walk(path):
                del dirs
                if root != path: continue
                for file in files:
                    name = file.split('.')[0]
                    fullname = os.path.join(root, file)
                    destname = os.path.join(spath, file)
                    if name in ('away','brb','busy','icon','idle','lunch', \
                                'invisible','mobile','offline','online', \
                                'phone'):
                        pixbuf = resizePixbuf(gtk.gdk.pixbuf_new_from_file(fullname), 16, 16)
                        pixbuf.save(destname, 'png')
                    elif file.split('.')[1] in ACCEPTED_FORMATS:
                        copyfile(fullname,destname)
            return spath
        except Exception, e:
            print "Error creating small theme", e
            return path
class SmilieTheme(BaseTheme):
    def __init__(self, parent, name):
        '''Constructor'''
        self.theme = parent
        self.defaults = {}
        self.pixbufs = {}
        self.defaultPath = paths.DEFAULT_SMILIES_PATH
        self.HOME_PATH = paths.SMILIES_HOME_PATH
        self.SYSTEM_WIDE_PATH = paths.SMILIES_SYSTEM_WIDE_PATH
        if name:
            self.setTheme(name)
        else:
            self.path = self.defaultPath
        self.smileyToName = {}
        for (key,value) in SMILEY_TO_NAME.iteritems():
            self.smileyToName[key] = value
            if key in ALTERNATIVE_SMILEYS:
                for i in ALTERNATIVE_SMILEYS[key]:
                    self.smileyToName[i] = value
        smileyPattern = ''
        for smiley in sortedKeysByLen(self.smileyToName):
            smileyPattern += re.escape(emesenelib.common.escape(smiley)) + '|'
        self.reSmiley = re.compile('('+smileyPattern[:-1]+')')
    def smileyParser(self, text, callback=None):
        '''return text with smileys parsed in html
        you can set a callback to handle smileys parsing'''
        if callback == None:
            return self.reSmiley.sub(lambda data: '<img src="file://%s"/>'%self.getSmileyPath(emesenelib.common.unescape(data.groups()[0])), text)
        else:
            return self.reSmiley.sub(lambda data: callback(self.getSmileyPath(emesenelib.common.unescape(data.groups()[0]))), text)
    def getSmiley(self, shortcode, animated=True):
        '''Returns a pixbuf representing the corresponding smiley if we have one,
        otherwise None'''
        if shortcode in self.smileyToName:
            pixbuf = self.getImage(self.smileyToName[shortcode], animated)
            if not animated and type(pixbuf) == gtk.gdk.PixbufAnimation:
                pixbuf = pixbuf.get_static_image()
            return pixbuf
    def getSmileyPath(self, shortcode):
        '''Returns the path of the smiley if we have one,
        otherwise None'''
        if shortcode in self.smileyToName:
            return self.getImageIcon(self.smileyToName[shortcode], True)
    def getSmileysList(self):
        '''Returns a list of all the currently accepted smileys shortcodes'''
        return sortedKeysByLen(self.smileyToName)
    def getSingleSmileysList(self):
        '''Returns a list of the currently accepted smileys shortcodes,
        without repetitions'''
        return sortedKeysByLen(SMILEY_TO_NAME)
