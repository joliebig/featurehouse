VERSION = '0.1'
import Plugin
from Parser import Url
import gtk
import urllib
ERROR = ''
MIMETYPE = 'application/x-youtube-thumb'
OBJECT_TEMPLATE = '<object class="%s" type="' + MIMETYPE + '"/>'
THUMBNAIL_URL = 'http://img.youtube.com/vi/%s/default.jpg'
class MainClass( Plugin.Plugin ):
    '''Main plugin class'''
    description = _( 'Displays thumbnails of youtube videos next to links' )
    authors = { 'Roger Duran' : 'RogerDuran at gmail dot com' }
    website = 'http://www.rogerpc.com.ar'
    displayName = _( 'Youtube' )
    name = 'Youtube'
    def __init__( self, controller, msn ):
        '''Constructor'''
        Plugin.Plugin.__init__( self, controller, msn )
        self.description = _( 'Displays thumbnails of youtube videos next to links' )
        self.authors = { 'Roger Duran' : 'RogerDuran at gmail dot com' }
        self.website = 'http://www.rogerpc.com.ar'
        self.displayName = _( 'Youtube' )
        self.name = 'Youtube'
        self.controller = controller
        self.parser = controller.unifiedParser
        self.convManager = controller.conversationManager
        self.config = controller.config
        self.postParseId = None
    def start( self ):
        '''start the plugin'''
        self.postParseId = self.parser.connect('filter', self.parseUrl )
        self.convManager.connect( 'new-conversation-ui', \
            self.newConversationWindow )
        for conversation in self.getOpenConversations():
            textView = conversation.ui.textview
            textView.customObjectsCallbacks.update({
                MIMETYPE: (self.youtubeWidget,None)
             })
        self.enabled = True
    def stop( self ):    
        '''stop the plugin'''
        self.controller.unifiedParser.disconnect( self.postParseId )
        for conversation in self.getOpenConversations():
            textView = conversation.ui.textview
            if MIMETYPE in textView.customObjectsCallbacks:
                textView.customObjectsCallbacks.pop(MIMETYPE)
        self.enabled = False
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
    def newConversationWindow( self, conversationmanager, conversation, win ):
        textView = conversation.ui.textview
        textView.customObjectsCallbacks.update({
                       MIMETYPE: (self.youtubeWidget,None)
                     })
    def youtubeWidget( self, textview, id, path=None ):
        img = gtk.Image()
        img.set_from_stock( gtk.STOCK_MISSING_IMAGE, \
            gtk.ICON_SIZE_SMALL_TOOLBAR )
        img.show()
        def getImage():
            try:
                mem = urllib.urlopen(THUMBNAIL_URL % id).read(10*1024*1024)
                loader = gtk.gdk.PixbufLoader()
                loader.write(mem)
                loader.close()
                return (textview, loader, img)
            except:
                return
        defrun = Plugin.Function(getImage, self.callback)
        defrun()
        return img
    def callback(self, handler):
        try:
            textview, loader, img = handler
            textview.scrollLater()
            img.set_from_animation(loader.get_animation())
        except:
            pass
    def parseUrl( self, parser, dataType, filterdata ):
        if not dataType or dataType.getDataType() != 'conversation':
            return
        newObjects = []
        for object in filterdata.list:
            code = ''
            if type(object) == Url and \
               object.url.count('youtube.com') and \
               object.url.count('watch?v='):
                videoid = object.url.split('watch?v=')[1].split('&')[0]
                code = OBJECT_TEMPLATE % videoid
                html = object.getHtml()
                object.getHtml = lambda: code + html
            newObjects.append(object)
        filterdata.list = newObjects
