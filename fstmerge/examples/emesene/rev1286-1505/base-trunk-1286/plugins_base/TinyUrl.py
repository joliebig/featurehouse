VERSION = '0.1'
from urllib import urlopen, urlencode
import Plugin
class MainClass( Plugin.Plugin ):
    '''Main plugin class'''
    def __init__( self, controller, msn ):
        '''Contructor'''
        Plugin.Plugin.__init__( self, controller, msn )
        self.description = 'Create tiny url with /tiny <url>'
        self.authors = { 'Roger Duran' : 'rogerduran@gmail.com' }
        self.website = 'http://www.rogerpc.com.ar'
        self.displayName = 'Tiny Url'
        self.name = 'TinyUrl'
        self.controller = controller
        self.Slash = controller.Slash
    def start( self ):
        '''start the plugin'''
        self.Slash.register('tiny', self.get_tiny, 'Create a tiny url')
        self.enabled = True
    def get_tiny( self, slash_action ):
        '''Return Tiny Url'''
        data = slash_action.getParams()
        params = urlencode({'url': data})
        page = urlopen('http://tinyurl.com/create.php',params).read()
        url = page.split('[<a href="')[1].split('" ')[0]
        slash_action.outputText( url, True )
    def stop( self ):    
        '''stop the plugin'''
        self.Slash.unregister('tiny')
        self.enabled = False
    def check( self ):
        '''Check Plugin'''
        return ( True, 'Ok' )
