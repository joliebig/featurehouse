VERSION = '0.1'
import Plugin
class MainClass( Plugin.Plugin ):
    '''Main plugin class'''
    def __init__( self, controller, msn ):
        '''Contructor'''
        Plugin.Plugin.__init__( self, controller, msn )
        self.description = _( 'Edit your config file' )
        self.authors = { 'j0hn' : 'j0hn.com.ar@gmail.com' }
        self.website = ''
        self.displayName = _( 'Tweaks' )
        self.name = 'Tweaks'
        self.config = controller.config
        self.controller = controller
    def start( self ):
        '''start the plugin'''
        self.enabled = True
    def stop( self ):    
        '''stop the plugin'''
        self.enabled = False
    def action( self ):
        pass
    def check( self ):
        return ( True, 'Ok' )
    def configure( self ):
        l = []
        configDict = {}
        for key, value in self.config.user:
            configDict[key] = value
        config = Plugin.Option('config', dict, 'Custom config', \
            'Config file', None, configDict)
        l.append( config )
        window = Plugin.ConfigWindow( _( 'Config config' ), l )
        window.set_default_size(400, -1)
        response = window.run()
        configWidget = config.widget
        if response != None:
            for key in configWidget.data.iterkeys():
                if configWidget.data[key] == "1":
                    configWidget.data[key] = True
                elif configWidget.data[key] == "0":
                    configWidget.data[key] = False
                if self.config.user[key] != configWidget.data[key]:
                    self.config.user[key] = configWidget.data[key]
        return True
