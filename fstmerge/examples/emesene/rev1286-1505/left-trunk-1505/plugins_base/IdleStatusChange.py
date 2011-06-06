import Plugin
from gobject import timeout_add, source_remove
try:
    import dbus
    dbusError = ''
    if getattr( dbus, 'version', ( 0, 0, 0 ) ) >= ( 0, 41, 0 ):
        import dbus.glib
except Exception, e:
    dbusError = e
class MainClass( Plugin.Plugin ):
    description = _( 'Changes status to selected one if session is idle (you must use gnome and have gnome-screensaver installed)' ) 
    authors = { 'mg' : 'themgzzy at gmail dot com' }
    displayName = _( 'Idle Status Change' )
    name = 'IdleStatusChange'
    def __init__( self, controller, msn ):
        Plugin.Plugin.__init__( self, controller, msn )
        self.description = _( 'Changes status to selected one if session is idle (you must use gnome and have gnome-screensaver installed)' ) 
        self.authors = { 'mg' : 'themgzzy at gmail dot com' }
        self.displayName = _( 'Idle Status Change' )
        self.name = 'IdleStatusChange'
        self.config = controller.config
        self.config.readPluginConfig( self.name )
        self.idlestatus = self.config.getPluginValue( self.name, 'idlestatus', '' )
        self.controller = controller
        self.msn = msn
        self.oldstatus = self.msn.status
        self.isIdle = False
    def start( self ):
        '''start the plugin'''
        try:
            self.startDbus()
        except:
            return
        self.enabled = True
        self.tag = timeout_add( 5000, self.idle_state )
    def stop( self ):
        source_remove(self.tag)
        self.enabled = False
    def check( self ):
        '''
        check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )
        '''
        if dbusError != '':
            return ( False, 'Can\'t Load dbus ' + str(dbusError) )
        return ( True, 'Ok' )
    def configure( self ):
        statuslist = [ 'idle', 'online', 'away', 'busy', 'brb', 'phone', 
             'lunch', 'invisible' ]
        l = [ Plugin.Option( 'idlestatus', list, _( 'Idle Status:' ), _( 'Set the idle status' ), self.config.getPluginValue( self.name, 'idlestatus', '' ), statuslist ) ]
        response = Plugin.ConfigWindow( _( 'Idle Status Change configuration' ), l ).run()
        if response != None:
            self.idlestatus = response['idlestatus'].value
            self.config.setPluginValue( self.name, 'idlestatus', self.idlestatus )
        return True
    def startDbus( self ):
        '''Start dbus session'''
        self.session_bus = dbus.SessionBus()
        self.screensaver = self.session_bus.get_object( 'org.gnome.ScreenSaver', '/org/gnome/ScreenSaver' )
    def idle_state( self ):
        idlestate = self.screensaver.GetSessionIdle()
        if idlestate and not self.isIdle and self.msn.status != "HDN":
            self.isIdle = True
            self.oldstatus = self.msn.status
            self.msn.changeStatus( self.idlestatus )
        elif not idlestate and self.isIdle:
            self.isIdle = False
            self.msn.changeStatus( self.oldstatus )
        return True
