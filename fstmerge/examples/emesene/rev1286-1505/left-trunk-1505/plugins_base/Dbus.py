import os
import Plugin
try:
    import dbus
    dbusError = ''
    if getattr(dbus, 'version', (0,0,0)) >= (0,41,0):
        import dbus.glib
        import dbus.service
        import dbus.mainloop.glib
except Exception, e:
    dbusError = e
BUS_NAME = 'org.emesene.dbus'
OBJECT_PATH = '/org/emesene/dbus'
class MainClass(Plugin.Plugin):
    '''Main plugin class'''
    description = _('With this plugin you can interact via Dbus with emesene')
    authors = {'Roger Duran' : 'RogerDuran at gmail dot com'}
    website = 'http://www.rogerpc.com.ar'
    displayName = _('Dbus')
    name = 'Dbus'
    def __init__(self, controller, msn):
        '''Contructor'''
        Plugin.Plugin.__init__(self, controller, msn)
        self.theme = controller.theme
        self.description = _('With this plugin you can interact via Dbus with emesene')
        self.authors = {'Roger Duran' : 'RogerDuran at gmail dot com'}
        self.website = 'http://www.rogerpc.com.ar'
        self.displayName = _('Dbus')
        self.name = 'Dbus'
        self.user_connect_id = None
        self.user_offline_id = None
        self.self_nick_changed_id = None
        self.psm_changed_id = None
        self.dbus_object = None
    def start( self ):
        '''start the plugin'''
        self.start_dbus()
        self.user_connect_id = self.connect('user-online', self.user_connect)
        self.user_offline_id = self.connect('user-offline', self.user_offline)
        self.self_nick_changed_id = self.connect('self-nick-changed',
                                                  self.self_nick_changed)
        self.psm_changed_id = self.connect('personal-message-changed',
                                            self.psm_changed)
        self.enabled = True
    def stop( self ):    
        '''stop the plugin'''
        self.disconnect( self.user_connect_id )
        self.disconnect( self.user_offline_id )
        self.disconnect( self.self_nick_changed_id )
        self.disconnect( self.psm_changed_id )
        self.destroy_dbus_session()
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
        if dbusError != '':
            self.destroy_dbus_session()
            return (False, 'Can\'t Load dbus', dbusError)
        return (True, 'Ok')
    def start_dbus(self):
        '''Start dbus session'''
        self.destroy_dbus_session()
        self.session_bus = dbus.SessionBus()
        self.bus_name = dbus.service.BusName(BUS_NAME, bus=self.session_bus)
        self.dbus_object = EmeseneObject(self.bus_name, OBJECT_PATH, self)
    def destroy_dbus_session(self):
        '''Destroy current dbus session'''
        if self.dbus_object:
            try:
                dbus.service.Object.remove_from_connection(self.dbus_object)
            except AttributeError:
                pass
            self.dbus_object = None
    def user_connect(self, msnp, user, oldStatus):
        if(oldStatus=='FLN'):
            self.dbus_object.user_connect(user)
    def user_offline(self, msnp, user):
        self.dbus_object.user_offline(user)
    def self_nick_changed(self, msnp, old, nick):
        self.dbus_object.self_changed_nick(nick)
    def psm_changed(self, msnp, email, pm):
        self.dbus_object.psm_changed(email, pm)
try:
    class EmeseneObject(dbus.service.Object):
        def __init__(self, bus_name, object_path, main):
            dbus.service.Object.__init__(self, bus_name, object_path)
            self.main = main
            self.msn = main.msn
            self.controller = main.controller
            self.contact_manager = self.controller.contacts
        @dbus.service.method(BUS_NAME)
        def set_psm(self, text):
            self.contact_manager.set_message(text)
            return 'Psm Changed'
        @dbus.service.method(BUS_NAME)
        def set_media(self, text):
            self.contact_manager.set_media(text)
            return 'Current Media Changed'
        @dbus.service.method(BUS_NAME)
        def set_status(self, status):
            self.contact_manager.set_status(status)
            return 'Status Changed'
        @dbus.service.method(BUS_NAME)
        def set_nick(self, nick):
            self.contact_manager.set_nick(nick)
            return 'Nick changed'
        @dbus.service.method(BUS_NAME)
        def set_avatar(self, path):
            ''' set avatar '''
            if path != None and os.path.exists(path):
                self.controller.changeAvatar(path)
                return True
            return False
        @dbus.service.method(BUS_NAME)
        def open_conversation(self, email=''):
            try:
                self.controller.newConversation(None, email)
                return 'opened ' + str(email)
            except Exception, e:
                return 'error trying to start a conversation: ' + str(e)
        @dbus.service.method(BUS_NAME, out_signature='aa{ss}')
        def get_online_users(self):
            users = self.contact_manager.get_online_list()
            contacts = []
            for user in users:
                user_status = self.contact_manager.get_status(user.account)
                contact = {'email': user.account, 'name': user.nick, 'status': user_status}
                contacts.append(contact)
            return contacts
        @dbus.service.method(BUS_NAME)
        def get_contact_nick(self, email):
            return self.contact_manager.get_nick(email)
        @dbus.service.method(BUS_NAME)
        def get_psm(self, email):
            return self.contact_manager.get_message(email)
        @dbus.service.signal(BUS_NAME)
        def user_connect(self, user):
            pass
        @dbus.service.signal(BUS_NAME)
        def user_offline(self, email):
            pass
        @dbus.service.signal(BUS_NAME)
        def self_changed_nick(self, nick):
            pass
        @dbus.service.signal(BUS_NAME)
        def psm_changed(self, email, pm):
            pass
except Exception, e:
    dbusError = e
