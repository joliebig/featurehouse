import gtk
from Plugin import Plugin
class MainClass(Plugin):
    description = _('Shakes the window when a nudge is received.')
    authors = {unicode('Yguaratã C. Cavalcanti','latin-1').encode('latin-1') : 'yguarata at gmail dot com' }
    website = 'http://www.yguarata.org'
    displayName = _('Window Trembling Nudge')
    name = 'WindowTremblingNudge'
    def __init__(self, controller, msn):
        '''
        Contructor
        '''
        Plugin.__init__(self, controller, msn, 100)
        self.description = _('Shakes the window when a nudge is received.')
        self.authors = {unicode('Yguaratã C. Cavalcanti','latin-1').
            encode('latin-1') : 'yguarata at gmail dot com' }
        self.website = 'http://www.yguarata.org'
        self.displayName = _('Window Trembling Nudge')
        self.name = 'WindowTremblingNudge'
        self.shakeWindowId = None
        self.controller = controller
        self.base_movement = 10
    def start(self):
        self.enabled = True
        self.shakeWindowId = self.connect('nudge-received', 
            self.shakeWindow)
    def stop(self):
        '''stop the plugin'''
        self.disconnect(self.shakeWindowId)
        self.enabled = False
    def check(self):
        '''
        check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> (True , 'some message')
        else -> (False , 'error message')
        '''
        return (True, 'Ok')
    def shakeWindow(self, msnp, mail):
        win, conv = self.controller.conversationManager.getOpenConversation(mail)
        if not win:
            return
        x, y = win.get_position()
        for i in range(30):
            win.move(x+self.base_movement, y-self.base_movement) #-down+right
            gtk.main_iteration()
            win.move(x+self.base_movement, y+self.base_movement) #+up+right
            gtk.main_iteration()
            win.move(x-self.base_movement, y-self.base_movement) #-down-left
            gtk.main_iteration()
            win.move(x-self.base_movement, y+self.base_movement) #+up-left
            gtk.main_iteration()
            win.move(x, y) #start position
            gtk.main_iteration()
