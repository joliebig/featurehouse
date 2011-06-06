import gtk
from Plugin import Plugin
from emesenecommon import *
class MainClass(Plugin):
    def __init__(self, controller, msn):
        '''
        Contructor
        '''
        Plugin.__init__(self, controller, msn, 100)
        self.alpha = 2
        self.description = _('Shakes the window when a nudge is received.')
        self.authors = {unicode('Yguaratã C. Cavalcanti','latin-1').
            encode('latin-1') : 'yguarata at gmail dot com' }
        self.website = 'http://www.yguarata.org'
        self.displayName = _('Window Trembling Nudge')
        self.name = 'WindowTremblingNudge'
        self.shakeWindowId = None
        self.controller = controller
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
        tmp = self.controller.conversationManager.getOpenConversation(mail)
        if tmp:
            conversationWindow, conversation = tmp
        else:
            return
        x, y = conversationWindow.get_position()
        for i in range(50):
            if i%2 == 0:
                conversationWindow.move(x+self.alpha, y)
                conversationWindow.show()
                conversationWindow.move(x, y+self.alpha)
                conversationWindow.show()
            else:
                conversationWindow.move(x-(self.alpha*2), y)
                conversationWindow.show()
                conversationWindow.move(x, y-(self.alpha*2))
                conversationWindow.show()
            gtk.main_iteration(False)
        conversationWindow.move(x, y)
        conversationWindow.show()
