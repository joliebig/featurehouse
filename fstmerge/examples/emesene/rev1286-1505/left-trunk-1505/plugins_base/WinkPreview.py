import os
import subprocess
from Plugin import Plugin
class MainClass(Plugin):
    description = _('Show preview image when you receive a wink.')
    authors = {'Jan de Mooij': 'jandemooij@gmail.com'}
    website = ''
    displayName = _('Wink Preview')
    name = 'WinkPreview'
    def __init__(self, controller, msn):
        Plugin.__init__(self, controller, msn)
        self.description = _('Show preview image when you receive a wink.')
        self.authors = {'Jan de Mooij': 'jandemooij@gmail.com'}
        self.website = ''
        self.displayName = _('Wink Preview')
        self.name = 'WinkPreview'
        self.msn = msn
        self.enabled = False
    def start(self):
        self.wink_id = self.msn.connect('switchboard::wink', self.on_wink)
        self.wink_transferred_id = self.msn.connect('wink-transferred', \
                self.on_wink_transferred)
        self.enabled = True
    def stop(self):
        self.msn.disconnect(self.wink_id)
        self.msn.disconnect(self.wink_transferred_id)
        self.enabled = False
    def check(self):
        try:
            subprocess.call(['cabextract'], #stderr=subprocess.PIPE, 
                stdout=subprocess.PIPE)
        except OSError:
            return (False, _('cabextract not installed'))
        return (True, 'Ok')
    def get_conversation(self, switchboard):
        for conversation in self.getOpenConversations():
            if conversation.getSwitchboard() == switchboard:
                return conversation
        return None
    def on_wink(self, msn, switchboard, signal, (mail, msnobj)):
        '''this method is called when a wink is received'''
        html = '<object type="application/x-emesene-wink" class="%s" ' \
            'data="%s"></object>' % (msnobj.sha1d, msnobj.sha1d)
        conversation = self.get_conversation(switchboard)
        if conversation:
            conversation.ui.textview.display_html(html.encode('ascii', \
                'xmlcharrefreplace'))
    def on_wink_transferred(self, msn, to, msnobj, path):
        '''call when a wink is transfered'''
        cabfile = os.path.join(path, 'wink.cab')
        retval = subprocess.call(['cabextract', '-d' + path, cabfile])
        xmlfile = os.path.join(path, 'content.xml')
        lines = open(xmlfile, 'r').readlines()
        image = ''
        for line in lines:
            if 'thumbnail' in line:
                image = line.split('file="')[1].split('"')[0]
                break
        image_path = os.path.join(path, image)
        for conversation in self.getOpenConversations():
            conversation.ui.textview.setCustomObject(msnobj.sha1d, \
                image_path, type='application/x-emesene-wink')
