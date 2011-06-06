import Plugin, Theme
import gtk
import gobject
import pango
from time import *
from emesenelib.common import unescape
class MainClass( Plugin.Plugin ):
    description = _('Show a list with the history of online/offline events of every contact with a timestamp.')
    authors = { 'mr.archano' : 'archano@gmail.com', 'Jan de Mooij' : 'jandemooij@gmail.com' }
    website = ''
    displayName = _('StatusHistory')
    name = 'StatusHistory'
    def __init__( self, controller, msn ):
        Plugin.Plugin.__init__( self, controller, msn )
        self.description = _('Show a list with the history of online/offline events of every contact with a timestamp.')
        self.authors = { 'mr.archano' : 'archano@gmail.com',
            'Jan de Mooij' : 'jandemooij@gmail.com' }
        self.website = ''
        self.displayName = _('StatusHistory')
        self.name = 'StatusHistory'
        self.enabled = False
        self.controller = controller
        self.config = controller.config
        self.config.readPluginConfig(self.name)
        self.showStatusImage = ((self.config.getPluginValue(self.name,
            'showStatusImage', '1')) == '1')
    def start( self ):
        self.listStore = gtk.ListStore(gobject.TYPE_STRING, gobject.TYPE_STRING,
            gobject.TYPE_STRING)
        self.box = gtk.VBox()
        self.addComboBox(self.box)
        self.addEntry('connected')
        self.box.show_all()
        self.controller.mainWindow.vbox.pack_start(self.box, False, False)
        self.onOnlineId = self.controller.msn.connect('user-online',
            self.on_online)
        self.onOfflineId = self.controller.msn.connect('user-offline',
            self.on_offline)
        self.enabled = True
    def stop( self ):
        self.controller.mainWindow.vbox.remove( self.box )
        self.disconnect( self.onOnlineId )
        self.disconnect( self.onOfflineId )
        self.enabled = False
    def check( self ):
        return ( True, 'Ok' )
    def configure( self ):
        l=[]
        l.append( Plugin.Option( 'showStatusImage', bool,
            _('Show status image:'), _('Show status image:'),
            self.showStatusImage))
        response = Plugin.ConfigWindow(
            _( 'StatusHistory plugin config' ), l ).run()
        if response != None:
            self.showStatusImage = response['showStatusImage'].value
            self.config.setPluginValue( self.name, 'showStatusImage',
                str(int(self.showStatusImage)) )
            if self.enabled:
                self.box.remove(self.comboBox)
                self.addComboBox(self.box)
        return True
    def addComboBox(self, box):
        self.comboBox = gtk.ComboBox(self.listStore)
        self.timeTextCell = gtk.CellRendererText()
        self.nickTextCell = gtk.CellRendererText()
        self.nickTextCell.set_property('ellipsize', pango.ELLIPSIZE_END)
        if self.showStatusImage:
            self.statusCell = gtk.CellRendererPixbuf()
        else:
            self.statusCell = gtk.CellRendererText()
        self.comboBox.pack_start(self.timeTextCell, False)
        self.comboBox.pack_start(self.nickTextCell, True)
        self.comboBox.pack_start(self.statusCell, False)
        self.comboBox.add_attribute(self.timeTextCell, 'text', 0)
        self.comboBox.add_attribute(self.nickTextCell, 'text', 1)
        if self.showStatusImage:
            self.comboBox.set_cell_data_func(self.statusCell,
                self.cellLayoutFunc)
        else:
            self.comboBox.add_attribute(self.statusCell, 'text', 2)
        self.comboBox.set_active(0)
        self.comboBox.show()
        box.pack_start(self.comboBox, False, False)
    def cellLayoutFunc(self, layout, cell, model, iter):
        '''show pixbuf for status'''
        item = model[iter][2]
        if item == None:
            return
        if item == 'connected':
            item = 'online'
        pixbuf = self.controller.theme.statusToPixbuf(item)
        cell.set_property('pixbuf', Theme.resizePixbuf(pixbuf, 16, 16))
    def addEntry(self, status, email = None):
        '''add a new entry to the liststore'''
        if email != None:
            nick = unescape(self.controller.unifiedParser.getParser(
                        self.controller.msn.contactManager.\
                        getContactNick(email)).get())
        else:
            nick = self.controller.msn.user
        time = strftime('[%H:%M:%S]', localtime())
        self.listStore.prepend([time, nick, status])
        self.comboBox.set_active(0)
    def on_online( self, msnp, email, oldStatus ):
        if oldStatus == 'FLN':
            self.addEntry('online', email)
    def on_offline( self, msnp, email ):
        self.addEntry('offline', email)
