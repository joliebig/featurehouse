import gtk
import Queue
import thread
import gobject
import inspect
from DebugManager import DebugLevel
class Plugin( object ):
    '''Base class to make a plugin'''
    name = ''
    displayName = ''
    description = ''    # name : mail
    authors = {}
    website = ''
    def __init__( self, controller, msn, period = None ):
        '''Contructor, the period parameter is the time between calls to 
        the function, in milliseconds, if period = None then the plugin 
        is asynchronous.'''
        self.controller = controller
        self.msn = msn
        self.period = period
        self.enabled = False
    def check( self ):
        '''check if everything is OK to start the plugin
        return a tuple whith a boolean and a message
        if OK -> ( True , 'some message' )
        else -> ( False , 'error message' )'''
        pass
    def start( self ):
        '''start the plugin'''
        pass
    def stop( self ):
        '''stop the plugin'''
        pass
    def action( self ):
        '''the method that is called every self.period milliseconds'''
        pass
    def connect( self, signal, callback ):
        '''connect a signal to a callback'''
        return self.msn.connect( signal, callback )
    def disconnect( self, callbackId ):
        '''disconnect a signal to a callback'''
        self.msn.disconnect( callbackId )
    def isEnabled( self ):
        '''return True if the plugin is running'''
        return self.enabled
    def getOpenConversations( self ):
        if self.controller.conversationManager == None:
            return ()
        conversations = self.controller.conversationManager.conversations
        return [conversation for (window, conversation) in conversations]
    def debug( self , message, level=DebugLevel.info):
        caller = '%s.%s' % (self.name, inspect.stack()[1][3])
        filename = inspect.stack()[1][0].f_code.co_filename
        category = 'plugin'
        self.controller.debug(message, category, level, caller, filename)
class Function( object ):
    '''a class that represent a function that will run
    on a thread and call a callback with the return value'''
    def __init__( self, function, callback ):
        self.function = function
        self.callback = callback
        self.queue = Queue.Queue( 0 )
    def __call__( self, *args ):
        '''when the object is called we start a thread
        with the function and parameters and start the
        check function that check every 0.2 seconds if
        the thread has ended (there is something in the
        queue), then call the callback with the result'''
        thread.start_new_thread( self._function, args )
        gobject.timeout_add( 200 , self.check )
    def _function( self, *args ):
        '''our function call the parameter function
        with the args and put the return value in the
        queue'''
        self.queue.put( self.function( *args ) )
    def check( self ):
        '''check there is something in the queue 
        that means that self.function has ended
        then call the callback with the result'''
        try:
            result = self.queue.get( True, 0.01 )
            self.callback( result )
            return False
        except Queue.Empty:
            return True
class Option( object ):
    '''a type of option to use by the plugins'''
    def __init__( self, name, optionType, label, description, value, \
                  options=None, changedcb=None ):
        '''constructor
        name: represent the name of the variable
        optionType: the type expected (str, int, float, bool, list,passwd)
        label: the text that will appear in the label
        descrption: the text in the tooltip
        value: the default value when show and the input value when returned'''
        self.name = name
        self.optionType = optionType
        self.label = label
        self.description = description
        self.value = value
        self.options = options
        self.changedcb = changedcb
        self.widget = None
class ConfigList(gtk.TreeView):
    '''A treeview that displays a dict and allows changing the values'''
    def __init__(self, data):
        self.model = gtk.ListStore(str, str)
        gtk.TreeView.__init__(self, self.model)
        self.refresh(data)
        crtkey = gtk.CellRendererText()
        crtvalue = gtk.CellRendererText()
        crtvalue.set_property('editable', True)
        crtvalue.connect('edited', self.changed, self.model)
        col1 = gtk.TreeViewColumn(_("Key"), crtkey, text=0)
        col2 = gtk.TreeViewColumn(_("Value"), crtvalue, text=1)
        self.append_column(col1)
        self.append_column(col2)
    def get_data( self ):
        return self.data
    def refresh( self, data ):
        '''Adds dict data to the model'''
        self.data = data
        self.model.clear()
        for item in self.data.keys():
            self.model.append((item, self.data[item]))
    def changed( self, cell, path, new_text, model ):
        '''a value is changed to new_text
        model[path] is the row
        model[path][0] is the key
        model[path][1] is the stored value
        self.data contains the _return_ values'''
        model[path][1] = new_text
        key = model[path][0]
        if key in self.data:
            self.data[key] = new_text
        else:
            pass
class ConfigWindow( gtk.Dialog ):
    '''build a generic window with configuration parameters to fill'''
    def __init__( self, title, optionList ):
        '''constructor'''
        gtk.Dialog.__init__( self, title, None,
            gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, \
             gtk.STOCK_OK, gtk.RESPONSE_OK) )
        self.set_position(gtk.WIN_POS_MOUSE)
        self.set_border_width( 6 )
        vbox = self.vbox
        vbox.set_spacing( 12 )
        self.options = {}
        self.widgets = {}
        for i in optionList:
            self.options[ i.name ] = i
            hbox = gtk.HBox()
            if i.optionType == bool:
                check = gtk.CheckButton( i.label )
                if type( i.value ) == bool:
                    check.set_active( i.value )
                self.widgets[ i.name ] = check.get_active
                if i.changedcb:
                    check.connect( 'toggled', i.changedcb )
                hbox.pack_start( check )
            elif i.optionType in ('list', list) and type(i.options) == list:
                label = gtk.Label( i.label )
                label.set_justify(gtk.JUSTIFY_LEFT)
                label.set_width_chars( 20 )
                combo = gtk.ComboBox()
                liststore = gtk.ListStore(str)
                cellRendererText = gtk.CellRendererText()
                combo.pack_start(cellRendererText)
                combo.add_attribute(cellRendererText, 'text', 0)
                combo.set_model(liststore)
                self.widgets[ i.name ] = combo.get_active_text
                hbox.pack_start( label )
                hbox.pack_start( combo )
                count = 0
                done = False
                for j in i.options:
                    if not done:
                        if j == i.value:
                            done = True
                        else:
                            count += 1
                    liststore.append( [ str( j ) ] )
                if not done:
                    combo.set_active( 0 )
                else:   
                    combo.set_active( count )
                if i.changedcb:
                    combo.connect( 'changed', i.changedcb )
            elif i.optionType == dict:
                label = gtk.Label( i.label )
                label.set_justify(gtk.JUSTIFY_LEFT)
                label.set_width_chars( 20 )
                cfglist = ConfigList(i.options)
                cfglist.set_size_request(-1, 200)
                i.widget = cfglist
                self.widgets[ i.name ] = cfglist.get_data
                scrolledWindow = gtk.ScrolledWindow()
                scrolledWindow.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
                scrolledWindow.add(cfglist)
                vbox.pack_start(label)
                vbox.pack_start(scrolledWindow)
            elif i.optionType == 'passwd':
                label = gtk.Label( i.label )
                label.set_justify(gtk.JUSTIFY_LEFT)
                label.set_width_chars( 20 )
                entry = gtk.Entry()
                entry.set_visibility(False)
                entry.set_text( str( i.value) )
                self.widgets[ i.name ] = entry.get_text
                hbox.pack_start( label )
                hbox.pack_start( entry )
            elif i.optionType == gtk.Widget:
                if i.label:
                    label = gtk.Label( i.label )
                    label.set_justify(gtk.JUSTIFY_LEFT)
                    label.set_width_chars( 20 )
                    hbox.pack_start( label )
                hbox.pack_start( i.value )
                self.widgets[ i.name ] = lambda: None
            else:
                label = gtk.Label( i.label )
                label.set_justify(gtk.JUSTIFY_LEFT)
                label.set_width_chars( 20 )
                entry = gtk.Entry()
                entry.set_text( str( i.value ) )
                if i.changedcb:
                    entry.connect( 'changed', i.changedcb )
                self.widgets[ i.name ] = entry.get_text
                hbox.pack_start( label )
                hbox.pack_start( entry )
            vbox.pack_start( hbox )
        vbox.show_all()
        self.connect( 'delete-event', self.close )
    def run( self, *args ):
        response = gtk.Dialog.run( self )
        self.hide()
        if response == gtk.RESPONSE_OK:
            for i in self.options.keys():
                self.options[ i ].value = self.widgets[ i ]()
            return self.options
        else:
            return None
    def close( self, *args ):
        self.response( gtk.RESPONSE_CLOSE )
def module_require(required, dictionary):
    '''loads module specified by required(iterable) into dictionary
    (usually it's globals()). Return a list of (module, error) tuples
    for each module it wasn't able to import.'''
    error_list = []
    for module in required:
        try:
            dictionary[module] = __import__(module, dictionary, locals(), [])
        except ImportError, error:
            error_list.append((module, error))
            print 'Some errors occured on', module, ':', error
    return error_list
if __name__ == '__main__':
    import time
    import Plugin
    def funcion(a,b):
        time.sleep(5)
        return a+b
    def callback( suma ):
        print suma
    f = Plugin.Function( funcion, callback )
    f( 2,3 )
    import gtk
    gtk.main()
