'''Handles the Plugin Dialog: starts/stops plugins, show description, ect.'''
import gtk
import pango
import dialog
class PluginManagerDialog( gtk.Window ):
    '''This class show and manage all the plugins installed
    in the plugins directory'''
    def __init__( self, pluginManager, parent, config ):
        '''Constructor'''
        gtk.Window.__init__( self )
        self.config = config
        self.pluginManager = pluginManager
        self.set_transient_for( parent )
        self.set_type_hint( gtk.gdk.WINDOW_TYPE_HINT_DIALOG )
        self.set_default_size( 500, 350 )
        self.set_title( _( 'Plugin Manager' ) )
        self.set_role( _( 'Plugin Manager' ) )
        self.set_border_width( 12 )
        self.set_resizable( True )
        vbox = gtk.VBox()
        vbox.set_spacing( 12 )
        self.listStore = gtk.ListStore( str, str, str, bool )
        cellRendererTextName = gtk.CellRendererText()
        cellRendererToggle = gtk.CellRendererToggle()
        cellRendererTextDesc = gtk.CellRendererText()
        cellRendererToggle.set_property( 'activatable', True )
        plugOn = gtk.TreeViewColumn(_('Active'), cellRendererToggle, active=3)
        plugOn.set_resizable( False )
        plugName = gtk.TreeViewColumn(_('Name'), cellRendererTextName, markup=0)
        plugName.add_attribute( cellRendererTextName, 'text', 0 )
        plugName.set_resizable( True )
        plugName.set_min_width( 60 )
        plugDesc = gtk.TreeViewColumn(_('Description'), cellRendererTextDesc)
        cellRendererTextDesc.set_property( 'ellipsize', pango.ELLIPSIZE_END )
        plugDesc.add_attribute( cellRendererTextDesc, 'text', 2 )
        plugDesc.set_expand( True )
        plugDesc.set_sizing( gtk.TREE_VIEW_COLUMN_FIXED )
        self.list = gtk.TreeView( self.listStore )
        self.list.append_column( plugOn )
        self.list.append_column( plugName )
        self.list.append_column( plugDesc )
        self.list.set_reorderable( False )
        self.list.set_headers_visible( True )
        self.list.set_rules_hint( True )
        self.scroll = gtk.ScrolledWindow()
        self.scroll.set_policy( gtk.POLICY_NEVER , gtk.POLICY_AUTOMATIC )
        self.scroll.set_shadow_type( gtk.SHADOW_OUT )
        self.scroll.add( self.list )
        table = gtk.Table( 3, 2 )
        table.set_row_spacings( 6 )
        table.set_col_spacings( 12 )
        hButBox = gtk.HButtonBox()
        hButBox.set_spacing( 6 )
        hButBox.set_layout( gtk.BUTTONBOX_END )
        self.description = gtk.Label() 
        self.author = gtk.Label() 
        self.web = gtk.Label() 
        self.description.set_alignment( 0.0, 0.0 )
        self.author.set_alignment( 0.0, 0.5 )
        self.web.set_alignment( 0.0, 0.5 )
        self.description.set_line_wrap( True )
        self.author.set_line_wrap( True )
        self.web.set_line_wrap( True )
        self.description.set_size_request( 476, 48 )
        auth = gtk.Label( '<b>' + _( 'Author:' ) + '</b>' )
        auth.set_alignment( 0.0, 0.5 )
        auth.set_use_markup( True )
        web = gtk.Label( '<b>' + _( 'Website:' ) + '</b>' )
        web.set_alignment( 0.0, 0.5 )
        web.set_use_markup( True )
        table.attach( self.description , 0, 2, 0, 1, gtk.FILL | gtk.EXPAND )
        table.attach( auth, 0, 1, 1, 2, gtk.FILL )
        table.attach( self.author , 1, 2, 1, 2 )
        table.attach( web, 0, 1, 2, 3, gtk.FILL )
        table.attach( self.web , 1, 2, 2, 3 )
        self.bConfigure = gtk.Button( _( 'Configure' ), gtk.STOCK_PROPERTIES )
        self.bReload = gtk.Button( stock=gtk.STOCK_REFRESH )
        self.bLoadNew = gtk.Button( _( 'Load New Plugins' ))
        self.bclose = gtk.Button( stock=gtk.STOCK_CLOSE )
        self.bConfigure.connect( 'clicked', self.configurePlugin )
        self.bReload.connect( 'clicked', self.reloadPlugin )
        self.bLoadNew.connect( 'clicked', self.loadNewPlugins )
        self.bclose.connect( 'clicked', self.close )
        hButBox.pack_start( self.bConfigure )
        hButBox.pack_start( self.bReload )
        hButBox.pack_start( self.bLoadNew )
        hButBox.pack_start( self.bclose )
        vbox.pack_start( self.scroll, True, True )
        vbox.pack_start( table, False, False )
        vbox.pack_start( hButBox, False, False )
        vbox.show_all()
        self.add( vbox )
        self.fillList()
        self.list.columns_autosize()
        self.connect( 'delete-event', self.close )
        self.list.connect( 'cursor-changed', self.row_selected )
        cellRendererToggle.connect( 'toggled', self.active_toggled, \
            ( self.listStore, 3 ) )
    def fillList( self ):
        '''fills the plugin list'''
        self.listStore.clear()
        for i in sort( self.pluginManager.getPlugins() ):
            plugin = self.pluginManager.getPluginData( i )
            if plugin:
                self.listStore.append(['<b>' + plugin['displayName'] + '</b>',
                    i, plugin['description'], self.pluginManager.isEnabled(i)])
    def close( self, *args ):
        '''close the window'''
        self.destroy()
    def getSelected( self ):
        '''Return the selected item'''
        model = self.list.get_model()
        selected = self.list.get_selection().get_selected()
        if selected[1]:
            return model.get(selected[1], 1)[0]
        else:
            return None
    def row_selected( self, *args ):
        '''callback for the row_selected event'''
        name = self.getSelected()
        plugin_instance = self.pluginManager.getPlugin(name)
        self.setDescription(name)
        if hasattr(plugin_instance, "configure") and callable(plugin_instance.configure):
            self.bConfigure.set_sensitive(True)
        else:
            self.bConfigure.set_sensitive(False)
    def configurePlugin( self, *args ):
        '''call the configure method in the plugin'''
        plugin_instance = self.pluginManager.getPlugin( self.getSelected() )
        if hasattr(plugin_instance, "configure") and callable(plugin_instance.configure):
            plugin_instance.configure()
    def reloadPlugin( self, *args ):
        '''Reloads the plugin python code'''
        name = self.getSelected()
        self.pluginManager.restartPlugin(name)
        self.fillList()
        self.setDescription(name)
    def loadNewPlugins( self, *args ):
        '''Loads new plugins'''
        for i in self.pluginManager.getNewModules():
            if i not in self.pluginManager.plugin_data:
                self.pluginManager.inspectPlugin(i)
            self.pluginManager.loadPlugin(i)
        self.fillList()
    def setDescription( self, name ):
        plugin_data = self.pluginManager.getPluginData(name)
        if plugin_data:
            self.description.set_label( plugin_data['description'] )
            string = ''
            for (author,mail) in plugin_data['authors'].iteritems():
                string += author + ' (' + mail + ')\n'
            self.author.set_label( '' + string[ :-1 ] )
            self.web.set_label( '' + plugin_data['website'] )
    def active_toggled(self, cell, path, user_data):
        '''enable or disable the plugin'''
        model, column = user_data
        iterator= model.get_iter_from_string(path)
        plugin_name = model.get_value(iterator, 1)
        plugin_instance = self.pluginManager.getPlugin(plugin_name, True)
        if not plugin_instance:
            return
        if self.pluginManager.isEnabled(plugin_name):
            self.pluginManager.stopPlugin(plugin_name)
            model.set_value(iterator, column, False)
        plugins = self.config.user['activePlugins'].split( ',' )
        if plugin_name in plugins:
            plugins.pop( plugins.index(plugin_name))
            self.config.user['activePlugins'] = ','.join(plugins)
        else:
            status = self.pluginManager.checkPlugin(plugin_name)
            if status[0]:
                self.pluginManager.startPlugin(plugin_name)
                model.set_value(iterator, column, True)
                plugins = self.config.user['activePlugins'].split(',')
                if plugins[0] == '' and len(plugins) == 1:
                    self.config.user['activePlugins'] = plugin_name
                elif not plugin_name in plugins:
                    s = ','.join(plugins) + ',' + plugin_name
                    self.config.user['activePlugins'] = s
            else:
                dialog.error(_('Plugin initialization failed: \n') \
                    + status[1])
def sort(l):
    '''Sorts a list in alphabetical order
    without regard of the capitalization'''
    l = l[:]
    l.sort(key=lambda x: str.lower(x))
    return l
