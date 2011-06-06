'''Handles plugins. Loads/unloads them, starts/stops'''
import os
import sys
import paths
import emesenelib.common
class PluginManager( object ):
    '''This module handles the plugins'''
    def __init__( self, controller ):
        '''Constructor'''
        self.userPlugins = []
        self.systemPlugins = []
        self.controller = controller
        self.plugin_data = {} #Meta-data of plugins
        self.loaded_plugin = {}#Instances of plugins
        self.scanPlugins()
        for i in self.getModules():
            self.inspectPlugin( i ) 
    def scanPlugins(self):
        '''updates userPlugins and systemPlugins'''
        userPlugins = {}
        if os.name == 'posix':
            sys.path.append(paths.CONFIG_DIR)
            userPlugins = self.inspectPackage(paths.CONFIG_DIR, 
                    paths.PLUGINS_HOME)
        systemPlugins = self.inspectPackage(paths.APP_PATH, 
                paths.PLUGINS_SYSTEM_WIDE)
        for i in userPlugins.keys():
            if not i in systemPlugins:
                continue
            elif not userPlugins[i]:
                del userPlugins[i]
            elif not systemPlugins[i]:
                del systemPlugins[i]
            elif self.comparePluginVersions(userPlugins[i], 
                                             systemPlugins[i] ):
                del systemPlugins[i]
            else:
                del userPlugins[i]
        self.userPlugins = userPlugins.keys()
        self.systemPlugins = systemPlugins.keys()
    def inspectPackage(self, path, package):
        '''Searches the given path for plugins in package.
        Returns a dict with the found plugins and their versions, if any.'''
        try:
            __import__(package, globals(), locals(), [])
        except:
            print 'LO QUE!!!'
        path = path + paths.DIR_SEP + package
        modules = [ x.split( '.' )[ 0 ] for x in os.listdir( path ) \
                    if x.endswith( '.py' ) and not x.startswith( '_' ) \
                    and x != 'Plugin.py' ]
        plugins = {}
        plugins = plugins.fromkeys( modules )
        for i in modules:
            try:
                mod = __import__( package + '.' + i, globals(), \
                                  None, [ 'VERSION' ] )
                plugins[i] = getattr( mod, 'VERSION' )
            except AttributeError:
                pass
            except Exception, e:
                del plugins[i]
                print('Exception importing %s\n%s'
                % (i, str(e)), 'PluginManager')
        return plugins
    def comparePluginVersions( self, v1, v2 ):
        '''Compare versions of the format 'x.y.z'
        Returns True if v1 and v2 can be compared and v1>v2, False otherwise '''
        v1 = v1.strip().split('.')
        v2 = v2.strip().split('.')
        try:
            for i in range( min( len( v1 ), len( v2 ) ) ):
                if int( v1[ i ] ) > int( v2[ i ] ):
                    return True
                if int( v1[ i ] ) < int( v2[ i ] ):
                    return False
        except:
            return False
        if len( v1 ) > len( v2 ):
            return True
        else:
            return False
    def getModules(self):
        '''Returns a list with the plugins full module names  on the form:
        module_name.plugin_name'''
        user = [ paths.PLUGINS_HOME + '.' + x for x in self.userPlugins ]
        system = [paths.PLUGINS_SYSTEM_WIDE + '.' + x 
                for x in self.systemPlugins]
        user.extend( system )
        return user
    def getPlugins(self):
        '''Returns a list with the plugins names'''
        return self.plugin_data.keys()
    def pluginToModuleName(self, plugin):
        '''Converts a plugin name into a module name with full path'''
        if plugin in self.systemPlugins:
            return paths.PLUGINS_SYSTEM_WIDE + '.' + plugin
        elif plugin in self.userPlugins:
            return paths.PLUGINS_HOME + '.' + plugin
        else:
            return ''
    def inspectPlugin( self, module, doReload=False ):
        '''Loads plugin metadata without instanciating it'''
        name = module.split('.')[ 1 ]
        if module in self.getModules():
            try:
                mod = __import__( module, globals(), locals(), [] )
                plugin = getattr( mod, name )
                plugin_class = plugin.MainClass
                if doReload:
                    reload(mod)
                data = {'module':module, 'plugin':plugin}
                for field in ['name', 'displayName', 'description',
                        'authors', 'website']:
                    data[field] = getattr(plugin_class, field)
            except Exception, e:
                print 'Plugin ' + name + ' could not be inspected'
                print 'Reason: ' + str(e)
            else:
                self.plugin_data[name] = data
    def loadPlugin( self, name, doReload=False ):
        '''Instanciate an object of the plugin, the possible exceptions should
        be handled by the caller.'''
        if name not in self.plugin_data:
            return False
        if doReload:
            self.inspectPlugin(self.plugin_data[name]['module'], True)
        plugin = self.plugin_data[name]['plugin']
        try:
            self.loaded_plugin[ name ] = plugin.MainClass( self.controller, \
                self.controller.msn)
        except Exception, e:
            print 'Plugin ' + name + ' could not be initialized'
            print 'Reason: ' + str( e  )
    def restartPlugin(self, name):
        '''reload a plugin and restart it if it was enabled'''
        plugin_instance = self.getPlugin(name)
        if plugin_instance and self.isEnabled(name):
            was_enabled = True
            self.stopPlugin(name)
        else:
            was_enabled = False
        self.loadPlugin(name, True)
        plugin_instance = self.getPlugin(name)
        if plugin_instance and was_enabled:
            self.startPlugin(name)
    def checkPlugin( self, name ):
        '''check if the plugin can be initialized'''
        if name in self.loaded_plugin:
            return self.loaded_plugin[ name ].check()
        else:
            return [ False, 'The plugin isn\'t loaded' ]
    def startPlugin( self, name ):
        '''start the plugin, call check before this!'''
        if not name in self.loaded_plugin:
            if name in self.plugin_data:
                self.loadPlugin(name)
            else:
                return False
        if name in self.loaded_plugin:
            self.loaded_plugin[ name ].start()
            return True
        else:
            return False
    def startActivePlugins(self):
        '''Start the plugins in pluginList'''
        plugins = self.controller.config.user['activePlugins'].split(',')
        for name in plugins:
            if name == '':
                continue
            if name not in self.loaded_plugin:
                self.loadPlugin(name)
            try:
                (success, message) = self.checkPlugin(name)
            except:
                success = False
                message = _('invalid check() return value')
            if not success:
                emesenelib.common.debug(
                    _('plugin %s could not be initialized, reason:') % name)
                emesenelib.common.debug(message)
            else:
                self.startPlugin(name)
    def stopPlugin(self, name):
        '''stop the plugin'''
        if name in self.loaded_plugin:
            self.loaded_plugin[ name ].stop()
            self.unloadPlugin(name)
            return True
        else:
            return False
    def unloadPlugin(self, name):
        '''delete a plugin from loaded_plugin. 
        Should reduce ram usage when deactivating a plugin.'''
        if name in self.loaded_plugin:
            del self.loaded_plugin[name]
    def getNewModules(self):
        '''finds new modules'''
        self.scanPlugins()
        return [x for x in self.getModules()
                if not x.split('.')[1] in self.plugin_data]
    def getPlugin( self, name, autoLoad=False ):
        '''return the plugin if exist or None.
        If autoLoad is true, will load plugin'''
        if name not in self.loaded_plugin and autoLoad:
            self.loadPlugin(name)
        if name in self.loaded_plugin:
            return self.loaded_plugin[ name ]
        else:
            return None
    def getPluginData( self, name ):
        '''return the plugin if exist or None'''
        if name in self.plugin_data:
            return self.plugin_data[ name ]
        else:
            return None
    def destroy( self ):
        '''Stops all the running plugins and cleans references'''
        for i in self.loaded_plugin.keys():
            try:
                self.loaded_plugin[ i ].stop()
                self.unloadPlugin(i)
            except:
                emesenelib.common.debug('Plugin ' + self.loaded_plugin[i].name +
                    ' failed to stop')
    def isEnabled( self, name ):
        '''checks if plugin called "name" is enabled'''
        if name in self.plugin_data and name in self.loaded_plugin:
            return self.loaded_plugin[name].enabled
        else:
            return False
