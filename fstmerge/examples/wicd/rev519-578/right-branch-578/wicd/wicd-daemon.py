""" wicd - wireless connection daemon implementation.
This module implements the wicd daemon that provides network
connection management, for both wireless and wired networks. The daemon
must be run as root to control the networks, however the user interface
components should be run as a normal user.
class WicdDaemon -- The main DBus daemon for Wicd.
def usage() -- Print usage information.
def daemonize() -- Daemonize the current process with a double fork.
def main() -- The wicd daemon main loop.
"""
import logging, logging.handlers
logging.basicConfig(level=logging.DEBUG,
                    format='%(levelname)-8s %(message)s',
                    datefmt='%Y%b%d %H:%M:%S')
logfile = logging.handlers.RotatingFileHandler('/var/log/wicd/wicd.log',
                                               maxBytes=1024*1024,
                                               backupCount=3)
logfile.setFormatter(
    logging.Formatter(
        '%(levelname)s:%(filename)s:%(funcName)s:%(lineno)d: %(message)s'
        )
    )
logging.getLogger().addHandler(logfile)
import sys, os, optparse, signal
import gobject
import dbus
import dbus.service
if getattr(dbus, 'version', (0, 0, 0)) < (0, 80, 0):
    import dbus.glib
else:
    from dbus.mainloop.glib import DBusGMainLoop
    DBusGMainLoop(set_as_default=True)
import wpath
import misc
from misc import WicdError
from interfacemanager import InterfaceManager
from pluginmanager import PluginManager
from wglobals import global_config
if __name__ == '__main__':
    wpath.chdir(__file__)
misc.rename_process("wicd")
class WicdDaemon(dbus.service.Object):
    def __init__(self, bus_name, options, object_path="/org/wicd/daemon"):
        ''' Creates a new WicdDaemon object. '''
        dbus.service.Object.__init__(self, bus_name=bus_name,
                                     object_path=object_path)
        self.options = options
        self.interface_manager = InterfaceManager(self.StatusChange,
                                                  self.StateChange)
        self.plugin_manager = PluginManager(self)
        import wglobals
        wglobals.daemon = self
        wglobals.plugin_manager = self.plugin_manager
        if not options.no_load_configuration:
            self.LoadConfiguration()
        self.plugin_manager.action('start')
    @dbus.service.method('org.wicd.daemon')
    def GetVersion(self):
        """ Returns the version number. """
        version = 'VeryPluggableBackends SVN'
        return version
    @dbus.service.method('org.wicd.daemon.interface')
    def ListInterfaces(self):
        ''' Returns the names of all the interfaces. '''
        data = self.interface_manager.get_all_names()
        if data in [(), [], {}]:
            data = ('',)
        return data
    @dbus.service.method('org.wicd.daemon.interface')
    def CreateInterface(self, interface_type, interface_name):
        self.interface_manager.create(interface_type, interface_name)
    @dbus.service.method('org.wicd.daemon.interface')
    def DeleteInterface(self, interface_name):
        self.interface_manager.delete(interface_name)
    def _has_data(self, data_tuple):
        ''' Used to convert tuples sent over DBus to real tuples. '''
        if data_tuple in [('dbusdoesntallowemptytuples',), None, ()]:
            return None
        else:
            return data_tuple
    @dbus.service.method('org.wicd.daemon.interface')
    def GetInterfaceData(self, interface_name, method_name, data):
        ''' Gets the specified data from the specified interface. '''
        data = self.interface_action(interface_name, method_name, data, 'get_')
        if data in [(), [], {}]:
            data = ('dbusdoesntallowemptytuples',)
        logging.debug( 'returning %s', data )
        return data
    @dbus.service.method('org.wicd.daemon.interface')
    def SetInterfaceData(self, interface_name, method_name, data):
        ''' Sets the specified data on the specified interface. '''
        self.interface_action(interface_name, method_name, data, 'set_')
    @dbus.service.method('org.wicd.daemon.interface')
    def DoInterfaceAction(self, interface_name, method_name, data):
        ''' Runs the specified command on the specified interface. '''
        self.interface_action(interface_name, method_name, data, 'do_')
    def interface_action(self, interface_name, method_name, data, prefix=''):
        ''' Runs a specified action on a specified method that starts with prefix. '''
        if not self.interface_manager.exists(interface_name):
            raise WicdError('Specified interface does not exist')
        interface = self.interface_manager.get(interface_name)
        if not hasattr(interface, (prefix + method_name)):
            raise WicdError('%s: method does not exist' % (prefix + method_name))
        self.plugin_manager.action('starting_action', (interface_name,
                                                      (prefix + method_name),
                                                      self._has_data(data)))
        method = getattr(interface, prefix + method_name)
        logging.debug( '%s interface action calling %s' % (prefix[:-1], method) )
        return_data = None
        if self._has_data(data):
            return_data = method(*data)
        else:
            return_data = method()
        self.plugin_manager.action('finished_action', (interface_name,
                                                      (prefix + method_name),
                                                      self._has_data(data),
                                                      return_data))
        return return_data
    @dbus.service.method('org.wicd.daemon.interface')
    def GetInterfaceActions(self, interface_name):
        if not self.interface_manager.exists(interface_name):
            raise WicdError('Specified interface does not exist')
        interface = self.interface_manager.get(interface_name)
        methods = [ method
                    for method in dir(interface)
                    if method.startswith('get_') or
                    method.startswith('set_') or
                    method.startswith('do_') ]
        return methods
    @dbus.service.method('org.wicd.daemon')
    def SaveConfiguration(self):
        ''' Saves the state of the daemon. '''
        logging.debug( 'saving configuration...' )
        self.interface_manager.save()
    @dbus.service.method('org.wicd.daemon')
    def LoadConfiguration(self):
        ''' Loads the saved state of the daemon. '''
        logging.debug( 'loading configuration...' )
        self.interface_manager.load()
    @dbus.service.signal('org.wicd.daemon')
    def StatusChange(self, interface_name, previous_status, status):
        logging.debug( '%s: status changed from %s to %s', interface_name, previous_status, status)
        self.plugin_manager.action('signal_%s' % status, (previous_status, interface_name))
        self.plugin_manager.action('signal_%s_from_%s' % (status, previous_status), (interface_name, ))
    @dbus.service.signal('org.wicd.daemon')
    def StateChange(self, interface_name, state):
        logging.debug( '%s: status changed to %s', interface_name, state)
        self.plugin_manager.action('state_%s' % state, (interface_name, ))
    @dbus.service.signal('org.wicd.daemon')
    def Closing(self):
        logging.debug('Daemon shutting down...')
        self.plugin_manager.action('closing')
def daemonize():
    """ Disconnect from the controlling terminal.
    Fork twice, once to disconnect ourselves from the parent terminal and a
    second time to prevent any files we open from becoming our controlling
    terminal.
    For more info see:
    http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/66012
    """
    try:
        pid = os.fork()
        if pid > 0:
            sys.exit(0)
    except OSError, e:
        print >> sys.stderr, "Fork #1 failed: %d (%s)" % (e.errno, e.strerror)
        sys.exit(1)
    os.setsid()
    os.umask(0)
    try:
        pid = os.fork()
        if pid > 0:
            dirname = os.path.dirname(wpath.pidfile)
            if not os.path.exists(dirname):
                os.makedirs(dirname)
            pidfile = open(wpath.pidfile, 'w')
            pidfile.write(str(pid) + '\n')
            pidfile.close()
            sys.exit(0)
    except OSError, e:
        print >> sys.stderr, "Fork #2 failed: %d (%s)" % (e.errno, e.strerror)
        sys.exit(1)
    sys.stdout.flush()
    sys.stderr.flush()
    os.close(sys.__stdin__.fileno())
    os.close(sys.__stdout__.fileno())
    os.close(sys.__stderr__.fileno())
    sys.stdin = open('/dev/null', 'r')
def main(argv):
    """ The main daemon program.
    Keyword arguments:
    argv -- The arguments passed to the script.
    """
    do_daemonize = True
    redirect_stderr = True
    redirect_stdout = True
    auto_connect = True
    p = optparse.OptionParser()
    p.add_option('--no-daemon', '-f', action='store_true')
    p.add_option('--no-stdout', '-o', action='store_true')
    p.add_option('--no-stderr', '-e', action='store_true')
    p.add_option('--no-autoconnect', '-a', action='store_true')
    p.add_option('--no-load-configuration', '-n', action='store_true')
    p.add_option('--add-current-dir-to-pythonpath', '-p', action='store_true')
    options, arguments = p.parse_args()
    if not options.no_daemon: daemonize()
    logpath = os.path.join(wpath.log, 'wicd.log')
    if os.path.exists(logpath):
        try:
            os.chmod(logpath, 0600)
        except:
            logging.debug( 'unable to chmod log file to 0600' )
    if options.add_current_dir_to_pythonpath:
        logging.debug( 'adding %s to path', os.getcwd())
        sys.path.insert(0, os.getcwd())
    logging.info( 'Wicd starting...' )
    bus = dbus.SystemBus()
    wicd_bus = dbus.service.BusName('org.wicd.daemon', bus=bus)
    global daemon
    daemon = WicdDaemon(wicd_bus, options)
    gobject.threads_init()
    signal.signal(signal.SIGTERM, sigterm_caught)
    mainloop = gobject.MainLoop()
    mainloop.run()
def shut_down():
    try:
        global child_pid, daemon
        logging.debug( 'Sending Closing signal...' )
        daemon.Closing()
        daemon.SaveConfiguration()
        logging.debug( 'Removing PID file...')
        if os.path.exists(wpath.pidfile):
            os.remove(wpath.pidfile)
        logging.debug( 'Shutting down...')
    except Exception, e:
        logging.error('Error occurred while shutting down:')
        logging.error('%s', e)
def sigterm_caught(sig, frame):
    """ Called when a SIGTERM is caught, causes the daemon to exit nicely. """
    shut_down()
    sys.exit(0)
if __name__ == '__main__':
    if os.getuid() != 0:
        logging.critical( "Root priviledges are required for the daemon to run properly." +
               "  Exiting." )
        sys.exit(1)
    signal.signal(signal.SIGTERM, sigterm_caught)
    try:
        main(sys.argv)
    finally:
        shut_down()
