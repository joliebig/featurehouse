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
from logfile import ManagedStdio
from interfacemanager import InterfaceManager
if __name__ == '__main__':
    wpath.chdir(__file__)
misc.RenameProcess("wicd")
class WicdDaemon(dbus.service.Object):
    def __init__(self, bus_name, options, object_path="/org/wicd/daemon"):
        ''' Creates a new WicdDaemon object. '''
        dbus.service.Object.__init__(self, bus_name=bus_name, 
                                     object_path=object_path)
        self.interface_manager = InterfaceManager()
        self.options = options
        if not options.no_load_configuration:
            self.LoadConfiguration()
    @dbus.service.method('org.wicd.daemon')
    def GetVersion(self):
        """ Returns the version number. 
        This number is major-minor-micro. Major is the major version of Wicd.
        Minor in incremented if Wicd undergoes major changes. 
        Minor can be anything >= 0. Micro is for everything else, and micro
        may be anything >= 0. This number is effective starting wicd v1.2.0.
        """
        version = 'VPB 0.1.0'
        return version
    @dbus.service.method('org.wicd.daemon.interface')
    def GetInterfaces(self):
        ''' Updates the current state of the interfaces.
        In a wireless interface this might involve
        scanning for networks. In a wired network, this
        might involve check for a wire.
        '''
        return self.interface_manager.get_all_names()
    @dbus.service.method('org.wicd.daemon.interface')
    def CreateNewInterface(self, type, name):
        self.interface_manager.create(type, name)
    def _has_data(self, data_tuple):
        ''' Used to convert tuples sent over DBus to real tuples. '''
        if data_tuple in [('dbusdoesntallowemptytuples',), None, ()]:
            return None
        else:
            return data_tuple
    @dbus.service.method('org.wicd.daemon.interface')
    def GetInterfaceData(self, interface_name, method_name, data):
        ''' Gets the specified data from the specified interface. '''
        return self._interface_action(interface_name, method_name, data, 'get_')
    @dbus.service.method('org.wicd.daemon.interface')
    def SetInterfaceData(self, interface_name, method_name, data):
        ''' Sets the specified data on the specified interface. '''
        self._interface_action(interface_name, method_name, data, 'set_')
    @dbus.service.method('org.wicd.daemon.interface')
    def DoInterfaceAction(self, interface_name, method_name, data):
        ''' Runs the specified command on the specified interface. '''
        self._interface_action(interface_name, method_name, data, 'do_')
    def _interface_action(self, interface_name, method_name, data, prefix):
        ''' Runs a specified action on a specified method that starts with prefix. '''
        interface = self.interface_manager.get(interface_name)
        method = getattr(interface, prefix + method_name)
        print '%s interface action calling %s' % (prefix[:-1], method)
        if self._has_data(data):
            return method(*data)
        else:
            return method()
    @dbus.service.method('org.wicd.daemon')
    def SaveConfiguration(self):
        ''' Saves the state of the daemon. '''
        print 'saving configuration...'
        self.interface_manager.save()
    @dbus.service.method('org.wicd.daemon')
    def LoadConfiguration(self):
        ''' Loads the saved state of the daemon. '''
        print 'loading configuration...'
        self.interface_manager.load()
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
    global child_pid
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
    if (not options.no_stdout) or (not options.no_stderr):
        logpath = os.path.join(wpath.log, 'wicd.log')
        output = ManagedStdio(logpath)
        if os.path.exists(logpath):
            try:
                os.chmod(logpath, 0600)
            except:
                print 'unable to chmod log file to 0600'
    if not options.no_stdout: sys.stdout = output
    if not options.no_stderr: sys.stderr = output
    if options.add_current_dir_to_pythonpath:
        print 'adding',os.getcwd(),'to path'
        sys.path.insert(0, os.getcwd())
    print 'Wicd starting...'
    bus = dbus.SystemBus()
    wicd_bus = dbus.service.BusName('org.wicd.daemon', bus=bus)
    daemon = WicdDaemon(wicd_bus, options)
    gobject.threads_init()
    signal.signal(signal.SIGTERM, sigterm_caught)
    mainloop = gobject.MainLoop()
    mainloop.run()
def sigterm_caught(sig, frame):
    """ Called when a SIGTERM is caught, kills monitor.py before exiting. """
    global child_pid
    print 'SIGTERM caught, killing wicd-monitor...'
    os.kill(child_pid, signal.SIGTERM)
    print 'Removing PID file...'
    if os.path.exists(wpath.pidfile):
        os.remove(wpath.pidfile)
    print 'Shutting down...'
    sys.exit(0)
if __name__ == '__main__':
    if os.getuid() != 0:
        print ("Root priviledges are required for the daemon to run properly." +
               "  Exiting.")
        sys.exit(1)
    main(sys.argv)
