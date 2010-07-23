from dbusmanager import DBusManager
import optparse
import gobject
from logfile import log
dbus_manager = DBusManager()
dbus_manager.connect_to_dbus()
dbus_ifaces = dbus_manager.get_dbus_ifaces()
p = optparse.OptionParser()
p.add_option('--load-configuration', '', action='store_true')
p.add_option('--save-configuration', '', action='store_true')
p.add_option('--create-interface', '', action='store_true')
p.add_option('--update', '', action='store_true')
p.add_option('--list-interfaces', '', action='store_true')
p.add_option('--get-data', '-g', default='')
p.add_option('--set-data', '-s', default='')
p.add_option('--do-action', '-p', default='')
p.add_option('--data', '-d', default=('dbusdoesntallowemptytuples',))
p.add_option('--name', '-n', default='')
p.add_option('--interface-name', '-i', default='')
p.add_option('--interface-type', '-t', default='')
p.add_option('--listen-for-signal', '-l', action='store_true')
p.add_option('--ping', '', action='store_true')
options, arguments = p.parse_args()
def _has_data(data_tuple):
    ''' Used to convert tuples sent over DBus to real tuples. '''
    if data_tuple in [('dbusdoesntallowemptytuples',), None, (), 'dbusdoesntallowemptytuples']:
        return None
    else:
        return data_tuple
if not options.data == ('dbusdoesntallowemptytuples',):
    split = options.data.split('|')
    options.data = tuple(split)    
def avg(iterable):
    return float(sum(iterable))/float(len(iterable))
if options.ping:
    import time
    daemon = dbus_ifaces['daemon']
    times = []
    try:
        while True:
            start = time.time()
            daemon.GetVersion()
            delta = time.time() - start
            log( 'Reponse time: %s' % delta)
            times.append(delta)
            time.sleep(0.1)
    except KeyboardInterrupt:
        raise
    finally:
        log( 'min: %s max: %s avg: %s' % (min(times), max(times), avg(times)))
elif options.listen_for_signal:
    def signal_change(*args):
        log( 'Signal %s: ' % options.name, args)
    dbus_manager.connect_to_signal(options.name, signal_change)
    mainloop = gobject.MainLoop()
    mainloop.run()
if options.load_configuration:
    dbus_ifaces['daemon'].LoadConfiguration()
elif options.save_configuration:
    dbus_ifaces['daemon'].SaveConfiguration()
elif options.create_interface:
    dbus_ifaces['interface'].CreateInterface(options.interface_type,
                                            options.interface_name)
elif options.update:
    dbus_ifaces['interface'].UpdateInterfaces()
elif options.list_interfaces:
    for interface in dbus_ifaces['interface'].ListInterfaces():
        log( interface)
elif options.get_data:
    log( _has_data(dbus_ifaces['interface'].GetInterfaceData(options.interface_name,
                                                       options.get_data,
                                                       options.data)))
elif options.set_data:
    dbus_ifaces['interface'].SetInterfaceData(options.interface_name,
                                                       options.set_data,
                                                       options.data)
elif options.do_action:
    dbus_ifaces['interface'].DoInterfaceAction(options.interface_name,
                                                       options.do_action,
                                                       options.data)
