import dbus
from dbusmanager import set, get, do, prepare_data_for_dbus, interface
from misc import WicdError
import logging
class ProxyInterface(object):
    def __init__(self, interface_name):
        if not interface_name in interface.ListInterfaces():
            raise ValueError('No interface of this name exists.')
        self.interface_name = interface_name
        self.methods = interface.GetInterfaceActions(interface_name)
    def __str__(self):
        return '<Proxy Interface %s>' % self.interface_name
    def __getattr__(self, name):
        access_method = None
        method_name = None
        methods = {
            'do' : do,
            'set' : set,
            'get' : get
            }
        if name in self.methods:
            access_method = methods[name[:name.index('_')]]
            method_name = name[name.index('_')+1:]
        else:
            raise AttributeError('%s: no such method: %s' %
                                 (self.interface_name, name))
        def proxy_method(*args):
            logging.debug('%s %s %s %s' % (method, self.interface_name, method_name, args))
            try:
                return access_method(self.interface_name, method_name, prepare_data_for_dbus(args))
            except WicdError, e:
                raise AttributeError(e)
        method = proxy_method
        method.func_name = name
        return method
