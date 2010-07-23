from baseinterface import BaseInterface
import os
class BackendWiredInterface(BaseInterface):
    @staticmethod
    def get_type():
        return 'wired'
    @staticmethod
    def find_available_interfaces():
        """
        Static method. Returns a list of the interfaces of this type.
        Accepts:
        Nothing
        Returns:
        A list with zero or more strings of the names of interfaces.
        These names can be passed to __init__ to instantiate an object.
        """
        basedir = '/sys/class/net/'
        return [iface for iface in os.listdir(basedir) if not 'wireless' 
            in os.listdir(basedir + iface) and 
            open(basedir + iface + "/type").readlines()[0].strip() == "1"]
    class XmlUiEventHandler(object):
        """ A class to handle the UI events. """
        def connect(self, values):
            print 'Connect clicked', values
    def __init__(self, interface_name, status_change_callback):
        BaseInterface.__init__(self, interface_name, status_change_callback)
        if not interface_name in self.find_available_interfaces():
            raise self.CannotCreateInterfaceException()
        self.name = 'Wired Interface %s' % self.interface_name
        self.status_change_callback('idle')
    def get_status(self):
        return "Wired Interface. Connected %s" % self.get_connected_to_network()
