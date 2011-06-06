import types
from zope.interface import interface, providedBy
from twisted.pb import schema
class RemoteInterfaceClass(interface.InterfaceClass):
    """This metaclass lets RemoteInterfaces be a lot like Interfaces. The
    methods are parsed differently (PB needs more information from them than
    z.i extracts, and the methods can be specified with a RemoteMethodSchema
    directly).
    RemoteInterfaces can accept the following additional attribute:
     __remote_name__: can be set to a string to specify the globally-unique
                      name for this interface. This should be a URL in a
                      namespace you administer. If not set, defaults to the
                      fully qualified classname.
    RIFoo.names() returns the list of remote method names.
    RIFoo['bar'] is still used to get information about method 'bar', however
    it returns a RemoteMethodSchema instead of a z.i Method instance.
    """
    def __init__(self, iname, bases=(), attrs=None, __module__=None):
        if attrs is None:
            interface.InterfaceClass.__init__(self, iname, bases, attrs,
                                              __module__)
            return
        try:
            rname, remote_attrs = self._parseRemoteInterface(iname, attrs)
        except:
            raise
        interface.InterfaceClass.__init__(self, iname, bases, attrs,
                                          __module__)
        a = getattr(self, "_InterfaceClass__attrs") # the ickiest part
        a.update(remote_attrs)
        self.__remote_name__ = rname
        try:
            registerRemoteInterface(self, rname)
        except:
            raise
    def _parseRemoteInterface(self, iname, attrs):
        remote_attrs = {}
        remote_name = attrs.get("__remote_name__", iname)
        if attrs.has_key("__remote_name__"):
            del attrs["__remote_name__"]
        names = [name for name in attrs.keys()
                 if ((type(attrs[name]) == types.FunctionType and
                      not name.startswith("_")) or
                     schema.IConstraint.providedBy(attrs[name]))]
        for name in names:
            m = attrs[name]
            if not schema.IConstraint.providedBy(m):
                m = schema.RemoteMethodSchema(method=m)
            m.name = name
            m.interface = self
            remote_attrs[name] = m
            del attrs[name]
        return remote_name, remote_attrs
RemoteInterface = RemoteInterfaceClass("RemoteInterface",
                                       __module__="pb.flavors")
def getRemoteInterface(obj):
    """Get the (one) RemoteInterface supported by the object, or None."""
    interfaces = list(providedBy(obj))
    ilist = []
    for i in interfaces:
        if isinstance(i, RemoteInterfaceClass):
            if i not in ilist:
                ilist.append(i)
    assert len(ilist) <= 1, "don't use multiple RemoteInterfaces! %s" % (obj,)
    if ilist:
        return ilist[0]
    return None
class DuplicateRemoteInterfaceError(Exception):
    pass
RemoteInterfaceRegistry = {}
def registerRemoteInterface(iface, name=None):
    if not name:
        name = iface.__remote_name__
    assert isinstance(iface, RemoteInterfaceClass)
    if RemoteInterfaceRegistry.has_key(name):
        old = RemoteInterfaceRegistry[name]
        msg = "remote interface %s was registered with the same name (%s) as %s, please use __remote_name__ to provide a unique name" % (old, name, iface)
        raise DuplicateRemoteInterfaceError(msg)
    RemoteInterfaceRegistry[name] = iface
def getRemoteInterfaceByName(iname):
    return RemoteInterfaceRegistry.get(iname)
