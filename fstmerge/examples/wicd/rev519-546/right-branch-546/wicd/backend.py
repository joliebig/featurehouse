""" Backend manager for wicd.
Manages and loads the pluggable backends for wicd.
"""
import sys
import os
import wicd.wpath as wpath
from baseinterface import BaseInterface
class BackendManager(object):
    def __init__(self):
        """ Initialize the backend manager. """
        self.backend_dir = "backends" 
        self.__loaded_backends = {}
    def _valid_backend(self, be_dir):
        """ Make sure the backend is valid. """
        access = os.access(be_dir, os.F_OK)
        isdir = os.path.isdir(be_dir)
        starts_with_be = os.path.basename(be_dir).startswith('be-')
        return access and isdir and starts_with_be
    def get_loaded_backends(self):
        if self.__loaded_backends and not self.__loaded_backends is None:
            return self.__loaded_backends
        else:
            return None
    def get_backend_by_type(self, type):
        return self.__loaded_backends[type]
    def get_available_backend_modules(self):
        """ Returns a list of all valid backends in the backend directory. """
        be_list = []
        for f in os.listdir(self.backend_dir):
            if self._valid_backend(os.path.join(self.backend_dir, f)):
                be_list.append(f[3:])
        return be_list
    def load_all_available_backends(self):
        for backend in self.get_available_backend_modules():
            print 'loading backend',backend
            self.load_backend(backend)
    def load_backend(self, backend_name):
        """ Load and return a backend module. 
        Given a backend name be-foo, attempt to load a python module
        in the backends directory called be-foo.  The module must
        include a certain set of classes and variables to be considered
        valid.
        """
        def fail(backend_name, reason):
            print "failed to load backend %s: %s" % (backend_name, reason)
        print 'trying to load backend %s' % backend_name
        backend_path = os.path.join(self.backend_dir,
                                    'be-' + backend_name)
        if self._valid_backend(backend_path):
            sys.path.insert(0, self.backend_dir)
            print backend_name
            backend = __import__('be-' + backend_name)
        else:
            fail(backend_name, 'invalid backend file.')
            return None
        new_backends = [ i for i in dir(backend.interface) if i.startswith('Backend') ]
        for backend_class_name in new_backends:
            backend_class = getattr(backend.interface, backend_class_name)
            if issubclass(backend_class, BaseInterface):
                self.__loaded_backends[backend_class.get_type()] = backend_class
                print 'successfully loaded backend %s' % backend_class.__name__
            else:
                fail(backend_class, 'does not subclass BaseInterface')
if __name__ == "__main__":
    print "main"
    be = BackendManager()
    print be.get_available_backend_modules()
    be.load_all_available_backends()
    print be.get_loaded_backends()
