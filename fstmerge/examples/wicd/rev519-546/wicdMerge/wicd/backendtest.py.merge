from backend import BackendManager
print 'Initalizing backend manager...'
bm = BackendManager()
print 'Loading all available backends...'
bm.load_all_available_backends()
print 'Listing loaded backends...'
bends = bm.get_loaded_backends()
for type, backend in bends.iteritems():
    print type, backend.find_available_interfaces()

