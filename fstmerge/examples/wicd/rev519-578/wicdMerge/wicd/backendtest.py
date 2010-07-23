from backend import BackendManager

import logging

logging.debug( 'Initalizing backend manager...')

bm = BackendManager()

logging.debug( 'Loading all available backends...')

bm.load_all_available_backends()

logging.debug( 'Listing loaded backends...')

bends = bm.get_loaded_backends()

for type, backend in bends.iteritems():

    logging.debug( type, backend.find_available_interfaces())



