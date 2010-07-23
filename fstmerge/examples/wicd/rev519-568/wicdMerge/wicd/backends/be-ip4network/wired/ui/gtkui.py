import sys, os

import logging

sys.path.insert(0, os.path.abspath(
    os.path.join(os.path.dirname(
        os.path.realpath(__file__)
        ),
                 '../..')))

logging.debug(sys.path[0])

from gtkuibase import ShortInterfaceUiBase

sys.path.pop(0)



