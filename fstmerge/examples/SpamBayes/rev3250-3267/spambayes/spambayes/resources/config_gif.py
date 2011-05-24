"""Resource config_gif (from file config.gif)"""

source = 'config.gif'

package = 'spambayes.resources'

import os

datafile = os.path.join(os.path.dirname(__file__), source)

data = open(datafile, "rb").read()



