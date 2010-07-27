"""Resource classify_gif (from file classify.gif)"""

source = 'classify.gif'

package = 'spambayes.resources'

import os

datafile = os.path.join(os.path.dirname(__file__), source)

data = open(datafile, "rb").read()



