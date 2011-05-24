"""Resource message_gif (from file message.gif)"""

source = 'message.gif'

package = 'spambayes.resources'

import os

datafile = os.path.join(os.path.dirname(__file__), source)

data = open(datafile, "rb").read()



