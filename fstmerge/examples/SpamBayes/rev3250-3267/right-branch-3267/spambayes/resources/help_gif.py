"""Resource help_gif (from file help.gif)"""
source = 'help.gif'
package = 'spambayes.resources'
import os
datafile = os.path.join(os.path.dirname(__file__), source)
data = open(datafile, "rb").read()
