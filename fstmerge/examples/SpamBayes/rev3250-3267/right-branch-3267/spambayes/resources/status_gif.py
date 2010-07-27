"""Resource status_gif (from file status.gif)"""
source = 'status.gif'
package = 'spambayes.resources'
import os
datafile = os.path.join(os.path.dirname(__file__), source)
data = open(datafile, "rb").read()
