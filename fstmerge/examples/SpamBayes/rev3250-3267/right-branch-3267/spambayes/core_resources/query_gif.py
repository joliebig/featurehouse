"""Resource query_gif (from file query.gif)"""
source = 'query.gif'
package = 'spambayes.resources'
import os
datafile = os.path.join(os.path.dirname(__file__), source)
data = open(datafile, "rb").read()
