"""Resource ui_psp (from file ui.psp)"""
source = 'ui.psp'
package = 'spambayes.resources'
import os
datafile = os.path.join(os.path.dirname(__file__), source)
data = open(datafile, "rb").read()
