"""Resource train_gif (from file train.gif)"""
source = 'train.gif'
package = 'spambayes.resources'
import os
datafile = os.path.join(os.path.dirname(__file__), source)
data = open(datafile, "rb").read()
