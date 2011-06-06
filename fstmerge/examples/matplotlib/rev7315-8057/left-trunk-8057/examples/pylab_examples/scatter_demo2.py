"""
make a scatter plot with varying color and size arguments
"""
import matplotlib
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import matplotlib.cbook as cbook
datafile = cbook.get_sample_data('goog.npy')
r = np.load(datafile).view(np.recarray)
r = r[-250:] # get the most recent 250 trading days
delta1 = np.diff(r.adj_close)/r.adj_close[:-1]
volume = (15*r.volume[:-2]/r.volume[0])**2
close = 0.003*r.close[:-2]/0.003*r.open[:-2]
fig = plt.figure()
ax = fig.add_subplot(111)
ax.scatter(delta1[:-1], delta1[1:], c=close, s=volume, alpha=0.75)
ax.set_xlabel(r'$\Delta_i$', fontsize=20)
ax.set_ylabel(r'$\Delta_{i+1}$', fontsize=20)
ax.set_title('Volume and percent change')
ax.grid(True)
plt.show()
