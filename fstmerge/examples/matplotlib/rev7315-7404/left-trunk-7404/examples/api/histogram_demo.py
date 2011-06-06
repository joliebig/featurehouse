"""
Make a histogram of normally distributed random numbers and plot the
analytic PDF over it
"""
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
mu, sigma = 100, 15
x = mu + sigma * np.random.randn(10000)
fig = plt.figure()
ax = fig.add_subplot(111)
n, bins, patches = ax.hist(x, 50, normed=1, facecolor='green', alpha=0.75)
bincenters = 0.5*(bins[1:]+bins[:-1])
y = mlab.normpdf( bincenters, mu, sigma)
l = ax.plot(bincenters, y, 'r--', linewidth=1)
ax.set_xlabel('Smarts')
ax.set_ylabel('Probability')
ax.set_xlim(40, 160)
ax.set_ylim(0, 0.03)
ax.grid(True)
plt.show()
