import matplotlib
import numpy as np
from matplotlib.pyplot import figure, show, rc, grid
rc('grid', color='#316931', linewidth=1, linestyle='-')
rc('xtick', labelsize=15)
rc('ytick', labelsize=15)
width, height = matplotlib.rcParams['figure.figsize'] 
size = min(width, height)
fig = figure(figsize=(size, size))
ax = fig.add_axes([0.1, 0.1, 0.8, 0.8], polar=True, axisbg='#d5de9c')
r = np.arange(0, 3.0, 0.01)
theta = 2*np.pi*r
ax.plot(theta, r, color='#ee8d18', lw=3)
ax.set_rmax(2.0)
grid(True)
ax.set_title("And there was much rejoicing!", fontsize=20)
show()
