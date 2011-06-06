import numpy as n
from pylab import figure, show
import matplotlib.cm as cm
import matplotlib.colors as colors
fig = figure()
ax = fig.add_subplot(111)
Ntotal = 1000
N, bins, patches = ax.hist(n.random.rand(Ntotal), 20)
fracs = N.astype(float)/N.max()
norm = colors.normalize(fracs.min(), fracs.max())
for thisfrac, thispatch in zip(fracs, patches):
    color = cm.jet(norm(thisfrac))
    thispatch.set_facecolor(color)
show()
