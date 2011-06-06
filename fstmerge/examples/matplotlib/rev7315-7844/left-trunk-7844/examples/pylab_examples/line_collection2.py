from pylab import *
from matplotlib.collections import LineCollection
N = 50
x = arange(N)
ys = [x+i for i in x]
ax = axes()
ax.set_xlim((amin(x),amax(x)))
ax.set_ylim((amin(amin(ys)),amax(amax(ys))))
line_segments = LineCollection([zip(x,y) for y in ys], # Make a sequence of x,y pairs
                                linewidths    = (0.5,1,1.5,2),
                                linestyles = 'solid')
line_segments.set_array(x)
ax.add_collection(line_segments)
fig = gcf()
axcb = fig.colorbar(line_segments)
axcb.set_label('Line Number')
ax.set_title('Line Collection with mapped colors')
sci(line_segments) # This allows interactive changing of the colormap.
show()
