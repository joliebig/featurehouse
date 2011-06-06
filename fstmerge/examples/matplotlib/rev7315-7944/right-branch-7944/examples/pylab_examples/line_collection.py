import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.colors import colorConverter
import numpy as np
x = np.arange(100)
ys = x[:50, np.newaxis] + x[np.newaxis, :]
segs = np.zeros((50, 100, 2), float)
segs[:,:,1] = ys
segs[:,:,0] = x
segs = np.ma.masked_where((segs > 50) & (segs < 60), segs)
ax = plt.axes()
ax.set_xlim(x.min(), x.max())
ax.set_ylim(ys.min(), ys.max())
line_segments = LineCollection(segs,
                                linewidths    = (0.5,1,1.5,2),
                                colors        = [colorConverter.to_rgba(i) \
                                                 for i in ('b','g','r','c','m','y','k')],
                                linestyle = 'solid')
ax.add_collection(line_segments)
ax.set_title('Line collection with masked arrays')
plt.show()
