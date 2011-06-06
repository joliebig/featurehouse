import matplotlib
matplotlib.use('Agg')
from pylab import figure, show
import numpy as np
fig = figure()
ax = fig.add_subplot(111)
ax.plot([1,2,3])
ax.set_title('a simple figure')
fig.canvas.draw()
buf = fig.canvas.buffer_rgba(0,0)
l, b, w, h = fig.bbox.bounds
X = np.fromstring(buf, np.uint8)
X.shape = h,w,4
fig2 = figure()
ax2 = fig2.add_subplot(111, frameon=False)
ax2.imshow(X)
show()
