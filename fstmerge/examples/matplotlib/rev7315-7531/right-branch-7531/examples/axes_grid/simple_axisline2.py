import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid.axislines import SubplotZero
import numpy as np
fig = plt.figure(1, (4,3))
ax = SubplotZero(fig, 1, 1, 1)
fig.add_subplot(ax)
ax.axis["xzero"].set_visible(True)
ax.axis["xzero"].label.set_text("Axis Zero")
for n in ["bottom", "top", "right"]:
    ax.axis[n].set_visible(False)
xx = np.arange(0, 2*np.pi, 0.01)
ax.plot(xx, np.sin(xx))
plt.show()
