import numpy as np
import matplotlib.pyplot as plt
t = np.arange(-1,2, .01)
s = np.sin(2*np.pi*t)
plt.plot(t,s)
l = plt.axhline(linewidth=4, color='r')
l = plt.axhline(y=1)
l = plt.axvline(x=1)
l = plt.axvline(x=0, ymin=0.75, linewidth=4, color='b')
l = plt.axhline(y=.5, xmin=0.25, xmax=0.75)
p = plt.axhspan(0.25, 0.75, facecolor='0.5', alpha=0.5)
p = plt.axvspan(1.25, 1.55, facecolor='g', alpha=0.5)
plt.axis([-1,2,-1,2])
plt.show()
