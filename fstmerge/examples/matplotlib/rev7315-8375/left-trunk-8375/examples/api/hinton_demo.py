import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
from matplotlib.ticker import NullLocator
def hinton(W, maxWeight=None, ax=None):
    """
    Draws a Hinton diagram for visualizing a weight matrix. 
    """
    if not ax:
        fig = plt.figure()
        ax = fig.add_subplot(1, 1, 1)
    if not maxWeight:
        maxWeight = 2**np.ceil(np.log(np.abs(W).max())/np.log(2))
    ax.patch.set_facecolor('gray')
    ax.set_aspect('equal', 'box')
    ax.xaxis.set_major_locator(NullLocator())
    ax.yaxis.set_major_locator(NullLocator())
    for (x,y),w in np.ndenumerate(W):
        if w > 0:
            color = 'white'
        else:
            color = 'black'
        size = np.sqrt(np.abs(w))
        rect = Rectangle([x - size / 2, y - size / 2], size, size,
            facecolor=color, edgecolor=color)
        ax.add_patch(rect)
    ax.autoscale_view()
    ax.set_ylim(*ax.get_ylim()[::-1])
if __name__ == '__main__':
    hinton(np.random.rand(20, 20) - 0.5)
    plt.title('Hinton Example')
    plt.show()
