import matplotlib
matplotlib.use('Agg')  # force the antigrain backend
from matplotlib import rc
from matplotlib.backends.backend_agg import FigureCanvasAgg
from matplotlib.figure import Figure
from matplotlib.cbook import iterable
import numpy as np
def make_fig():
    """
    make a figure
    No need to close figures or clean up since the objects will be
    destroyed when they go out of scope
    """
    fig = Figure()
    ax = fig.add_axes([0.2, 0.3, 0.7, 0.6])
    line,  = ax.plot([1,2,3], 'ro--', markersize=12, markerfacecolor='g')
    x = np.random.rand(100)
    y = np.random.rand(100)
    area = np.pi*(10 * np.random.rand(100))**2 # 0 to 10 point radiuses
    c = ax.scatter(x,y,area)
    c.set_alpha(0.5)
    ax.set_title('My first image')
    ax.set_ylabel('Some numbers')
    ax.set_xticks( (.2,.4,.6,.8) )
    labels = ax.set_xticklabels(('Bill', 'Fred', 'Ted', 'Ed'))
    for l in labels:
        l.set_rotation(45)
        l.set_fontsize(12)
    canvas = FigureCanvasAgg(fig)
    canvas.print_figure('webapp', dpi=150)
make_fig()
