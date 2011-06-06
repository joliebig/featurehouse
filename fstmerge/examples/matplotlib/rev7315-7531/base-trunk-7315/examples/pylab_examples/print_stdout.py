import sys
import matplotlib
matplotlib.use('Agg')
from pylab import *
plot([1,2,3])
savefig(sys.stdout)
show()
