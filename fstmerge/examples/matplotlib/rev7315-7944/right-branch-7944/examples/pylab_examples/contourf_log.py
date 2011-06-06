'''
Demonstrate use of a log color scale in contourf
'''
from matplotlib import pyplot as P
import numpy as np
from numpy import ma
from matplotlib import colors, ticker
from matplotlib.mlab import bivariate_normal
N = 100
x = np.linspace(-3.0, 3.0, N)
y = np.linspace(-2.0, 2.0, N)
X, Y = np.meshgrid(x, y)
z = (bivariate_normal(X, Y, 0.1, 0.2, 1.0, 1.0)
        + 0.1 * bivariate_normal(X, Y, 1.0, 1.0, 0.0, 0.0))
z[:5, :5] = -1
z = ma.masked_where(z<= 0, z)
cs = P.contourf(X, Y, z, locator=ticker.LogLocator())
cbar = P.colorbar()
P.show()
