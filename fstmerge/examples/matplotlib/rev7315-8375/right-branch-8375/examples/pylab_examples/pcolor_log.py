from pylab import *
from matplotlib.colors import LogNorm
N = 100
x = linspace(-3.0, 3.0, N)
y = linspace(-2.0, 2.0, N)
X, Y = meshgrid(x, y)
Z1 = bivariate_normal(X, Y, 0.1, 0.2, 1.0, 1.0) + 0.1*bivariate_normal(X, Y, 1.0, 1.0, 0.0, 0.0)
subplot(2,1,1)
pcolor(X, Y, Z1, norm=LogNorm(vmin=Z1.min(), vmax=Z1.max()))
colorbar()
subplot(2,1,2)
pcolor(X, Y, Z1)
colorbar()
show()
