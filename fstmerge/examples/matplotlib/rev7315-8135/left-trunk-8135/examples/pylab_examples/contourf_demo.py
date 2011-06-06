from pylab import *
origin = 'lower'
delta = 0.025
x = y = arange(-3.0, 3.01, delta)
X, Y = meshgrid(x, y)
Z1 = bivariate_normal(X, Y, 1.0, 1.0, 0.0, 0.0)
Z2 = bivariate_normal(X, Y, 1.5, 0.5, 1, 1)
Z = 10 * (Z1 - Z2)
nr, nc = Z.shape
Z[-nr//6:, -nc//6:] = nan
Z = ma.array(Z)
Z[:nr//6, :nc//6] = ma.masked
interior = sqrt((X**2) + (Y**2)) < 0.5
Z[interior] = ma.masked
CS = contourf(X, Y, Z, 10, # [-1, -0.1, 0, 0.1],
                        cmap=cm.bone,
                        origin=origin)
CS2 = contour(CS, levels=CS.levels[::2],
                        colors = 'r',
                        origin=origin,
                        hold='on')
title('Nonsense (3 masked regions)')
xlabel('word length anomaly')
ylabel('sentence length anomaly')
cbar = colorbar(CS)
cbar.ax.set_ylabel('verbosity coefficient')
cbar.add_lines(CS2)
figure()
levels = [-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5]
CS3 = contourf(X, Y, Z, levels,
                        colors = ('r', 'g', 'b'),
                        origin=origin)
CS4 = contour(X, Y, Z, levels,
                       colors = ('k',),
                       linewidths = (3,),
                       origin = origin)
title('Listed colors (3 masked regions)')
clabel(CS4, fmt = '%2.1f', colors = 'w', fontsize=14)
colorbar(CS3)
show()
