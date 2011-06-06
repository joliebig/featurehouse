'''
Test combinations of contouring, filled contouring, and image plotting.
For contour labelling, see contour_demo.py.
The emphasis in this demo is on showing how to make contours register
correctly on images, and on how to get both of them oriented as
desired.  In particular, note the usage of the "origin" and "extent"
keyword arguments to imshow and contour.
'''
from pylab import *
delta = 0.5
extent = (-3,4,-4,3)
x = arange(-3.0, 4.001, delta)
y = arange(-4.0, 3.001, delta)
X, Y = meshgrid(x, y)
Z1 = bivariate_normal(X, Y, 1.0, 1.0, 0.0, 0.0)
Z2 = bivariate_normal(X, Y, 1.5, 0.5, 1, 1)
Z = (Z1 - Z2) * 10
levels = arange(-2.0, 1.601, 0.4) # Boost the upper limit to avoid truncation
figure()
subplot(2,2,1)
cset1 = contourf(X, Y, Z, levels,
                        cmap=cm.get_cmap('jet', len(levels)-1),
                        )
cset2 = contour(X, Y, Z, cset1.levels,
                        colors = 'k',
                        hold='on')
for c in cset2.collections:
    c.set_linestyle('solid')
cset3 = contour(X, Y, Z, (0,),
                colors = 'g',
                linewidths = 2,
                hold='on')
title('Filled contours')
colorbar(cset1)
subplot(2,2,2)
imshow(Z, extent=extent)
v = axis()
contour(Z, levels, hold='on', colors = 'k',
        origin='upper', extent=extent)
axis(v)
title("Image, origin 'upper'")
subplot(2,2,3)
imshow(Z, origin='lower', extent=extent)
v = axis()
contour(Z, levels, hold='on', colors = 'k',
        origin='lower', extent=extent)
axis(v)
title("Image, origin 'lower'")
subplot(2,2,4)
im = imshow(Z, interpolation='nearest', extent=extent)
v = axis()
contour(Z, levels, hold='on', colors = 'k',
        origin='image', extent=extent)
axis(v)
ylim = get(gca(), 'ylim')
setp(gca(), ylim=ylim[::-1])
title("Image, origin from rc, reversed y-axis")
colorbar(im)
show()
