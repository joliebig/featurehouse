from pylab import *
N = 150
r = 2*rand(N)
theta = 2*pi*rand(N)
area = 200*r**2*rand(N)
colors = theta
ax = subplot(111, polar=True)
c = scatter(theta, r, c=colors, s=area)
c.set_alpha(0.75)
show()
