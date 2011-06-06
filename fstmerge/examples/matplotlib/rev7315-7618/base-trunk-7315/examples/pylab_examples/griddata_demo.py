from numpy.random import uniform, seed
from matplotlib.mlab import griddata
import matplotlib.pyplot as plt
import numpy as np
seed(0)
npts = 200
x = uniform(-2,2,npts)
y = uniform(-2,2,npts)
z = x*np.exp(-x**2-y**2)
xi = np.linspace(-2.1,2.1,100)
yi = np.linspace(-2.1,2.1,100)
zi = griddata(x,y,z,xi,yi)
CS = plt.contour(xi,yi,zi,15,linewidths=0.5,colors='k')
CS = plt.contourf(xi,yi,zi,15,cmap=plt.cm.jet)
plt.colorbar() # draw colorbar
plt.scatter(x,y,marker='o',c='b',s=5)
plt.xlim(-2,2)    
plt.ylim(-2,2)     
plt.title('griddata test (%d points)' % npts)
plt.show()
