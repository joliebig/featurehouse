import os
from mpl_toolkits.basemap import Basemap, shiftgrid
import numpy as np
from pylab import title, colorbar, show, axes, cm, arange, figure, \
                  text
datadir = '/home/jdhunter/python/svn/matplotlib/trunk/htdocs/screenshots/data/'
if not os.path.exists(datadir):
    raise SystemExit('You need to download the data with svn co https://matplotlib.svn.sourceforge.net/svnroot/matplotlib/trunk/htdocs/screenshots/data/" and set the datadir variable in %s'%__file__)
topoin = np.loadtxt(os.path.join(datadir, 'etopo20data.gz'))
lons = np.loadtxt(os.path.join(datadir, 'etopo20lons.gz'))
lats = np.loadtxt(os.path.join(datadir, 'etopo20lats.gz'))
topoin,lons = shiftgrid(180.,topoin,lons,start=False)
m = Basemap(llcrnrlon=-145.5,llcrnrlat=1.,urcrnrlon=-2.566,urcrnrlat=46.352,\
            rsphere=(6378137.00,6356752.3142),\
            resolution='l',area_thresh=1000.,projection='lcc',\
            lat_1=50.,lon_0=-107.)
nx = int((m.xmax-m.xmin)/40000.)+1; ny = int((m.ymax-m.ymin)/40000.)+1
topodat,x,y = m.transform_scalar(topoin,lons,lats,nx,ny,returnxy=True)
fig=figure(figsize=(6,6))
ax = fig.add_axes([0.1,0.1,0.7,0.7])
im = m.imshow(topodat,cm.jet)
pos = ax.get_position()
l, b, w, h = getattr(pos, 'bounds', pos)
cax = axes([l+w+0.075, b, 0.05, h])
colorbar(cax=cax) # draw colorbar
axes(ax)  # make the original axes current again
xpt,ypt = m(-104.237,40.125)
m.plot([xpt],[ypt],'bo')
text(xpt+100000,ypt+100000,'Boulder')
m.drawcoastlines()
m.drawcountries()
m.drawstates()
parallels = arange(0.,80,20.)
m.drawparallels(parallels,labels=[1,1,0,1])
meridians = arange(10.,360.,30.)
m.drawmeridians(meridians,labels=[1,1,0,1])
title('ETOPO Topography - Lambert Conformal Conic')
show()
