import math
from pylab import *
from matplotlib.patches import Ellipse, Arc
x = 2692.440
y = 6720.850
r = math.sqrt( x*x+y*y )
delta = 6
def custom_ellipse( ax, x, y, major, minor, theta, numpoints = 750, **kwargs ):
   xs = []
   ys = []
   incr = 2.0*math.pi / numpoints
   incrTheta = 0.0
   while incrTheta <= (2.0*math.pi):
      a = major * math.cos( incrTheta )
      b = minor * math.sin( incrTheta )
      l = math.sqrt( ( a**2 ) + ( b**2 ) )
      phi = math.atan2( b, a )
      incrTheta += incr
      xs.append( x + ( l * math.cos( theta + phi ) ) )
      ys.append( y + ( l * math.sin( theta + phi ) ) )
   incrTheta = 2.0*math.pi
   a = major * math.cos( incrTheta )
   b = minor * math.sin( incrTheta )
   l = sqrt( ( a**2 ) + ( b**2 ) )
   phi = math.atan2( b, a )
   xs.append( x + ( l * math.cos( theta + phi ) ) )
   ys.append( y + ( l * math.sin( theta + phi ) ) )
   ellipseLine = ax.plot( xs, ys, **kwargs )
ax1 = subplot( 311, aspect='equal' )
ax1.set_aspect( 'equal', 'datalim' )
diam = (r - delta) * 2.0
lower_ellipse = Ellipse( (0.0, 0.0), diam, diam, 0.0, fill=False, edgecolor="darkgreen" )
ax1.add_patch( lower_ellipse )
diam = r * 2.0
target_ellipse = Ellipse( (0.0, 0.0), diam, diam, 0.0, fill=False, edgecolor="darkred" )
ax1.add_patch( target_ellipse )
diam = (r + delta) * 2.0
upper_ellipse = Ellipse( (0.0, 0.0), diam, diam, 0.0, fill=False, edgecolor="darkblue" )
ax1.add_patch( upper_ellipse )
diam = delta * 2.0
target = Ellipse( (x, y), diam, diam, 0.0, fill=False, edgecolor="#DD1208" )
ax1.add_patch( target )
ax1.plot( [x], [y], marker='x', linestyle='None', mfc='red', mec='red', markersize=10 )
ax = subplot( 312, aspect='equal' , sharex=ax1, sharey=ax1)
ax.set_aspect( 'equal', 'datalim' )
diam = (r - delta) * 2.0
lower_arc = Arc( (0.0, 0.0), diam, diam, 0.0, fill=False, edgecolor="darkgreen" )
ax.add_patch( lower_arc )
diam = r * 2.0
target_arc = Arc( (0.0, 0.0), diam, diam, 0.0, fill=False, edgecolor="darkred" )
ax.add_patch( target_arc )
diam = (r + delta) * 2.0
upper_arc = Arc( (0.0, 0.0), diam, diam, 0.0, fill=False, edgecolor="darkblue" )
ax.add_patch( upper_arc )
diam = delta * 2.0
target = Arc( (x, y), diam, diam, 0.0, fill=False, edgecolor="#DD1208" )
ax.add_patch( target )
ax.plot( [x], [y], marker='x', linestyle='None', mfc='red', mec='red', markersize=10 )
ax = subplot( 313, aspect='equal', sharex=ax1, sharey=ax1 )
ax.set_aspect( 'equal', 'datalim' )
custom_ellipse( ax, 0.0, 0.0, r-delta, r-delta, 0.0, color="darkgreen" )
custom_ellipse( ax, 0.0, 0.0, r, r, 0.0, color="darkred" )
custom_ellipse( ax, 0.0, 0.0, r+delta, r+delta, 0.0, color="darkblue" )
custom_ellipse( ax, x, y, delta, delta, 0.0, color="#BB1208" )
ax.plot( [x], [y], marker='x', linestyle='None', mfc='red', mec='red', markersize=10 )
ax.plot( [x], [y], marker='x', linestyle='None', mfc='red', mec='red', markersize=10 )
ax1.set_xlim(2650, 2735)
ax1.set_ylim(6705, 6735)
savefig("ellipse")
show()
