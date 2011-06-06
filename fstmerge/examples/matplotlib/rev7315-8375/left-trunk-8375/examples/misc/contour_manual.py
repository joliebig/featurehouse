"""
Example of displaying your own contour lines and polygons using ContourSet.
"""
import matplotlib.pyplot as plt
from matplotlib.contour import ContourSet
import matplotlib.cm as cm
lines0 = [ [[0,0],[0,4]] ]
lines1 = [ [[2,0],[1,2],[1,3]] ]
lines2 = [ [[3,0],[3,2]], [[3,3],[3,4]] ]  # Note two lines.
filled01 = [ [[0,0],[0,4],[1,3],[1,2],[2,0]] ]
filled12 = [ [[2,0],[3,0],[3,2],[1,3],[1,2]],   # Note two polygons.
             [[1,4],[3,4],[3,3]] ]
plt.figure()
cs = ContourSet(plt.gca(), [0,1,2], [filled01, filled12], filled=True, cmap=cm.bone)
cbar = plt.colorbar(cs)
lines = ContourSet(plt.gca(), [0,1,2], [lines0, lines1, lines2], cmap=cm.cool,
                   linewidths=3)
cbar.add_lines(lines)
plt.axis([-0.5, 3.5, -0.5, 4.5])
plt.title('User-specified contours')
plt.figure()
filled01 = [ [[0,0],[3,0],[3,3],[0,3],[1,1],[1,2],[2,2],[2,1]] ]
kinds01  = [ [1,2,2,2,1,2,2,2] ]
cs = ContourSet(plt.gca(), [0,1], [filled01], [kinds01], filled=True)
cbar = plt.colorbar(cs)
plt.axis([-0.5, 3.5, -0.5, 3.5])
plt.title('User specified filled contours with holes')
plt.show()
