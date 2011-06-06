import matplotlib
matplotlib.use('TkAgg')
import sys
import pylab as p
import numpy as npy
import time
ax = p.subplot(111)
canvas = ax.figure.canvas
x = npy.arange(0,2*npy.pi,0.01)
line, = p.plot(x, npy.sin(x), animated=True, lw=2)
def run(*args):
    background = canvas.copy_from_bbox(ax.bbox)
    tstart = time.time()
    while 1:
        canvas.restore_region(background)
        line.set_ydata(npy.sin(x+run.cnt/10.0))
        ax.draw_artist(line)
        canvas.blit(ax.bbox)
        if run.cnt==1000:
            print 'FPS:' , 1000/(time.time()-tstart)
            sys.exit()
        run.cnt += 1
run.cnt = 0
p.subplots_adjust(left=0.3, bottom=0.3) # check for flipy bugs
p.grid() # to ensure proper background restore
manager = p.get_current_fig_manager()
manager.window.after(100, run)
p.show()
