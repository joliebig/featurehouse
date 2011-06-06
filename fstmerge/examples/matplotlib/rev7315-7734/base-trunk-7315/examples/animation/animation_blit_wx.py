NBLITS = 1000
import matplotlib
matplotlib.use('WXAgg')
matplotlib.rcParams['toolbar'] = 'None'
import wx
import sys
import pylab as p
import numpy as npy
import time
if '--no-accel' in sys.argv:
    import matplotlib.backends.backend_wxagg
    matplotlib.backends.backend_wxagg._use_accelerator(False)
ax = p.subplot(111)
canvas = ax.figure.canvas
p.subplots_adjust(left=0.3, bottom=0.3) # check for flipy bugs
p.grid() # to ensure proper background restore
x = npy.arange(0,2*npy.pi,0.01)
line, = p.plot(x, npy.sin(x), animated=True, lw=2)
tstart = time.time()
blit_time = 0.0
def update_line(*args):
    global blit_time
    if update_line.background is None:
        update_line.background = canvas.copy_from_bbox(ax.bbox)
    canvas.restore_region(update_line.background)
    line.set_ydata(npy.sin(x+update_line.cnt/10.0))
    ax.draw_artist(line)
    t = time.time()
    canvas.blit(ax.bbox)
    blit_time += time.time() - t
    if update_line.cnt == NBLITS:
        frame_time = time.time() - tstart
        print '%d frames: %.2f seconds' % (NBLITS, frame_time)
        print '%d blits:  %.2f seconds' % (NBLITS, blit_time)
        print
        print 'FPS: %.2f' % (NBLITS/frame_time)
        print 'BPS: %.2f' % (NBLITS/blit_time)
        sys.exit()
    update_line.cnt += 1
    wx.WakeUpIdle()
update_line.cnt = 0
update_line.background = None
wx.EVT_IDLE(wx.GetApp(), update_line)
p.show()
