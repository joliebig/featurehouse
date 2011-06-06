import time
import gtk, gobject
import matplotlib
matplotlib.use('GTKAgg')
import numpy as np
import matplotlib.pyplot as plt
fig = plt.figure()
ax = fig.add_subplot(111)
canvas = fig.canvas
fig.subplots_adjust(left=0.3, bottom=0.3) # check for flipy bugs
ax.grid() # to ensure proper background restore
x = np.arange(0,2*np.pi,0.01)
line, = ax.plot(x, np.sin(x), animated=True, lw=2)
canvas.draw()
tstart = time.time()
def update_line(*args):
    print 'you are here', update_line.cnt
    if update_line.background is None:
        update_line.background = canvas.copy_from_bbox(ax.bbox)
    canvas.restore_region(update_line.background)
    line.set_ydata(np.sin(x+update_line.cnt/10.0))
    ax.draw_artist(line)
    canvas.blit(ax.bbox)
    if update_line.cnt==1000:
        print 'FPS:' , 1000/(time.time()-tstart)
        gtk.mainquit()
        raise SystemExit
    update_line.cnt += 1
    return True
update_line.cnt = 0
update_line.background = None
def start_anim(event):
    gobject.idle_add(update_line)
    canvas.mpl_disconnect(start_anim.cid)
start_anim.cid = canvas.mpl_connect('draw_event', start_anim)
plt.show()
