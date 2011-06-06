import os, sys
import matplotlib
matplotlib.use('QtAgg') # qt3 example
from qt import *
QApplication.setColorSpec(QApplication.NormalColor)
TRUE  = 1
FALSE = 0
ITERS = 1000
import pylab as p
import numpy as npy
import time
class BlitQT(QObject):
    def __init__(self):
        QObject.__init__(self, None, "app")
        self.ax = p.subplot(111)
        self.canvas = self.ax.figure.canvas
        self.cnt = 0
        self.x = npy.arange(0,2*npy.pi,0.01)
        self.line, = p.plot(self.x, npy.sin(self.x), animated=True, lw=2)
        self.background = None
    def timerEvent(self, evt):
        if self.background is None:
            self.background = self.canvas.copy_from_bbox(self.ax.bbox)
        self.canvas.restore_region(self.background)
        self.line.set_ydata(npy.sin(self.x+self.cnt/10.0))
        self.ax.draw_artist(self.line)
        self.canvas.blit(self.ax.bbox)
        if self.cnt==ITERS:
            print 'FPS:' , ITERS/(time.time()-self.tstart)
            sys.exit()
        else:
            self.cnt += 1
p.subplots_adjust(left=0.3, bottom=0.3) # check for flipy bugs
p.grid() # to ensure proper background restore
app = BlitQT()
app.tstart = time.time()
app.startTimer(0)
p.show()
