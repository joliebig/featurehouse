import matplotlib.pyplot as plt
import matplotlib.transforms as mtransforms
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(range(10))
ax.set_yticks((2,5,7))
labels = ax.set_yticklabels(('really, really, really', 'long', 'labels'))
def on_draw(event):
   bboxes = []
   for label in labels:
       bbox = label.get_window_extent()
       bboxi = bbox.inverse_transformed(fig.transFigure)
       bboxes.append(bboxi)
   bbox = mtransforms.Bbox.union(bboxes)
   if fig.subplotpars.left < bbox.width:
       fig.subplots_adjust(left=1.1*bbox.width) # pad a little
       fig.canvas.draw()
   return False
fig.canvas.mpl_connect('draw_event', on_draw)
plt.show()
