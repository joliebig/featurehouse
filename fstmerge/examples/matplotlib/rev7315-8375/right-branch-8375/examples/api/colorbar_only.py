'''
Make a colorbar as a separate figure.
'''
from matplotlib import pyplot, mpl
fig = pyplot.figure(figsize=(8,3))
ax1 = fig.add_axes([0.05, 0.65, 0.9, 0.15])
ax2 = fig.add_axes([0.05, 0.25, 0.9, 0.15])
cmap = mpl.cm.cool
norm = mpl.colors.Normalize(vmin=5, vmax=10)
cb1 = mpl.colorbar.ColorbarBase(ax1, cmap=cmap,
                                   norm=norm,
                                   orientation='horizontal')
cb1.set_label('Some Units')
cmap = mpl.colors.ListedColormap(['r', 'g', 'b', 'c'])
cmap.set_over('0.25')
cmap.set_under('0.75')
bounds = [1, 2, 4, 7, 8]
norm = mpl.colors.BoundaryNorm(bounds, cmap.N)
cb2 = mpl.colorbar.ColorbarBase(ax2, cmap=cmap,
                                     norm=norm,
                                     boundaries=[0]+bounds+[13],
                                     extend='both',
                                     ticks=bounds, # optional
                                     spacing='proportional',
                                     orientation='horizontal')
cb2.set_label('Discrete intervals, some other units')
pyplot.show()
