import numpy as np
import matplotlib.pyplot as plt
a = np.linspace(0, 1, 256).reshape(1,-1)
a = np.vstack((a,a))
maps = sorted(filter(lambda m: not m.endswith("_r"), plt.cm.datad))
nmaps = len(maps) + 1
fig = plt.figure(figsize=(5,10))
fig.subplots_adjust(top=0.99, bottom=0.01, left=0.2, right=0.99)
for i,m in enumerate(maps):
    ax = plt.subplot(nmaps, 1, i+1)
    plt.axis("off")
    plt.imshow(a, aspect='auto', cmap=plt.get_cmap(m), origin='lower')
    pos = list(ax.get_position().bounds)
    fig.text(pos[0] - 0.01, pos[1], m, fontsize=10, horizontalalignment='right')
plt.show()
