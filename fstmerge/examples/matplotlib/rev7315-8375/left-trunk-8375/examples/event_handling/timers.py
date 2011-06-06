import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime
def update_title(axes):
    axes.set_title(datetime.now())
    axes.figure.canvas.draw()
fig = plt.figure()
ax = fig.add_subplot(1, 1, 1)
x = np.linspace(-3, 3)
ax.plot(x, x*x)
timer = fig.canvas.new_timer(interval=100)
timer.add_callback(update_title, ax)
timer.start()
plt.show()
