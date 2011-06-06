import datetime
import numpy as np
import matplotlib
import matplotlib.cbook as cbook
import matplotlib.dates as dates
import matplotlib.ticker as ticker
import matplotlib.pyplot as plt
fh = cbook.get_sample_data('aapl.npy')
r = np.load(fh); fh.close()
r = r[-250:]  # get the last 250 days
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(r.date, r.adj_close)
ax.xaxis.set_major_locator(dates.MonthLocator())
ax.xaxis.set_minor_locator(dates.MonthLocator(bymonthday=15))
ax.xaxis.set_major_formatter(ticker.NullFormatter())
ax.xaxis.set_minor_formatter(dates.DateFormatter('%b'))
for tick in ax.xaxis.get_minor_ticks():
    tick.tick1line.set_markersize(0)
    tick.tick2line.set_markersize(0)
    tick.label1.set_horizontalalignment('center')
imid = len(r)/2
ax.set_xlabel(str(r.date[imid].year))
plt.show()
