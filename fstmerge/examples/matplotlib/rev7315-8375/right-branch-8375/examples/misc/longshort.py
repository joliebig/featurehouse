"""
Illustrate the rec array utility funcitons by loading prices from a
csv file, computing the daily returns, appending the results to the
record arrays, joining on date
"""
import urllib
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
u1 = urllib.urlretrieve('http://ichart.finance.yahoo.com/table.csv?s=AAPL&d=9&e=14&f=2008&g=d&a=8&b=7&c=1984&ignore=.csv')
u2 = urllib.urlretrieve('http://ichart.finance.yahoo.com/table.csv?s=GOOG&d=9&e=14&f=2008&g=d&a=8&b=7&c=1984&ignore=.csv')
r1 = mlab.csv2rec(file(u1[0]))
r2 = mlab.csv2rec(file(u2[0]))
gains1 = np.zeros_like(r1.adj_close)
gains2 = np.zeros_like(r2.adj_close)
gains1[1:] = np.diff(r1.adj_close)/r1.adj_close[:-1]
gains2[1:] = np.diff(r2.adj_close)/r2.adj_close[:-1]
r1 = mlab.rec_append_fields(r1, 'gains', gains1)
r2 = mlab.rec_append_fields(r2, 'gains', gains2)
r = mlab.rec_join('date', r1, r2)
g = r.gains1-r.gains2
tr = (1+g).cumprod()  # the total return
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(r.date, tr)
ax.set_title('total return: long APPL, short GOOG')
ax.grid()
fig.autofmt_xdate()
plt.show()
