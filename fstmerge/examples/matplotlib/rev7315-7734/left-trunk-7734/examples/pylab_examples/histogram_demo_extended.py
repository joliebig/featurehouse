import pylab as P
mu, sigma = 200, 25
x = mu + sigma*P.randn(10000)
n, bins, patches = P.hist(x, 50, normed=1, histtype='stepfilled')
P.setp(patches, 'facecolor', 'g', 'alpha', 0.75)
y = P.normpdf( bins, mu, sigma)
l = P.plot(bins, y, 'k--', linewidth=1.5)
P.figure()
bins = [100,125,150,160,170,180,190,200,210,220,230,240,250,275,300]
n, bins, patches = P.hist(x, bins, normed=1, histtype='bar', rwidth=0.8)
P.figure()
n, bins, patches = P.hist(x, 50, normed=1, histtype='step', cumulative=True)
y = P.normpdf( bins, mu, sigma).cumsum()
y /= y[-1]
l = P.plot(bins, y, 'k--', linewidth=1.5)
sigma2 = 15.
x = mu + sigma2*P.randn(10000)
n, bins, patches = P.hist(x, bins=bins, normed=1, histtype='step', cumulative=True)
y = P.normpdf( bins, mu, sigma2).cumsum()
y /= y[-1]
l = P.plot(bins, y, 'r--', linewidth=1.5)
n, bins, patches = P.hist(x, bins=bins, normed=1,
    histtype='step', cumulative=-1)
P.grid(True)
P.ylim(0, 1.05)
P.figure()
x = mu + sigma*P.randn(1000,3)
n, bins, patches = P.hist(x, 10, normed=1, histtype='bar')
P.figure()
n, bins, patches = P.hist(x, 10, normed=1, histtype='barstacked')
x0 = mu + sigma*P.randn(10000)
x1 = mu + sigma*P.randn(7000)
x2 = mu + sigma*P.randn(3000)
P.figure()
n, bins, patches = P.hist( [x0,x1,x2], 10, histtype='bar')
P.show()
