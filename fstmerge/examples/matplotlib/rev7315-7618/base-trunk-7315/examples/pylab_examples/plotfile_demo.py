from pylab import plotfile, show, gca
fname = '../data/msft.csv'
fname2 = '../data/data_x_x2_x3.csv'
plotfile(fname, (0,5,6))
plotfile(fname, ('date', 'volume', 'adj_close'))
plotfile(fname, ('date', 'volume', 'adj_close'), plotfuncs={'volume': 'semilogy'})
plotfile(fname, (0,5,6), plotfuncs={5:'semilogy'})
plotfile(fname, ('date', 'open', 'high', 'low', 'close'), subplots=False)
plotfile(fname2, cols=(0,1,2), delimiter=' ',
         names=['$x$', '$f(x)=x^2$', '$f(x)=x^3$'])
plotfile(fname2, cols=(0, 1), delimiter=' ')
plotfile(fname2, cols=(0, 2), newfig=False, delimiter=' ') # use current figure
gca().set_xlabel(r'$x$')
gca().set_ylabel(r'$f(x) = x^2, x^3$')
plotfile(fname, (0,5,6), plotfuncs={5:'bar'})
show()
