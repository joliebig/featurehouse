__version__ = '$Id: optimize.py 3232 2009-01-27 12:31:49Z montanaro $'
import copy
def SimplexMaximize(var, err, func, convcrit = 0.001, minerr = 0.001):
    import numpy
    var = numpy.array(var)
    simplex = [var]
    for i in range(len(var)):
        var2 = copy.copy(var)
        var2[i] = var[i] + err[i]
        simplex.append(var2)
    value = []
    for i in range(len(simplex)):
        value.append(func(simplex[i]))
    while 1:
        wi = 0
        bi = 0
        for i in range(len(simplex)):
            if value[wi] > value[i]:
                wi = i
            if value[bi] < value[i]:
                bi = i
        if abs(value[bi] - value[wi]) <= convcrit:
            return simplex[bi]
        ave = numpy.zeros(len(var), dtype=numpy.float)
        for i in range(len(simplex)):
            if i != wi:
                ave = ave + simplex[i]
        ave = ave / (len(simplex) - 1)
        worst = numpy.array(simplex[wi])
        simsize = numpy.add.reduce(numpy.absolute(ave - worst))
        if simsize <= minerr:
            return simplex[bi]
        new = 2 * ave - simplex[wi]
        newv = func(new)
        if newv <= value[wi]:
            new = 0.5 * ave + 0.5 * worst
            newv = func(new)
        elif newv > value[bi]:
            new2 = 3 * ave - 2 * worst
            newv2 = func(new2)
            if newv2 > newv:
                new = new2
                newv = newv2
        simplex[wi] = new
        value[wi] = newv
def DoubleSimplexMaximize(var, err, func, convcrit=0.001, minerr=0.001):
    import numpy
    err = numpy.array(err)
    var = SimplexMaximize(var, err, func, convcrit*5, minerr*5)
    return SimplexMaximize(var, 0.4 * err, func, convcrit, minerr)
