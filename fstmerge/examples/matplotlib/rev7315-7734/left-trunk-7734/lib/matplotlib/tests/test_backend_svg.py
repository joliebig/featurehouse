import matplotlib.pyplot as plt
import numpy as np
import cStringIO as StringIO
import xml.parsers.expat
from matplotlib.testing.decorators import knownfailureif
@knownfailureif(True)
def test_visibility():
    fig=plt.figure()
    ax=fig.add_subplot(1,1,1)
    x = np.linspace(0,4*np.pi,50)
    y = np.sin(x)
    yerr = np.ones_like(y)
    a,b,c=ax.errorbar(x,y,yerr=yerr,fmt='ko')
    for artist in b:
        artist.set_visible(False)
    fd = StringIO.StringIO()
    fig.savefig(fd,format='svg')
    fd.seek(0)
    buf = fd.read()
    fd.close()
    parser = xml.parsers.expat.ParserCreate()
    parser.Parse(buf) # this will raise ExpatError if the svg is invalid
