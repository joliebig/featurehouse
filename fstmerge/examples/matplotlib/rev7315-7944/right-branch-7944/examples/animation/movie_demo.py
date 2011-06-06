import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt   # For plotting graphs.
import numpy as np
import subprocess                 # For issuing commands to the OS.
import os
import sys                        # For determining the Python version.
print 'Executing on', os.uname()
print 'Python version', sys.version
print 'matplotlib version', matplotlib.__version__
not_found_msg = """
The mencoder command was not found;
mencoder is used by this script to make an avi file from a set of pngs.
It is typically not installed by default on linux distros because of
legal restrictions, but it is widely available.
"""
try:
    subprocess.check_call(['mencoder'])
except subprocess.CalledProcessError:
    print "mencoder command was found"
    pass # mencoder is found, but returns non-zero exit as expected
except OSError:
    print not_found_msg
    sys.exit("quitting\n")
print 'Initializing data set...'   # Let the user know what's happening.
numberOfTimeSteps = 100   # Number of frames we want in the movie.
x = np.arange(-10,10,0.01)   # Values to be plotted on the x-axis.
mean = -6                 # Initial mean of the Gaussian.
stddev = 0.2              # Initial standard deviation.
meaninc = 0.1             # Mean increment.
stddevinc = 0.1           # Standard deviation increment.
y = np.zeros((numberOfTimeSteps,len(x)), float)
for i in range(numberOfTimeSteps) :
    y[i] = (1/np.sqrt(2*np.pi*stddev))*np.exp(-((x-mean)**2)/(2*stddev))
    mean = mean + meaninc
    stddev = stddev + stddevinc
print 'Done.'                       # Let the user know what's happening.
for i in range(len(y)) :
    plt.plot(x,y[i],'b.')
    plt.axis((x[0],x[-1],-0.25,1))
    plt.xlabel('time (ms)')
    plt.ylabel('probability density function')
    plt.title(r'$\cal{N}(\mu, \sigma^2)$', fontsize=20)
    filename = str('%03d' % i) + '.png'
    plt.savefig(filename, dpi=100)
    print 'Wrote file', filename
    plt.clf()
command = ('mencoder',
           'mf://*.png',
           '-mf',
           'type=png:w=800:h=600:fps=25',
           '-ovc',
           'lavc',
           '-lavcopts',
           'vcodec=mpeg4',
           '-oac',
           'copy',
           '-o',
           'output.avi')
print "\n\nabout to execute:\n%s\n\n" % ' '.join(command)
subprocess.check_call(command)
print "\n\n The movie was written to 'output.avi'"
print "\n\n You may want to delete *.png now.\n\n"
