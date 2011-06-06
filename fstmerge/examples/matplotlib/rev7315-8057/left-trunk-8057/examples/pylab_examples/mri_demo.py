from pylab import *
import matplotlib.cbook as cbook
dfile = cbook.get_sample_data('s1045.ima', asfileobj=False)
print 'loading image', dfile
im = np.fromstring(file(dfile, 'rb').read(), np.uint16).astype(float)
im.shape = 256, 256
imshow(im, cmap=cm.jet)
axis('off')
show()
