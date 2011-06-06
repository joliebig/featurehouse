from pylab import *
dfile = '../data/s1045.ima'
im = fromstring(file(dfile, 'rb').read(), uint16).astype(float)
im.shape = 256, 256
imshow(im, cmap=cm.jet)
axis('off')
show()
