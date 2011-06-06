import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LightSource
X,Y=np.mgrid[-5:5:0.05,-5:5:0.05]
Z=np.sqrt(X**2+Y**2)+np.sin(X**2+Y**2)
ls = LightSource(azdeg=0,altdeg=65)
rgb = ls.shade(Z,plt.cm.copper)
plt.figure(figsize=(12,5))
plt.subplot(121)
plt.imshow(Z,cmap=plt.cm.copper)
plt.title('imshow')
plt.xticks([]); plt.yticks([])
plt.subplot(122)
plt.imshow(rgb)
plt.title('imshow with shading')
plt.xticks([]); plt.yticks([])
plt.show()
