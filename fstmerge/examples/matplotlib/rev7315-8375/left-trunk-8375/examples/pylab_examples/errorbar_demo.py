import numpy as np
import matplotlib.pyplot as plt
x = np.arange(0.1, 4, 0.5)
y = np.exp(-x)
yerr = 0.1 + 0.2*np.sqrt(x)
xerr = 0.1 + yerr
plt.figure()
plt.errorbar(x, y, xerr=0.2, yerr=0.4)
plt.title("Simplest errorbars, 0.2 in x, 0.4 in y")
fig, axs = plt.subplots(nrows=2, ncols=2, sharex=True)
ax = axs[0,0]
ax.errorbar(x, y, yerr=yerr, fmt='o')
ax.set_title('Vert. symmetric')
ax.locator_params(nbins=4)
ax = axs[0,1]
ax.errorbar(x, y, xerr=xerr, fmt='o')
ax.set_title('Hor. symmetric')
ax = axs[1,0]
ax.errorbar(x, y, yerr=[yerr, 2*yerr], xerr=[xerr, 2*xerr], fmt='--o')
ax.set_title('H, V asymmetric')
ax = axs[1,1]
ax.set_yscale('log')
ylower = np.maximum(1e-2, y - yerr)
yerr_lower = y - ylower
ax.errorbar(x, y, yerr=[yerr_lower, 2*yerr], xerr=xerr,
                            fmt='o', ecolor='g')
ax.set_title('Mixed sym., log y')
fig.suptitle('Variable errorbars')
plt.show()
