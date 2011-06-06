import matplotlib.pyplot as plt
plt.plot([0,1], label='going up')
plt.plot([1,0], label='going down')
leg = plt.legend(fancybox=True, loc='center')
leg.get_frame().set_alpha(0.5)
plt.show()
