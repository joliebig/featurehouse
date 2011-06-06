"""
Thanks Josh Hemann for the example
This examples comes from an application in which grade school gym
teachers wanted to be able to show parents how their child did across
a handful of fitness tests, and importantly, relative to how other
children did. To extract the plotting code for demo purposes, we'll
just make up some data for little Johnny Doe...
"""
import numpy as np
import matplotlib.pyplot as plt
import pylab
from matplotlib.patches import Polygon
from matplotlib.ticker import MaxNLocator
student = 'Johnny Doe'
grade = 2
gender = 'boy'
cohortSize = 62 #The number of other 2nd grade boys
numTests = 5
testNames = ['Pacer Test', 'Flexed Arm\n Hang', 'Mile Run', 'Agility',
            'Push Ups']
testMeta = ['laps', 'sec', 'min:sec', 'sec', '']
scores = ['7', '48', '12:52', '17', '14']
rankings = np.round(np.random.uniform(0, 1, numTests)*100, 0)
fig = plt.figure(figsize=(9,7))
ax1 = fig.add_subplot(111)
plt.subplots_adjust(left=0.115, right=0.88)
fig.canvas.set_window_title('Eldorado K-8 Fitness Chart')
pos = np.arange(numTests)+0.5    #Center bars on the Y-axis ticks
rects = ax1.barh(pos, rankings, align='center', height=0.5, color='m')
ax1.axis([0,100,0,5])
pylab.yticks(pos, testNames)
ax1.set_title('Johnny Doe')
plt.text(50, -0.5, 'Cohort Size: ' + str(cohortSize),
        horizontalalignment='center', size='small')
ax2 = ax1.twinx()
ax2.plot([100,100], [0, 5], 'white', alpha=0.1)
ax2.xaxis.set_major_locator(MaxNLocator(11))
xticks = pylab.setp(ax2, xticklabels=['0','10','20','30','40','50','60',
'70',
                                     '80','90','100'])
ax2.xaxis.grid(True, linestyle='--', which='major', color='grey',
alpha=0.25)
plt.plot([50,50], [0, 5], 'grey', alpha=0.25)
def withnew(i, scr):
    if testMeta[i] != '' : return '%s\n'%scr
    else: return scr
scoreLabels = [withnew(i, scr) for i,scr in enumerate(scores)]
scoreLabels = [i+j for i,j in zip(scoreLabels, testMeta)]
pylab.yticks(pos, scoreLabels)
ax2.set_ylabel('Test Scores')
suffixes =['th', 'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th']
ax2.set_xlabel('Percentile Ranking Across ' + str(grade) + suffixes[grade] \
              + ' Grade ' + gender.title() + 's')
for rect in rects:
   width = int(rect.get_width())
   lastDigit = width % 10
   if (width == 11) or (width == 12) or (width == 13):
       suffix = 'th'
   else:
       suffix = suffixes[lastDigit]
   rankStr = str(width) + suffix
   if (width < 5): # The bars aren't wide enough to print the ranking inside
       xloc = width + 1 # Shift the text to the right side of the right edge
       clr = 'black' # Black against white background
       align = 'left'
   else:
       xloc = 0.98*width # Shift the text to the left side of the right edge
       clr = 'white' # White on magenta
       align = 'right'
   yloc = rect.get_y()+rect.get_height()/2.0 #Center the text vertically in the bar
   ax1.text(xloc, yloc, rankStr, horizontalalignment=align,
            verticalalignment='center', color=clr, weight='bold')
plt.show()
