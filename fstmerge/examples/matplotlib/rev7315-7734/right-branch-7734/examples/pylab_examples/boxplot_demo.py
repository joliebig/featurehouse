from pylab import *
spread= rand(50) * 100
center = ones(25) * 50
flier_high = rand(10) * 100 + 100
flier_low = rand(10) * -100
data =concatenate((spread, center, flier_high, flier_low), 0)
boxplot(data)
figure()
boxplot(data,1)
figure()
boxplot(data,0,'gD')
figure()
boxplot(data,0,'')
figure()
boxplot(data,0,'rs',0)
figure()
boxplot(data,0,'rs',0,0.75)
spread= rand(50) * 100
center = ones(25) * 40
flier_high = rand(10) * 100 + 100
flier_low = rand(10) * -100
d2 = concatenate( (spread, center, flier_high, flier_low), 0 )
data.shape = (-1, 1)
d2.shape = (-1, 1)
data = [data, d2, d2[::2,0]]
figure()
boxplot(data)
show()
