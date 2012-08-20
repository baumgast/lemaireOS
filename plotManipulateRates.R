xlim = c(40,1000)
ylim = c(0,100)

rows = which(out[,1] >= xlim[1] & out1[,1] <= xlim[2])

mPol1 = mean(sumPol(out1))
mPol2 = mean(sumPol(out2))
mPol3 = mean(sumPol(out3))
mPol4 = mean(sumPol(out4))
mPol5 = mean(sumPol(out5))

t = out1[,1]
par(mfrow = c(1,1))

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'ER-alpha, time evolution at different reaction rates',xlab = 'Time (min)', ylab = '% bound promoters')

points(t,sumComponents(out1), type = 'l', lwd = 2)
points(t,sumComponents(out2), type = 'l', col = 'lightblue')
points(t,sumComponents(out3), type = 'l', col = 'steelblue')
points(t,sumComponents(out4), type = 'l', col = 'royalblue')
points(t,sumComponents(out5), type = 'l', col = 'darkblue')

legend(x = 120, y = 105, c(paste('a = 5/min, mean Pol =',round(mPol1,2),'%'),
                           paste('a = 2.5/min, mean Pol =', round(mPol2,2), '%'),
                           paste('a = 1/min, mean Pol =', round(mPol3,2), '%'),
                           paste('a = 0.5/min, mean Pol =',round(mPol4,2), '%'), 
                           paste('a = 0.1/min, mean Pol =', round(mPol5,2),'%')), lwd = c(2,2,2,2,2), 
       col = c('black','lightblue', 'steelblue', 'royalblue', 'darkblue'), bty = 'n')
                           
plot(t,sumPol(out1), type = 'l', xlim = xlim, ylim = ylim, lwd = 2,
     xlab = 'Time (min)',
     ylab = '% bound promoters')
points(t,sumPol(out2), type = 'l', col = 'lightblue')
points(t,sumPol(out3), type ='l' ,col = 'steelblue')
points(t,sumPol(out4), type ='l' ,col = 'royalblue')
points(t,sumPol(out5), type ='l' ,col = 'darkblue')

legend(x = 160, y = 100, c('a = 5/min','a = 2.5/min', 'a = 1/min', 'a = 0.5/min', 'a = 0.1/min'), lwd = c(2,2,2,2,2), 
       col = c('black','lightblue', 'steelblue', 'royalblue', 'darkblue'), bty = 'n')

plot(t,out4[,m11e], type = 'l')
points(t,out4[,(m11e + 1)], type = 'l', col = 'red')
points(t,out4[,(m11e + 2)], type= 'l', col = 'blue')