#plot the Er-alpha occupancy at different reaction rates
t = out1[,1]
par(mfrow = c(1,1), cex=1.2)
xlim = c(40,180)
ylim = c(0,100)
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

legend(x = 40, y = 105, c(paste('a = 5/min, mean Pol =',round(mPol1,2),'%'),
                           paste('a = 2.5/min, mean Pol =', round(mPol2,2), '%')),
                           lwd = c(2,2), col = c('black','lightblue'), bty = 'n')

legend(x = 115, y = 105, c(paste('a = 1/min, mean Pol =', round(mPol3,2), '%'),
                           paste('a = 0.5/min, mean Pol =',round(mPol4,2), '%'),
                           paste('a = 0.1/min, mean Pol =', round(mPol5,2),'%')), lwd = c(2,2,2),
       col = c('steelblue','royalblue','darkblue'), bty = 'n')
#                            
#                            paste('a = 1/min, mean Pol =', round(mPol3,2), '%'),
#                            paste('a = 0.5/min, mean Pol =',round(mPol4,2), '%'), 
#                            paste('a = 0.1/min, mean Pol =', round(mPol5,2),'%')), lwd = c(2,2,2,2,2), 
#        col = c('black','lightblue', 'steelblue', 'royalblue', 'darkblue'), bty = 'n')
#-------------------------------------------------------------------------------
#plot the polymerase occupancy
par(mfrow = c(1,1), cex = 1.2)
xlim = c(40,180)
ylim = c(0,100)
plot(t,sumPol(out1), type = 'l', xlim = xlim, ylim = ylim, lwd = 2,
     main = 'RNA pol, time evolution at different rates',
     xlab = 'Time (min)',
     ylab = '% bound promoters')
grid()
points(t,sumPol(out2), type = 'l', col = 'lightblue')
points(t,sumPol(out3), type ='l' ,col = 'steelblue')
points(t,sumPol(out4), type ='l' ,col = 'royalblue')
points(t,sumPol(out5), type ='l' ,col = 'darkblue')

legend(x = 40, y = 100, c('a = 5/min','a = 2.5/min', 'a = 1/min', 'a = 0.5/min', 'a = 0.1/min'), lwd = c(2,2,2,2,2), 
       col = c('black','lightblue', 'steelblue', 'royalblue', 'darkblue'), bty = 'n')

#-------------------------------------------------------------------------------
#compare the single components with its neighbours at different rates
pdf('~/Desktop/lemaireOS/Figues/singleComponents.pdf', width = 11.7, height = 8)
par(mfrow = c(2,3), cex = 1.0)
ylim = c(0,80)
xlim = c(0,200)
plot(t,out1[,(m11e - 0)], type = 'l', main='a = 5', ylim = ylim,
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out1[,(m11e + 1)], type = 'l', col = 'red')
points(t,out1[,(m11e + 2)], type= 'l', col = 'blue')
points(t,out1[,(m11e + 3)], type= 'l', col = 'seagreen')
legend('topright', c('-1', '0','+1','+2'), lwd = 2, col = c('black','red','blue','seagreen'), bty = 'n')

plot(t,out2[,(m11e -0)], type = 'l', main='a = 2.5', ylim = ylim,
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out2[,(m11e + 1)], type = 'l', col = 'red')
points(t,out2[,(m11e + 2)], type= 'l', col = 'blue')
points(t,out2[,(m11e + 3)], type= 'l', col = 'seagreen')
legend('topright', c('-1', '0','+1','+2'), lwd = 2, col = c('black','red','blue','seagreen'), bty = 'n')

plot(t,out3[,(m11e - 0)], type = 'l', main='a = 1', ylim = ylim,
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out3[,(m11e + 1)], type = 'l', col = 'red')
points(t,out3[,(m11e + 2)], type= 'l', col = 'blue')
points(t,out3[,(m11e + 3)], type= 'l', col = 'seagreen')
legend('topright', c('-1', '0','+1','+2'), lwd = 2, col = c('black','red','blue','seagreen'), bty = 'n')

plot(t,out4[,(m11e - 0)], type = 'l', main='a = 0.5', ylim = ylim,
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out4[,(m11e + 1)], type = 'l', col = 'red')
points(t,out4[,(m11e + 2)], type= 'l', col = 'blue')
points(t,out4[,(m11e + 3)], type= 'l', col = 'seagreen')
legend('topright', c('-1', '0','+1','+2'), lwd = 2, col = c('black','red','blue','seagreen'), bty = 'n')

plot(t,out5[,(m11e - 0)], type = 'l', main='a = 0.1', ylim = ylim,
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out5[,(m11e + 1)], type = 'l', col = 'red')
points(t,out5[,(m11e + 2)], type= 'l', col = 'blue')
points(t,out5[,(m11e + 3)], type= 'l', col = 'seagreen')
legend('topright', c('-1', '0','+1','+2'), lwd = 2, col = c('black','red','blue','seagreen'), bty = 'n')

plot(t,out6[,(m11e - 0)], type = 'l', main='a = 0.01', ylim = ylim,
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out6[,(m11e + 1)], type = 'l', col = 'red')
points(t,out6[,(m11e + 2)], type= 'l', col = 'blue')
points(t,out6[,(m11e + 3)], type= 'l', col = 'seagreen')
legend('topright', c('-1', '0','+1','+2'), lwd = 2, col = c('black','red','blue','seagreen'), bty = 'n')
dev.off()
#-------------------------------------------------------------------------------
pdf('~/Desktop/lemaireOS/Figues/singleComponentsRates.pdf', width = 11.7, height = 8)
par(mfrow = c(2,2), cex = 1.0)
ylim1 = c(0,10)
ylim2 = c(0,100)
xlim = c(0,200)
#col = gray(1:6/6)
col = c('black','darkblue', 'purple','royalblue','slateblue','lightblue')
plot(t,out1[,(m11e - 0)], type = 'l', main='-1', ylim = ylim1,col = col[1],
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out2[,(m11e - 0)], type = 'l', col = col[2])
points(t,out3[,(m11e - 0)], type = 'l', col = col[3])
points(t,out4[,(m11e - 0)], type = 'l', col = col[4])
points(t,out5[,(m11e - 0)], type = 'l', col = col[5])
points(t,out6[,(m11e - 0)], type = 'l', col = col[6])

legend('topright', c('a = 5','a = 2.5', 'a = 1', 'a = 0.5', 'a = 0.1', 'a = 0.01'), lwd = 2, col = col, bty = 'n')

plot(t,out1[,(m11e + 1)], type = 'l', main='0', ylim = ylim2,col = col[1],
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out2[,(m11e + 1)], type = 'l', col = col[2])
points(t,out3[,(m11e + 1)], type = 'l', col = col[3])
points(t,out4[,(m11e + 1)], type = 'l', col = col[4])
points(t,out5[,(m11e + 1)], type = 'l', col = col[5])
points(t,out6[,(m11e + 1)], type = 'l', col = col[6])

legend('topright', c('a = 5','a = 2.5', 'a = 1', 'a = 0.5', 'a = 0.1', 'a = 0.01'), lwd = 2, col = col, bty = 'n')

plot(t,out1[,(m11e + 2)], type = 'l', main='+1', ylim = ylim1,col = col[1],
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out2[,(m11e + 2)], type = 'l', col = col[2])
points(t,out3[,(m11e + 2)], type = 'l', col = col[3])
points(t,out4[,(m11e + 2)], type = 'l', col = col[4])
points(t,out5[,(m11e + 2)], type = 'l', col = col[5])
points(t,out6[,(m11e + 2)], type = 'l', col = col[6])

legend('topright', c('a = 5','a = 2.5', 'a = 1', 'a = 0.5', 'a = 0.1', 'a = 0.01'), lwd = 2, col = col, bty = 'n')

plot(t,out1[,(m11e + 3)], type = 'l', main='+2', ylim = ylim1,col = col[1],
     xlim = xlim,
     xlab = 'Time (min)',
     ylab = 'single factors')
grid()
points(t,out2[,(m11e + 3)], type = 'l', col = col[2])
points(t,out3[,(m11e + 3)], type = 'l', col = col[3])
points(t,out4[,(m11e + 3)], type = 'l', col = col[4])
points(t,out5[,(m11e + 3)], type = 'l', col = col[5])
points(t,out6[,(m11e + 3)], type = 'l', col = col[6])

legend('topright', c('a = 5','a = 2.5', 'a = 1', 'a = 0.5', 'a = 0.1', 'a = 0.01'), lwd = 2, col = col, bty = 'n')
dev.off()
#-------------------------------------------------------------------------------
#plot time series of mRNA abundace for different Er-alpha reaction rates
#half life of the mRna in minutes
tau = 7.736*60
#decay rate from the half life
death = log(2)/tau
beta = 0.001

time = out1[,1]

mRNA1 = detRNA(transEff = beta*sumPol(out1), time = time, death = death)
mRNA2 = detRNA(transEff = beta*sumPol(out2), time = time, death = death)
mRNA3 = detRNA(transEff = beta*sumPol(out3), time = time, death = death)
mRNA4 = detRNA(transEff = beta*sumPol(out4), time = time, death = death)
mRNA5 = detRNA(transEff = beta*sumPol(out5), time = time, death = death)

xlim = c(0,1000)
ylim = c(0,14)
par(mfrow = c(1,1), cex=1.2)

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'TFF1 mRNA abundance for tau = 7.7h at different ER-alpha reaction rates',
      xlab = 'Time (min)', ylab = 'mRNA abundance (a.u.)')

points(mRNA1$timeRNA,mRNA1$RNA, type = 'l', lwd = '2')
points(mRNA2$timeRNA,mRNA2$RNA, type = 'l', col = 'lightblue')
points(mRNA3$timeRNA,mRNA3$RNA, type = 'l', col = 'steelblue')
points(mRNA4$timeRNA,mRNA4$RNA, type = 'l', col = 'royalblue')
points(mRNA5$timeRNA,mRNA5$RNA, type = 'l', col = 'darkblue')

legend(x = 0, y = 14, c('a = 5/min','a = 2.5/min', 'a = 1/min', 'a = 0.5/min', 'a = 0.1/min'), lwd = c(2,2,2,2,2), 
       col = c('black','lightblue', 'steelblue', 'royalblue', 'darkblue'), bty = 'n')

#------------------------------------------------------------------------------
#half life of the mRna in minutes
tau = 10
ylim = c(0,3)
#decay rate from the half life
death = log(2)/tau

time = out1[,1]

mRNA1 = detRNA(transEff = beta*sumPol(out1), time = time, death = death)
mRNA2 = detRNA(transEff = beta*sumPol(out2), time = time, death = death)
mRNA3 = detRNA(transEff = beta*sumPol(out3), time = time, death = death)
mRNA4 = detRNA(transEff = beta*sumPol(out4), time = time, death = death)
mRNA5 = detRNA(transEff = beta*sumPol(out5), time = time, death = death)

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = paste('mRNA abundance for tau =',tau,'min at different ER-alpha reaction rates'),
      xlab = 'Time (min)', ylab = 'mRNA abundance (a.u.)')

points(mRNA1$timeRNA,mRNA1$RNA, type = 'l', lwd = '2')
points(mRNA2$timeRNA,mRNA2$RNA, type = 'l', col = 'lightblue')
points(mRNA3$timeRNA,mRNA3$RNA, type = 'l', col = 'steelblue')
points(mRNA4$timeRNA,mRNA4$RNA, type = 'l', col = 'royalblue')
points(mRNA5$timeRNA,mRNA5$RNA, type = 'l', col = 'darkblue')

legend(x = 0, y = 3, c('a = 5/min','a = 2.5/min', 'a = 1/min', 'a = 0.5/min', 'a = 0.1/min'), lwd = c(2,2,2,2,2), 
       col = c('black','lightblue', 'steelblue', 'royalblue', 'darkblue'), bty = 'n')