pdf('~/Desktop/lemaireOS/Figues/allReactions.pdf', height = 8.3, width = 11.7)
par(mfrow = c(2,3), cex = 1.0)
xlim = c(0,1000)
ylim = c(0,20)

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'a = 5',
      xlab = 'Time (min)',
      ylab = 'Relative concentration (percent)')

plots = seq(2,200,1)
col = rainbow(length(plots))
matpoints(out1[,1],out1[,plots], type = 'l', lty = 1, col = col)

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'a = 2.5',
      xlab = 'Time (min)',
      ylab = 'Relative concentration (percent)')

plots = seq(2,200,1)
col = rainbow(length(plots))
matpoints(out1[,1],out2[,plots], type = 'l', lty = 1, col = col)

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'a = 1',
      xlab = 'Time (min)',
      ylab = 'Relative concentration (percent)')

plots = seq(2,200,1)
col = rainbow(length(plots))
matpoints(out1[,1],out3[,plots], type = 'l', lty = 1, col = col)

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'a = 0.5',
      xlab = 'Time (min)',
      ylab = 'Relative concentration (percent)')

plots = seq(2,200,1)
col = rainbow(length(plots))
matpoints(out1[,1],out4[,plots], type = 'l', lty = 1, col = col)

ylim = c(0,30)
plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'a = 0.1',
      xlab = 'Time (min)',
      ylab = 'Relative concentration (percent)')

plots = seq(2,200,1)
col = rainbow(length(plots))
matpoints(out1[,1],out5[,plots], type = 'l', lty = 1, col = col)

ylim = c(0,130)
plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'a = 0.01',
      xlab = 'Time (min)',
      ylab = 'Relative concentration (percent)')

plots = seq(2,200,1)
col = rainbow(length(plots))
matpoints(out1[,1],out6[,plots], type = 'l', lty = 1, col = col)
#legend('topright', as.character(plots - 1), col = 1:length(plots),lty = 1, bty = 'n')
dev.off()
#------------------------------------------------------------------------------
#summing certain components x_i
#components:
total = 69
m = dim(out1)[2]-1

m11e = 1+round(7/total*m)
m12e= 1+round(10/total*m)
ER1 = apply(out1[,m11e:m12e],1,sum)

m21e = 1+round(17/total*m)
m22e = 1+round(27/total*m)
ER2 = apply(out1[,m21e:m22e],1,sum)

m31e = 1+round(31/total*m)
m32e = 1+round(41/total*m)
ER3 = apply(out1[,m31e:m32e],1,sum)

m41e = 1+round(45/total*m)
m42e = 1+round(48/total*m)
ER4 = apply(out1[,m41e:m42e],1,sum)
ER1.4 = apply(out1[,m11e:m42e],1,sum)
m51e = 1+round(62/total*m)
m52e = 1+round(65/total*m)
ER5 = apply(out1[,m51e:m52e],1,sum)
ER1.5 = apply(out1[,m11e:m52e],1,sum)

#Pol II
m11 = 1+round(21/total*m)
m12 = 1+round(27/total*m)
Pol1 = apply(out1[,m11:m12],1,sum)

m21 = 1+round(34/total*m)
m22 = 1+round(45/total*m)
Pol2 = apply(out1[,m21:m22],1,sum)

#TRIP1
m11 = 1+round(45/total*m)
m12 = 1+round(48/total*m)
TRIP1 = apply(out1[,m11:m12],1,sum)

m21 = 1 + round(58/total*m)
m22 = 1 + round(62/total*m)
TRIP2 = apply(out1[,m21:m22],1,sum)

#HDAC
m11 = 1+round(52/total*m)
m12 = 1+round(62/total*m)
HDAC = apply(out1[,m11:m12],1,sum)
#------------------------------------------------------------------------------
par(mfrow = c(4,1))

xlim = c(40,180)#range(out1[,1])[2])
ylim = c(0,100)

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'ER-alpha',
      xlab = 'Time (min)',
      ylab = '% bound promoters')

points(out1[,1],ER1+ER2+ER3+ER4+ER5, type = 'l')
points(out1[,1],ER1,type = 'l', col = 'orangered')
points(out1[,1],ER2,type = 'l',col = 'orangered1')
points(out1[,1],ER3,type = 'l',col = 'orangered2')
points(out1[,1],ER4,type = 'l',col = 'orangered3')
points(out1[,1],ER5,type = 'l',col = 'orangered4')
points(out1[,1],ER1.4, type = 'l', col = 'springgreen')
points(out1[,1],ER1.5, type = 'l', col = 'orange')
#------------------------------------------------------------------------------
#Pol II
plot(out1[,1],Pol1, type = 'l', ylim = ylim, col = 'orangered',
     main = 'Pol II',
     xlab = 'time (min)',
     ylab = '% of bound promoters')
grid()
points(out1[,1],Pol2,type = 'l', col = 'orangered1')
points(out1[,1],Pol1+Pol2, type = 'l')
#------------------------------------------------------------------------------
plot(out1[,1],TRIP1, type = 'l', col = 'orangered', ylim = ylim,
     main = 'TRIP1',
     xlab = 'time (min)',
     ylab = '% of bound promoters')
grid()
points(out1[,1],TRIP2, type = 'l', col = 'orangered')
points(out1[,1], TRIP1+TRIP2,type = 'l', col = 'black')
#------------------------------------------------------------------------------
#HDAC
plot(out1[,1],HDAC,type = 'l', ylim = ylim,
     main = 'TRIP1',
     xlab = 'time (min)',
     ylab = '% of bound promoters')
grid()
#------------------------------------------------------------------------------
par(mfrow = c(1,1), cex = 1.2)

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'Time courses as in Lemaire et al.',
      xlab = 'Time (min)',
      ylab = '% bound promoters')

points(out1[,1],ER1+ER2+ER3+ER4+ER5, type = 'l')
points(out1[,1],Pol2+Pol1,type = 'l', col = 'orangered1')
points(out1[,1], TRIP1+TRIP2,type = 'l', col = 'skyblue4')
points(out1[,1],HDAC,type = 'l', col = 'magenta')

legend(x = 150,y = 100, c('ER-alpha','Pol II','TRIP1','HDAC'), lty = c(1,1,1,1),
       col = c('black','orangered1','skyblue4','magenta'), bty = 'n')
#-------------------------------------------------------------------------------
