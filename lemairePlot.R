xlim = c(40,max(times))
ylim = c(0,4)

par(mfrow = c(1,1))

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = '',
      xlab = 'Time (min)',
      ylab = 'Relative concentration (percent)')

plots = seq(2,100,10)
matpoints(out[,1],out[,plots], type = 'l', lty = 1)
legend('topright', as.character(plots - 1), col = 1:length(plots),lty = 1, bty = 'n')
#------------------------------------------------------------------------------
#summing certain components x_i
#components:
total = 69
m = dim(out)[2]-1

m11 = 1+round(7/total*m)
m12 = 1+round(10/total*m)
ER1 = apply(out[,m11:m12],1,sum)

m21 = 1+round(17/total*m)
m22 = 1+round(27/total*m)
ER2 = apply(out[,m21:m22],1,sum)

m31 = 1+round(31/total*m)
m32 = 1+round(41/total*m)
ER3 = apply(out[,m31:m32],1,sum)

m41 = 1+round(45/total*m)
m42 = 1+round(48/total*m)
ER4 = apply(out[,m41:m42],1,sum)
ER1.4 = apply(out[,m11:m42],1,sum)
m51 = 1+round(62/total*m)
m52 = 1+round(65/total*m)
ER5 = apply(out[,m51:m52],1,sum)
ER1.5 = apply(out[,m11:m52],1,sum)

#Pol II
m11 = 1+round(21/total*m)
m12 = 1+round(27/total*m)
Pol1 = apply(out[,m11:m12],1,sum)

m21 = 1+round(34/total*m)
m22 = 1+round(45/total*m)
Pol2 = apply(out[,m21:m22],1,sum)

#TRIP1
m11 = 1+round(45/total*m)
m12 = 1+round(48/total*m)
TRIP1 = apply(out[,m11:m12],1,sum)

m21 = 1 + round(58/total*m)
m22 = 1 + round(62/total*m)
TRIP2 = apply(out[,m21:m22],1,sum)

#HDAC
m11 = 1+round(52/total*m)
m12 = 1+round(62/total*m)
HDAC = apply(out[,m11:m12],1,sum)
#------------------------------------------------------------------------------
par(mfrow = c(1,1))

xlim = range(out[,1])
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

points(out[,1],ER1+ER2+ER3+ER4+ER5, type = 'l')
points(out[,1],ER1,type = 'l', col = 'orangered')
points(out[,1],ER2,type = 'l',col = 'orangered1')
points(out[,1],ER3,type = 'l',col = 'orangered2')
points(out[,1],ER4,type = 'l',col = 'orangered3')
points(out[,1],ER5,type = 'l',col = 'orangered4')
points(out[,1],ER1.4, type = 'l', col = 'springgreen')
points(out[,1],ER1.5, type = 'l', col = 'orange')
#------------------------------------------------------------------------------
#Pol II
plot(out[,1],Pol1, type = 'l', ylim = ylim, col = 'orangered',
     main = 'Pol II',
     xlab = 'time (min)',
     ylab = '% of bound promoters')
points(out[,1],Pol2,type = 'l', col = 'orangered1')
points(out[,1],Pol1+Pol2, type = 'l')
#------------------------------------------------------------------------------
plot(out[,1],TRIP1, type = 'l', col = 'orangered', ylim = ylim,
     main = 'TRIP1',
     xlab = 'time (min)',
     ylab = '% of bound promoters')
points(out[,1],TRIP2, type = 'l', col = 'orangered')
points(out[,1], TRIP1+TRIP2,type = 'l', col = 'black')