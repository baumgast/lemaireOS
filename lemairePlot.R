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
par(mfrow = c(1,1))
m = dim(out)[2]-1

m1 = 1+round(m/8)
m2 = 1+round(m*2/3)

ER1 = apply(out[,m1:m2],1, sum)

plot(out[,1], ER1, type = 'l',
     xlab = 'Time (min)',
     ylab = '% of bound promoters')
#legend('topright', c('ER-alpha',paste('complexes m1 =',m1,'to m2 =',m2)), bty = 'n')
#-------------------------------------------------------------------------------
m1 = 1+round(m/8)
m2 = 1+round(m*3/8)

ER2 = apply(out[,m1:m2],1, sum)

points(out[,1],ER2, type = 'l',col = 'red',
     xlab = 'Time (min)',
     ylab = '% of bound promoters')
#legend('topright', c('ER-alpha',paste('complexes m1 =',m1,'to m2 =',m2)), bty = 'n')
#-------------------------------------------------------------------------------
m1 = 1+round(m*3/7)
m2 = 1+round(m*2/3)

ER3 = apply(out[,m1:m2],1, sum)

points(out[,1],ER3, type = 'l',col = 'blue',
     xlab = 'Time (min)',
     ylab = '% of bound promoters')
#legend('topright', c('ER-alpha',paste('complexes m1 =',m1,'to m2 =',m2)), bty = 'n')
#-------------------------------------------------------------------------------
points(out[,1],ER3+ER2, type = 'l', col = 'orange')