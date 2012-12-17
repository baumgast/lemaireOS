#monte carlo sampling of reaction rates

library(deSolve)
source('~/Desktop/lemaireOS/lemaireOscillation.R')

zero = c(x = vector(length = 200)*0)
zero[1] = 100
yini = zero
times = seq(0,1000,0.5)

#sample all reaction rates from a normal distribution
set.seed(2)
A = vector(length = 200)+rlnorm(200,log(5),0.5)

#solve for all rates fixed at a = 5
pars  = c(a = A)

outMC   = ode(func = lemaire, y = yini, parms = pars, times = times)

t = outMC[,1]
ERa = sumComponents(outMC)
xlim = c(40,180)
ylim = c(0,100)
pdf('~/Desktop/lemaireOS/Figues/randomRates.pdf', height = 8.3, width = 11.7)
#par(mfrow = c(1,2))
layout(matrix(c(1,1,2,3),2,2, byrow = T),widths = c(2,1))
plot(t,ERa, type = 'l', xlim = xlim, ylim = ylim,lwd = 2,
     main = 'time courses for random reaction rates',
     xlab = 'Time (min)',
     ylab = '% bound promoters')
grid()
points(t,sumComponents(out1), type = 'l', col = 'gray', lwd = 1)
points(t,sumPol(outMC), type = 'l', col = 'royalblue', lwd = 2)
points(t,sumPol(out1), type = 'l', col = 'lightblue', lwd = 1)

legend('topright', c('ER-alpha, random rates','Er-alpha, a = 5','Pol II, random rates','Pol II, a = 5'), lwd = 2,
       col = c('black','gray','royalblue','lightblue'), bty = 'n')

#plot(sumComponents(outMC), sumPol(outMC), type = 'l')
xlim = c(0,1000)
ylim = c(0,10)

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
matpoints(outMC[,1],outMC[,plots], type = 'l', lty = 1, col = col)

hist(pars, breaks = seq(0,max(pars)+1,1), col = 'grey', border = 'white', probability = F)
#lines(density(pars))
dev.off()


# tau = 7.736*60
# 
# death = log(2)/tau
# beta = 0.001
# mRNAmc = detRNA(transEff = beta*sumPol(outMC), time = t, death = death)
# mRNAa5 = detRNA(transEff = beta*sumPol(out1), time = t, death = death)
# 
# plot(mRNAmc, type = 'l')
# points(mRNAa5, type = 'l', col = 'lightblue')
#define the rates that are the first in a block of Er-alpha incorporating complexes
total = 69
m = dim(out1)[2]-1

m11e = 1+round(7/total*m)
m12e= 1+round(10/total*m)

m21e = 1+round(17/total*m)
m22e = 1+round(27/total*m)

m31e = 1+round(31/total*m)
m32e = 1+round(41/total*m)

m41e = 1+round(45/total*m)
m42e = 1+round(48/total*m)

m51e = 1+round(62/total*m)
m52e = 1+round(65/total*m)
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low  = 0.1\,mnbv==
high = A[1]# + A[1]/low

pars[m11e] = low
#pars[m12e] = high
pars[m21e] = low
#pars[m22e] = high
pars[m31e] = low
#pars[m32e] = high
pars[m41e] = low
#pars[m42e] = high
pars[m51e] = low
#pars[m52e] = high

outMC1   = ode(func = lemaire, y = yini, parms = pars, times = times)

ylim = c(0,20)
xlim = c(0,1000)

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
matpoints(outMC1[,1],outMC1[,plots], type = 'l', lty = 1, col = col)
