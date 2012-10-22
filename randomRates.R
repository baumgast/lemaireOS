#monte carlo sampling of reaction rates

library(deSolve)
source('~/Desktop/lemaireOS/lemaireOscillation.R')

zero = c(x = vector(length = 200)*0)
zero[1] = 100
yini = zero
times = seq(0,1000,0.5)

#sample all reaction rates from a normal distribution
set.seed(2)
A = vector(length = 200)+runif(200,0.1,10)

#solve for all rates fixed at a = 5
pars  = c(a = A)

outMC   = ode(func = lemaire, y = yini, parms = pars, times = times)

t = outMC[,1]
ERa = sumComponents(outMC)
xlim = c(40,1000)
ylim = c(0,100)

plot(t,ERa, type = 'l', xlim = xlim, ylim = ylim,
     main = 'time courses for random reaction rates',
     xlab = 'Time (min)',
     ylab = '% bound promoters')
points(t,sumComponents(out1), type = 'l', col = 'orange')
points(t,sumPol(out1), type = 'l', col = 'lightblue')
points(t,sumPol(outMC), type = 'l', col = 'royalblue')

plot(sumComponents(outMC), sumPol(outMC), type = 'l')

hist(pars, breaks = seq(0,10,1), col = 'grey', border = 'white', probability = T)
lines(density(pars))
