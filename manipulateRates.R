#lowering reaction rates selectively in accordance to the blocks, were ERa is involved

library(deSolve)

library(deSolve)
zero = c(x = vector(length = 200)*0)
zero[1] = 100
yini = zero
times = seq(0,1000,0.5)

#define reaction rates, initially set all 5 min^-1
A = vector(length = 200)+5
pars  = c(a = A)

#lower rate
low = 0.5

pars[m11e] = low
pars[m21e] = low
pars[m31e] = low
pars[m41e] = low
pars[m51e] = low

out   = ode(func = lemaire, y = yini, parms = pars, times = times)

source('~/Desktop/lemaireOS/lemairePlot.R')