#lowering reaction rates selectively in accordance to the blocks, were ERa is involved

library(deSolve)
source('~/Desktop/lemaireOS/lemaireOscillation.R')

zero = c(x = vector(length = 200)*0)
zero[1] = 100
yini = zero
times = seq(0,1000,0.5)

#define reaction rates, initially set all 5 min^-1
A = vector(length = 200)+5

#-------------------------------------------------------------------------------
#solve for all rates fixed at a = 5
pars  = c(a = A)

out1   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------
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
low  = 2.5
high = A[1]# + A[1]/low

pars[m11e] = low
pars[m12e] = high
pars[m21e] = low
pars[m22e] = high
pars[m31e] = low
pars[m32e] = high
pars[m41e] = low
pars[m42e] = high
pars[m51e] = low
pars[m52e] = high

out2   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low = 1
high = A[1]# + A[1]/low

pars[m11e] = low
pars[m12e] = high
pars[m21e] = low
pars[m22e] = high
pars[m31e] = low
pars[m32e] = high
pars[m41e] = low
pars[m42e] = high
pars[m51e] = low
pars[m52e] = high

out3   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low = 0.5
high = A[1]# + A[1]/low

pars[m11e] = low
pars[m12e] = high
pars[m21e] = low
pars[m22e] = high
pars[m31e] = low
pars[m32e] = high
pars[m41e] = low
pars[m42e] = high
pars[m51e] = low
pars[m52e] = high

out4   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low = 0.1
high = A[1]# + A[1]/low

pars[m11e] = low
pars[m12e] = high
pars[m21e] = low
pars[m22e] = high
pars[m31e] = low
pars[m32e] = high
pars[m41e] = low
pars[m42e] = high
pars[m51e] = low
pars[m52e] = high

out5   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low = 0.01
high = A[1]# + A[1]/low

pars[m11e] = low
pars[m12e] = high
pars[m21e] = low
pars[m22e] = high
pars[m31e] = low
pars[m32e] = high
pars[m41e] = low
pars[m42e] = high
pars[m51e] = low
pars[m52e] = high

out6   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------


#half life of the mRna
tau = 420
#decay rate from the half life
death = log(2)/tau
time = out1[,1]

mRNA1 = detRNA(transEff = 0.01*sumPol(out1), time = time, death = death)
mRNA2 = detRNA(transEff = 0.01*sumPol(out2), time = time, death = death)
mRNA3 = detRNA(transEff = 0.01*sumPol(out3), time = time, death = death)
mRNA4 = detRNA(transEff = 0.01*sumPol(out4), time = time, death = death)
mRNA5 = detRNA(transEff = 0.01*sumPol(out5), time = time, death = death)