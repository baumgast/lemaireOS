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
low = 2.5

pars[m11e] = low
pars[m21e] = low
pars[m31e] = low
pars[m41e] = low
pars[m51e] = low

out2   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low = 1

pars[m11e] = low
pars[m21e] = low
pars[m31e] = low
pars[m41e] = low
pars[m51e] = low

out3   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low = 0.5

pars[m11e] = low
pars[m21e] = low
pars[m31e] = low
pars[m41e] = low
pars[m51e] = low

out4   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low = 0.1

pars[m11e] = low
pars[m21e] = low
pars[m31e] = low
pars[m41e] = low
pars[m51e] = low

out5   = ode(func = lemaire, y = yini, parms = pars, times = times)
#-------------------------------------------------------------------------------

#summing certain components x_i
#components:
sumComponents = function(out) {
  total = 69
  m = dim(out)[2]-1
  
  m11e = 1+round(7/total*m)
  m12e= 1+round(10/total*m)
  ER1 = apply(out[,m11e:m12e],1,sum)
  
  m21e = 1+round(17/total*m)
  m22e = 1+round(27/total*m)
  ER2 = apply(out[,m21e:m22e],1,sum)
  
  m31e = 1+round(31/total*m)
  m32e = 1+round(41/total*m)
  ER3 = apply(out[,m31e:m32e],1,sum)
  
  m41e = 1+round(45/total*m)
  m42e = 1+round(48/total*m)
  ER4 = apply(out[,m41e:m42e],1,sum)
  ER1.4 = apply(out[,m11e:m42e],1,sum)
  m51e = 1+round(62/total*m)
  m52e = 1+round(65/total*m)
  ER5 = apply(out[,m51e:m52e],1,sum)
  ER1.5 = apply(out[,m11e:m52e],1,sum)

  return(ER1+ER2+ER3+ER4+ER5)
}
sumPol = function(out) {
  total = 69
  m = dim(out)[2]-1
  
  m11 = 1+round(21/total*m)
  m12 = 1+round(27/total*m)
  Pol1 = apply(out[,m11:m12],1,sum)
  
  m21 = 1+round(34/total*m)
  m22 = 1+round(45/total*m)
  Pol2 = apply(out[,m21:m22],1,sum)
  
  return(Pol1+Pol2)
}
#-------------------------------------------------------------------------------
#calculate the mRNA content over time deterministically
detRNA = function(transEff, time, death){
  library(deSolve)
  
  #deterministic, via ode: dRNA = beta(t)(1-exp(-alpha*t))
  #transcription rate beta, decay rate alpha
  
  mRNA = function(Time,State,Pars) {
    with(as.list(c(State,Pars)), {
      
      dRNA = beta - alpha*RNA
      
      return(list(c(dRNA)))
    })
  }
  
  yini   = c(RNA = 0)
  deltaT = diff(time)
  RNA    = vector()
  timeRNA = vector()
  
  for (i in 1:length(deltaT)) {
    td  = seq(time[i],time[i+1],0.05)
    pars = c(alpha = death, beta = transEff[i])
    
    rna = ode(func = mRNA, parms = pars, time = td, y = yini)  
    
    yini = rna[length(rna[,2]),2]
    
    timeRNA = c(timeRNA,td)
    RNA = c(RNA,rna[,2])
  }
  return(data.frame(timeRNA,RNA))
}

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