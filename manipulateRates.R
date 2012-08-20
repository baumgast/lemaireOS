#lowering reaction rates selectively in accordance to the blocks, were ERa is involved

library(deSolve)

library(deSolve)
zero = c(x = vector(length = 200)*0)
zero[1] = 100
yini = zero
times = seq(0,1000,0.5)

#define reaction rates, initially set all 5 min^-1
A = vector(length = 200)+5
#-------------------------------------------------------------------------------
pars  = c(a = A)

#lower rate
low = 5

pars[m11e] = low
pars[m21e] = low
pars[m31e] = low
pars[m41e] = low
pars[m51e] = low

out1   = ode(func = lemaire, y = yini, parms = pars, times = times)
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
  m11 = 1+round(21/total*m)
  m12 = 1+round(27/total*m)
  Pol1 = apply(out[,m11:m12],1,sum)
  
  m21 = 1+round(34/total*m)
  m22 = 1+round(45/total*m)
  Pol2 = apply(out[,m21:m22],1,sum)
  
  return(Pol1+Pol2)
}
#-------------------------------------------------------------------------------
