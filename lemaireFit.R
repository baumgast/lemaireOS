#adjustment of the lemaire model to george`s data,
#calculate for all possible combinations of complexes (sum of x_i) the squared 
#difference to the data by applying a nested loop
#try ER alpha first
#-------------------------------------------------------------------------------

Time = seq(0,180,5)
Era = c(0,2,6,11,18,5,1,6,17,33,51,65,65,57,31,14,13,26,60,73,70,58,28,19,23,29,55,72,72,58,39,29,39,51,62,64,61)

plot(Time,Era, type = 'l',
     xlab = 'Time (min)',
     ylab = '% of bound promoters',
     ylim = c(0,100))
grid()
points(Time,Era, pch = 19)

m = dim(out)[2] - 1
t = Time[which(Time > 35)]
Match = match(out[,1], t)
rows = which(!is.na(Match))

count = 0
for (i in 1:m) {
  for (j in 1:(m-i)) {
    count = count + 1
  }
}