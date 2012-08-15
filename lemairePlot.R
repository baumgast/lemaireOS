xlim = c(0,max(times))
ylim = c(0,12)

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