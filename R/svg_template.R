
#svg template class

plot.svg_template <- function(x){
  plot.new()
  plot.window(xlim = c(0,1), ylim = c(0,1))
  if(any(names(x) == 'bars')){
    half_bar_width <- 0.5 * x$bars$bar_width
    for(i in seq_along(x$bars$x)){
      rect(xleft = x$bars$x[i] - half_bar_width, xright = x$bars$x[i] + half_bar_width, 
           ybottom = 0, ytop = 1-x$bars$y[i], col = 'grey')
    }
  }
  axis(side = 1, at = x$x_tick_ats, labels = x$x_tick_labels)
  axis(side = 2, at = 1-x$y_tick_ats, labels = x$y_tick_labels)
}

