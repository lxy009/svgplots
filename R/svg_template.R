
#svg template class

dev_plot_svg_template <- function(x){
  plot.new()
  plot.window(xlim = c(0,1), ylim = c(0,1)) 
  for(geom in x$geom){
    if(geom$elem == 'rect'){
      rect(xleft = geom$x, xright = geom$x + geom$width, 
           ytop = 1 - geom$y, ybottom = 1-(geom$y+geom$height), col='grey')
    } 
  }
  if(any(names(x) == 'x_axis')){
    axis(side = 1, at = x$x_axis$x_tick_ats, label = x$x_axis$x_tick_labels)
  }
  if(any(names(x) == 'y_axis')){
    axis(side = 2, at = 1-x$y_axis$y_tick_ats, label = x$y_axis$y_tick_labels)
  }
}

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
  if(any(names(x) == 'points')){
    points(x = x$points$x, y = 1-x$points$y)
  }
  if(any(names(x) == 'lines')){
    lines(x = x$points$x, y = 1-x$points$y)
  }
  if(!any(names(x) %in% c('gauge', 'donut'))){
    if(any(names(x) == 'x_tick_ats')){
      if(any(names(x) == 'x_tick_labels')){
        axis(side = 1, at = x$x_tick_ats, labels = x$x_tick_labels)  
      }else{
        axis(side = 1, at = x$x_tick_ats)
      }
    }
    if(any(names(x) == 'y_tick_ats')){
      if(any(names(x) == 'y_tick_labels')){
        axis(side = 2, at = 1-x$y_tick_ats, labels = x$y_tick_labels)   
      }else{
        axis(side = 2, at = 1-x$y_tick_ats)
      }
    }
    if(any(names(x) == 'y_tick2_ats')){
      if(any(names(x) == 'y_tick2_labels')){
        axis(side = 2, at = 1-x$y_tick2_ats, labels = x$y_tick2_labels)   
      }else{
        axis(side = 2, at = 1-x$y_tick2_ats)
      }
    }
  }else{
    if(any(names(x) == 'gauge')){ #gauge really isn't compatible with other graphs
      N <- 1E3
      thetas <- seq(from=pi-x$gauge$ang,to=pi,length.out=N)
      xs <- x$gauge$r[1] * cos(thetas) + x$gauge$cx
      ys <- x$gauge$r[1] * sin(thetas) + (1-x$gauge$cy)
      xs <- c(xs,x$gauge$cx,xs[1])
      ys <- c(ys,(1-x$gauge$cy),ys[1])
      polygon(x = xs, y = ys, col = 'blue', border = 'white')
      xs <- x$gauge$r[2] * cos(thetas) + x$gauge$cx
      ys <- x$gauge$r[2] * sin(thetas) + (1-x$gauge$cy)
      xs <- c(xs,x$gauge$cx,xs[1])
      ys <- c(ys,(1-x$gauge$cy),ys[1])
      polygon(x = xs, y = ys, col = 'white', border = 'white')
    }   
    if(any(names(x) == 'donut')){ #gauge really isn't compatible with other graphs
      N <- 1E3
      thetas_beg <- c(0,cumsum(head(x$donut$angles,-1)))
      thetas_end <- cumsum(x$donut$angles)
      if(any(names(x$donut) == 'col')){
        my_col <- x$donut$col
      }else{
        my_col <- rainbow(length(thetas_beg))
      }
      r1 <- x$donut$r[1]
      r2 <- x$donut$r[2]
      for(i in seq_along(thetas_beg)){
        thetas <- seq(from=thetas_beg[i],to=thetas_end[i],length.out=N)
        xs <- r1 * cos(thetas) + x$donut$cx
        ys <- r1 * sin(thetas) + (1-x$donut$cy)
        thetas <- rev(thetas)
        xs <- c(xs, r2 * cos(thetas) + x$donut$cx, xs[1])
        ys <- c(ys, r2 * sin(thetas) + (1-x$donut$cy), ys[1]) 
        polygon(x = xs, y = ys, col = my_col[i], border = 'white')
      }

    }  
  }
}

