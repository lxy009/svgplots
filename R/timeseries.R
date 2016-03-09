


svg_ts <- function(ys, labels = NULL){
  #make dimensions
  ranges <- list(y = 1.04*diff(range(ys)), x = length(ys)+1)
  extend <- 0.04*diff(range(ys))
  windows <- list(ymin = min(ys)-extend, ymax = max(ys)+extend, xmin = 0, xmax = length(ys)+1)
  
  x_ticks <- x_axis_ticks(ranges$x, labels)
  
  #make mapping
  x_map <- function(old_x){
    return((old_x - windows$xmin)/ranges$x)
  }
  y_map <- function(old_y){
    return((ranges$y - (old_y - windows$ymin))/ranges$y)
  }
  
  return(list(points = list(x = x_map(1:length(ys)), y = y_map(ys)),
              x_tick_ats = x_map(x_ticks$to_tick), x_tick_labels = x_ticks$to_label))
}


x_axis_ticks <- function(x_range, labels = NULL){
  #determine ticks to show based on R pretty
  xs <- 0:x_range
  to_tick <- pretty(xs)
  to_tick <- to_tick[to_tick%%1 == 0] #no decimals
  to_tick <- to_tick[to_tick != 0]
  to_tick <- to_tick[to_tick <= x_range]
  
  idx <- which(xs %in% to_tick)
  if(!is.null(labels)){
    to_label <- labels[idx]
  }else{
    to_label <- NULL
  }
  
  return(list(to_tick = to_tick, to_label = to_label))
}


