
svg_bar <- function(ys, labels = NULL){
  
  #build window and mapping
  y_window <- list(min = 0, max = 1.04*max(ys), range = 1.04*max(ys))
  x_window <- list(min = 0, max = length(ys)+1, range = length(ys)+1)
  x_map <- make_x_mapping(x_window)
  y_map <- make_y_mapping(y_window)
  
  #ticks
  y_ticks <- find_ticks(y_window, y_map)
  #special function for time series data
  x_ticks <- bar_x_axis_ticks(x_window$range, labels)
  
  return(list(bars = list(x = x_map(1:length(ys)), y = y_map(ys), hover = ys),
              x_tick_ats = x_map(x_ticks$to_tick), x_tick_labels = x_ticks$to_label,
              y_tick_ats = y_ticks$ticks, y_tick_labels = y_ticks$labels))
}

bar_x_axis_ticks <- function(x_range, labels = NULL){
  #determine ticks to show based on R pretty
  xs <- 0:x_range
  to_tick <- pretty(xs)
  to_tick <- to_tick[to_tick%%1 == 0] #no decimals
  to_tick <- to_tick[to_tick != 0]
  to_tick <- to_tick[to_tick <= x_range]
  
  idx <- which(xs %in% to_tick)-1
  if(!is.null(labels)){
    to_label <- labels[idx]
  }else{
    to_label <- xs[idx+1]
  }
  
  return(list(to_tick = to_tick, to_label = to_label))
}

