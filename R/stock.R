


svg_stock <- function(values, volumes = NULL, labels = NULL, bar_spacing = 0.2, buffer = 0.04){
  
  #build two blocks of the graph - split graph in half
  #both blocks use same x-axi
  x_window <- list(min = 0, max = length(values)+1, range = length(values)+1)
  x_map <- make_x_mapping(x_window)
  
  #build window and mapping for both bocks
  y_win1 <- list(min = 0, max = 1.04*max(values), range = 1.04*max(values)) 
  y_map1 <- make_y_mapping(y_win1)
  
  #ticks
  
  #special function for time series data
  x_ticks <- x_axis_ticks(x_window$range, labels)
  
  value_xs <- x_map(1:length(values))
  base_structure <- list(x_tick_ats = x_map(x_ticks$to_tick), x_tick_labels = x_ticks$to_label)
  if(is.null(volumes)){
    value_ys <- y_map1(values) 
    y_tcks_1 <- find_ticks(y_win1, y_map1)
  }
  if(!is.null(volumes)){
    value_ys <- y_map1(values) * (0.5 - (buffer/2))
    y_tcks_1 <- find_ticks(y_win1, y_map1)
    y_tcks_1$ticks <- y_tcks_1$ticks * (0.5 - (buffer/2))
    
    y_win2 <- list(min = 0, max = 1.04*max(volumes), range = 1.04*max(volumes))  
    y_map2 <- make_y_mapping(y_win2)
    value_2s <- (y_map2(volumes) * (0.5 - (buffer/2))) + (0.5 + (buffer/2))
    y_tcks_2 <- find_ticks(y_win2, y_map2)
    y_tcks_2$ticks <- (y_tcks_2$ticks * (0.5 - (buffer/2))) + (0.5 + (buffer/2))
    
    N_bars <- length(value_xs)
    bar_width <- 1/(N_bars + ((N_bars+1) * bar_spacing))
    base_structure[['bars']] <- list(x = value_xs, y = value_2s, bar_width = bar_width)
    base_structure[['y_tick2_ats']] = y_tcks_2$ticks
    base_structure[['y_tick2_labels']] = y_tcks_2$labels
  }
  base_structure[['points']] <- list(x = value_xs, y = value_ys)
  base_structure[['lines']] <- list(x = value_xs, y = value_ys)
  base_structure[['y_tick_ats']] = y_tcks_1$ticks
  base_structure[['y_tick_labels']] = y_tcks_1$labels
  
  return(structure(base_structure, class = 'svg_template'))
}

make_svg_stock <- function(x, width = 600, height = 400){
  points <- paste("<points x='",x$points$x*width)
}

#ALREADY DEFINED IN TIMESERIES.R!!!!
# x_axis_ticks <- function(x_range, labels = NULL){
#   #determine ticks to show based on R pretty
#   xs <- 0:x_range
#   to_tick <- pretty(xs)
#   to_tick <- to_tick[to_tick%%1 == 0] #no decimals
#   to_tick <- to_tick[to_tick != 0]
#   to_tick <- to_tick[to_tick <= x_range]
#   
#   idx <- which(xs %in% to_tick)-1
#   if(!is.null(labels)){
#     to_label <- labels[idx]
#   }else{
#     to_label <- xs[idx+1]
#   }
#   
#   return(list(to_tick = to_tick, to_label = to_label))
# }


