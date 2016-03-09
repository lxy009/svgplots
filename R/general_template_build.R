
#make window
calculate_window <- function(x){
  ext_factor <- 0.04 #for now - just use R default
  window_range <- (1+ext_factor+ext_factor)*diff(range(x)) 
  extend <- ext_factor*diff(range(x))
  return(list(min = min(x) - extend, max = max(x) + extend, range = window_range))
}

#make mapping functions into svg_coordinates in 1x1 box, no margin
make_x_mapping <- function(x){#input should be from calculate_window(). a list with min, max, and range
  x_map <- function(old_x){
    return((old_x - x$min)/x$range)  
  }
  return(x_map)
}
make_y_mapping <- function(x){
  y_map <- function(old_y){
    return((x$range - (old_y - x$min))/x$range)
  }
  return(y_map)
}

#general tick position generation
#input should be from calculate_window(). a list with min, max, and range 
#second input should be an appropriate mapping
find_ticks <- function(x, mapping){
  tcks <- pretty(c(x$min,x$max))
  tcks <- tcks[tcks >= x$min & tcks <= x$max]
  svg_tcks <- mapping(tcks)
  return(list(ticks = svg_tcks, labels = tcks))
}


