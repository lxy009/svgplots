
#functions for bar plots

svg_bar <- function(ys, labels = NULL, spacing = 0.2){
  
  N_bars <- length(ys)
  
  #build window and mapping
  y_window <- list(min = 0, max = 1.04*max(ys), range = 1.04*max(ys))
  # length(ys) * bar_width + (length(ys)+1) * spacing * bar_width = 1
  bar_width <- 1/(N_bars + ((N_bars+1) * spacing))
  x_window <- list(min = 0.5, max = N_bars + 0.5, range =  N_bars)
  x_map <- make_x_mapping(x_window)
  y_map <- make_y_mapping(y_window)
  
  #ticks
  y_ticks <- find_ticks(y_window, y_map)
  #special function for time series data
  x_ticks <- bar_x_axis_ticks(x_window$range, labels)
  
  return(structure(list(bars = list(x = x_map(1:length(ys)), y = y_map(ys), hover = ys, 
                                    bar_width = bar_width),
              x_tick_ats = x_map(x_ticks$to_tick), x_tick_labels = x_ticks$to_label,
              y_tick_ats = y_ticks$ticks, y_tick_labels = y_ticks$labels), class='svg_template'))
}

dev_svg_barplot <- function(ys, labels = NULL, spacing = 0.2){
  N_bars <- length(ys)
  
  #build window and mapping
  y_window <- list(min = 0, max = 1.04*max(ys), range = 1.04*max(ys))
  # length(ys) * bar_width + (length(ys)+1) * spacing * bar_width = 1
  bar_width <- 1/(N_bars + ((N_bars+1) * spacing))
  x_window <- list(min = 0.5, max = N_bars + 0.5, range =  N_bars)
  x_map <- make_x_mapping(x_window)
  y_map <- make_y_mapping(y_window)
  
  #ticks
  y_ticks <- find_ticks(y_window, y_map)
  #special function for bar plot
  x_ticks <- bar_x_axis_ticks(x_window$range, labels)
  
  geom <- mapply(FUN = function(x, y, width){
    return(list(elem = 'rect', x = x, y = y, width = width, height = 1-y))
  },x = x_map(1:length(ys)), y = y_map(ys), MoreArgs = list(width = bar_width), SIMPLIFY = F)
  
  
  return(structure(list(geom = geom,
                        x_axis = list(x_tick_ats = x_map(x_ticks$to_tick), x_tick_labels = x_ticks$to_label),
                        y_axis = list(y_tick_ats = y_ticks$ticks, y_tick_labels = y_ticks$labels) 
                        ),class='svg_template')) 
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
  
  #adjust for x_tick to be in middle
  #list(min = 0.5, max = N_bars + 0.5, range =  N_bars)
  to_tick <- to_tick+0.5
  
  return(list(to_tick = to_tick, to_label = to_label))
}


dev_make_svg_bars <- function(rel_coods, width, height, margin, font_size, svg_class = NULL, style = NULL){
  
  #length by height of plotting window (figure dimension sans margin)
  svg_win_dim <- c( width - (font_size*(margin[2]+margin[4])), height - (font_size*(margin[1]+margin[3])) )
  
  #bar dimensions
  bar_width <- rel_coods$bar_width * svg_win_dim[1]
  bar_height <- (1 - rel_coods$y) *svg_win_dim[2]
  
  #coordinates for rect element top left corner
  svg_coods_xs <- (rel_coods$x * svg_win_dim[1]) + (margin[2] * font_size)
  svg_coods_xs <- svg_coods_xs - (0.5 * bar_width)
  svg_coods_ys <- (rel_coods$y * svg_win_dim[2]) + (margin[3] * font_size)
  
  #return as class = svg_instance_bars
  to_return <- list()
  for(i in seq_along(svg_coods_xs)){
    to_return[[i]] <- list(
      id = paste0('bar',i),
      class = svg_class,
      style = style,
      x = svg_coods_xs[i],
      y = svg_coods_ys[i],
      width = bar_width,
      height = bar_height[i],
      hover_text = rel_coods$hover[i],
      hover_x = svg_coods_xs[i] + (0.5*bar_width),
      hover_y = svg_coods_ys[i],
      hover_class = NULL,
      hover_id = paste0('bar_info',i),
      hover_font_size = font_size,
      hover_translate = c(0,-0.02*min(c(width,height)))
    )
  }
  
  return(structure(to_return, class='svg_instance_bars'))
}

dev_make_svg_bars2 <- function(bar_info, x_map, y_map, plot_length, plot_height,
                               svg_class = NULL, style = NULL){
  #bar dimensions
  bar_width <- bar_info$bar_width * plot_length
  bar_height <- (1-bar_info$y) * plot_height
  
  #coordinates for rect element top left corner
  svg_coods_xs <- x_map(bar_info$x)
  svg_coods_xs <- svg_coods_xs - (0.5 * bar_width)
  svg_coods_ys <- y_map(bar_info$y)
  
  to_return <- list()
  for(i in seq_along(svg_coods_xs)){
    to_return[[i]] <- list(
      id = paste0('bar',i),
      class = svg_class,
      style = style,
      x = svg_coods_xs[i],
      y = svg_coods_ys[i],
      width = bar_width,
      height = bar_height[i]
    )
  }
  return(structure(to_return, class='svg_instance_rect'))
}











