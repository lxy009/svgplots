

# x should be class svg_template
# all dimensions defined in px
make_svg_instance <- function(x,width,height,margin=NULL,scale=1,classes=NULL){
  
  to_ret <- list()
  to_ret$template <- x
  to_ret$width <- width
  to_ret$height <- height
  
  #calculate margin and dependencies
  if(is.null(margin)){
    char_size <- 16 # default font size of 16 px
    margin <- ceiling(par('mar') * char_size) #use R's current graphics margin
  }
  to_ret$line_size <- char_size
  to_ret$margin <- margin
  plot_length <- width - margin[1] - margin[3]
  plot_height <- height - margin[2] - margin[4]
  
  #define map from svg template coordinates (scaled to 0-1) to svg instance coordinate
  x_map <- function(x){
    return(margin[1] + plot_length*x)
  }
  y_map <- function(y){
    return(margin[4] + plot_height*y)
  }
  to_ret$x_map <- x_map
  to_ret$y_map <- y_map
  
  to_ret$geom <- x$geom
  for(i in seq_along(to_ret$geom)){
    if(to_ret$geom[[i]]$elem == 'rect'){
      to_ret$geom[[i]]$x <- x_map(to_ret$geom[[i]]$x)
      to_ret$geom[[i]]$y <- y_map(to_ret$geom[[i]]$y)
      to_ret$geom[[i]]$width <- to_ret$geom[[i]]$width * plot_length
      to_ret$geom[[i]]$height<- to_ret$geom[[i]]$height* plot_height
      if(is.null(classes)){
        to_ret$geom[[i]]$style <- 'fill:blue;'
      }
    }
  }
  
  if(any(names(x) == 'x_axis')){
    axis_list <- list()
    x_axis_at <- height-margin[2]+0.1*char_size
    axis_list$line <- list(x1 = x_map(x$x_axis$x_tick_ats[1]), x2 = x_map(tail(x$x_axis$x_tick_ats,1)), 
                           y1 = x_axis_at, y2 = x_axis_at)
    xs <- x_map(x$x_axis$x_tick_ats)
    axis_list$tick <- list(x1 = xs, x2 = xs, y1 = x_axis_at, y2 = x_axis_at + 0.5*char_size, 
                           y_lab = x_axis_at + char_size, label = x$x_axis$x_tick_labels)
    to_ret$x_axis <- axis_list
  }
  
  if(any(names(x) == 'y_axis')){
    axis_list <- list()
    y_axis_at <- margin[1]-0.1*char_size
    axis_list$line <- list(x1 = y_axis_at, x2 = y_axis_at, 
                           y1 = y_map(x$y_axis$y_tick_ats[1]), y2 = y_map(tail(x$y_axis$y_tick_ats,1)))
#     xs <- x_map(x$x_axis$x_tick_ats)
#     axis_list$tick <- list(x1 = xs, x2 = xs, y1 = x_axis_at, y2 = x_axis_at + 0.5*char_size, 
#                            y_lab = x_axis_at + char_size, label = x$x_axis$x_tick_labels)
    to_ret$y_axis <- axis_list
  }
  
  return(to_ret)
}

dev_write_svg_instance <- function(x){
  #elements first
  svg_code <- paste0(sapply(X = x$geom, FUN = dev_write_a_geom_element), collapse="\n")
  #axes next
  svg_code <- paste(svg_code, make_axes_code(x),sep="\n")
  svg_code <- paste(paste("<svg viewBox='0 0",x$width,x$height,"'>"), svg_code, "</svg>", sep="\n")
  return(svg_code)
}

dev_write_a_geom_element <- function(x){
  tag <- x[[1]]
  attr_list <- x[-1]
  attr_code <- paste0(names(attr_list),"='",attr_list,"'", collapse=" ")
  code <- paste0("<",tag," ",attr_code," />")
  return(code)
}

svg_line_code <- function(x1,x2,y1,y2,class=NULL, style=NULL){
  input <- list(elem = 'line', x1 = x1, x2 = x2, y1 = y1, y2 = y2)
  if(is.null(class)){
    if(is.null(style)){
      input$stroke <- 'grey'
      input$`stroke-width` <- 1
    }
  }
  return(dev_write_a_geom_element(input))
}

write_a_tag_element <- function(x){
  attr_list <- x[-c(1,2)]
  tag <- x[[1]]
  value <- x[[2]]
  attr_code <- paste0(names(attr_list),"='",attr_list,"'", collapse=" ")
  code <- paste0("<",tag," ",attr_code," >",value,"</",tag,">")
  return(code)
}

write_a_text_element <- function(x,y,value, text_anchor='middle'){
  return(write_a_tag_element(list('text',value, x = x, y = y, "text-anchor" = text_anchor,
                                  'style' = 'alignment-baseline:before-edge;')))
}

make_axes_code <- function(x){
  x_axis <- x$x_axis
  to_ret <- svg_line_code(x_axis$line$x1,x_axis$line$x2,x_axis$line$y1,x_axis$line$y2)
  x_tick_code <- paste0(mapply(FUN = svg_line_code, x1 = x_axis$tick$x1, x2 = x_axis$tick$x2,
                        MoreArgs = list(y1 = x_axis$tick$y1, y2 = x_axis$tick$y2)),collapse="\n")
  x_tick_label_code <- paste0(mapply(FUN = write_a_text_element, x = x_axis$tick$x1, value = x_axis$tick$label, 
                              MoreArgs = list(y = x_axis$tick$y_lab)),collapse="\n")
  to_ret <- paste(to_ret, x_tick_code, x_tick_label_code, sep="\n")
  if(any(names(x) == 'y_axis')){
    axis <- x$y_axis
    to_ret <- paste(to_ret, svg_line_code(axis$line$x1,axis$line$x2,axis$line$y1,axis$line$y2), sep="\n")
  }
  return(to_ret)
}

write_svg_bars <- function(x){
  svg_code <- ''
  for(i in seq_along(x)){
    bar_info <- x[[i]]
    if(is.null(bar_info$class)) bar_info$class <- ''
    if(is.null(bar_info$style)) bar_info$style <- ''
    bar_svg_code <- paste0("<rect id='",bar_info$id,"' class='",bar_info$class,"' style='",bar_info$style,
                           "' x='",bar_info$x,"' y='",bar_info$y,
                           "' width='",bar_info$width,"' height='",bar_info$height,"' />")
    hover_transform_text <- paste0("transform='translate(",bar_info$hover_translate[1],",",bar_info$hover_translate[2],")'")
    text_svg_code <- paste0("<text id='",bar_info$hover_id,"' class='",bar_info$hover_class,
                            "' x='",bar_info$hover_x,"' y='",bar_info$hover_y,"' font_size='",
                            bar_info$hover_font_size,"' fill-opacity='0' text-anchor='middle' ",
                            hover_transform_text,"><set attributeName='fill-opacity' to='1.0' begin='",
                            bar_info$id,".mouseover' end='",bar_info$id,".mouseout' />",
                            bar_info$hover_text,"</text>")
    svg_code <- paste(svg_code, bar_svg_code, text_svg_code, sep="\n")
  }
  
  return(svg_code)
}

print.svg_instance_bars <- function(x){
  
  cat(write_svg_bars(x))
  
}

write_svg_rects <- function(x){
  svg_code <- ''
  for(i in seq_along(x)){
    rect_info <- x[[i]]
    if(is.null(rect_info$class)) rect_info$class <- ''
    if(is.null(rect_info$style)) rect_info$style <- ''
    rect_svg_code <- paste0("<rect id='",rect_info$id,"' class='",rect_info$class,"' style='",rect_info$style,
                           "' x='",rect_info$x,"' y='",rect_info$y,
                           "' width='",rect_info$width,"' height='",rect_info$height,"' />")
    svg_code <- paste(svg_code, rect_svg_code, sep="\n")
  }
  
  return(svg_code)
}


