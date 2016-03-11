


make_svg_points <- function(rel_coods, width, height, svg_class = NULL, style = NULL){
  font_size <- svg_par_instance$font_size
  margin <- svg_par_instance$margin
  svg_coods_xs <- (rel_coods$x * (width  - (font_size*(margin[2]+margin[4])) ) ) + svg_par_instance$margin[2]*font_size
  svg_coods_ys <- (rel_coods$y * (height - (font_size*(margin[1]+margin[3])) ) ) + svg_par_instance$margin[3]*font_size
  if(is.null(svg_class)){
    class_attr <- ''
  }else{
    class_attr <- paste0("class='",svg_class,"'")
  }
  if(is.null(style)){
    style_attr <- ''
  }else{
    # if(style == 'default'){
      style_attr <- paste0("r = '",0.01*min(c(width,height)),"' stroke='black' fill='blue' stroke-width='1'")
#     }else{
#       if(style == 'use_par'){
#         
#       }
#     }
  }
  all_points <- paste0(paste0("<circle id='pt",1:length(svg_coods_xs),"' ",class_attr," cx='",svg_coods_xs,"' cy='",svg_coods_ys,"' ",style_attr," />"), collapse = '\n')
  
  hover_info <- paste0(paste0("<text id='pt_info",1:length(svg_coods_xs),"' x='",svg_coods_xs,"' y='",svg_coods_ys,
                              "' font-size='",font_size,"' fill-opacity='0' text-anchor='middle' transform='translate(0 -",
                              0.02*min(c(width,height)),")'><set attributeName='fill-opacity' to='1.0' begin='pt",
                              1:length(svg_coods_xs),".mouseover' end='pt",1:length(svg_coods_xs),".mouseout'/>",rel_coods$hover,"</text>"), collapse="\n")
  
  return(paste(all_points,hover_info, sep="\n"))
}

make_svg_lines <- function(rel_coods, width, height, margin = svg_par_instance$margin, font_size = svg_par_instance$font_size,
                           svg_class = NULL, style = NULL){
  font_size <- svg_par_instance$font_size
  # margin <- svg_par_instance$margin
  svg_coods_xs <- (rel_coods$x * (width  - (font_size*(margin[2]+margin[4])) ) ) + margin[2]*font_size
  svg_coods_ys <- (rel_coods$y * (height - (font_size*(margin[1]+margin[3])) ) ) + margin[3]*font_size
  svg_coods_x1 <- svg_coods_xs[-length(svg_coods_xs)]
  svg_coods_x2 <- svg_coods_xs[-1]
  svg_coods_y1 <- svg_coods_ys[-length(svg_coods_ys)]
  svg_coods_y2 <- svg_coods_ys[-1]
  
  if(is.null(svg_class)){
    class_attr <- ''
  }else{
    class_attr <- paste0("class='",svg_class,"'")
  }
  if(is.null(style)){
    style_attr <- ''
  }else{
    style_attr <- paste0("stroke='blue' stroke-width='1'")
  }
  return(paste0(paste0("<line ",class_attr," x1='",svg_coods_x1,"' x2='",svg_coods_x2,
                       "' y1='",svg_coods_y1,"' y2='",svg_coods_y2,"' ",style_attr," />"), collapse = '\n'))
}


make_svg_bars <- function(rel_coods, width, height, svg_class = NULL, style = NULL){
  font_size <- svg_par_instance$font_size
  margin <- svg_par_instance$margin
  
  #hardcode svg bar margin
  bar_margin <- 0.2
  bar_width <- (1-bar_margin)*((width-(font_size*(margin[2]+margin[4])))/(length(rel_coods$x)+1)) 
  bar_height <- (1 - rel_coods$y) *(height - (font_size*(margin[1]+margin[3])))
  
  svg_coods_xs <- (rel_coods$x * (width  - (font_size*(margin[2]+margin[4])) ) ) + svg_par_instance$margin[2]*font_size
  svg_coods_xs <- svg_coods_xs - (0.5 * bar_width)
  svg_coods_ys <- (rel_coods$y * (height - (font_size*(margin[1]+margin[3])) ) ) + svg_par_instance$margin[3]*font_size

  if(is.null(svg_class)){
    class_attr <- ''
  }else{
    class_attr <- paste0("class='",svg_class,"'")
  }
  if(is.null(style)){
    style_attr <- ''
  }else{
    style_attr <- paste0("stroke='black' fill='blue' stroke-width='1'")
  }
  all_points <- paste0(paste0("<rect id='pt",1:length(svg_coods_xs),"' ",class_attr,
                              " x='",svg_coods_xs,"' y='",svg_coods_ys,"' width='",bar_width,
                              "' height='",bar_height,"' ",style_attr," />"), collapse = '\n')
  
  hover_info <- paste0(paste0("<text id='pt_info",1:length(svg_coods_xs),"' x='",svg_coods_xs+(0.5*bar_width),"' y='",svg_coods_ys,
                              "' font-size='",font_size,"' fill-opacity='0' text-anchor='middle' transform='translate(0 -",
                              0.02*min(c(width,height)),")'><set attributeName='fill-opacity' to='1.0' begin='pt",
                              1:length(svg_coods_xs),".mouseover' end='pt",1:length(svg_coods_xs),".mouseout'/>",rel_coods$hover,"</text>"), collapse="\n")
  
  return(paste(all_points,hover_info, sep="\n"))
}


make_svg_instance <- function(template, width=svg_par_instance$width, height=svg_par_instance$height, 
                              class_points = NULL, class_lines = NULL, styles = NULL){
  svg_content <- ''
  if(any(names(template) == 'points')){
    svg_content <- paste(svg_content,make_svg_points(template$points, width, height, svg_class = class_points, styles ),sep='\n')
  }
  if(any(names(template) == 'lines')){
    svg_content <- paste(svg_content,make_svg_lines(template$lines, width, height, svg_class = class_lines, style = styles ),sep='\n')
  }
  if(any(names(template) == 'bars')){
    svg_content <- paste(svg_content,make_svg_bars(template$bars, width, height, svg_class = class_points, styles ),sep='\n')
  }
  axes <- make_axes_lines(width, height, style='default')
  y_t_and_l <- make_y_ticks_and_labels(template$y_tick_ats, template$y_tick_labels, height, width, 
                                       svg_par_instance$margin, svg_par_instance$font_size)
  x_t_and_l <- make_x_ticks_and_labels(template$x_tick_ats, template$x_tick_labels,
                                                            height, width,svg_par_instance$margin, svg_par_instance$font_size)
  return(paste(paste0('<svg viewbox = "0 0 ',width,' ',height,'">'),axes,y_t_and_l,x_t_and_l,svg_content,'</svg>', sep="\n"))
}




