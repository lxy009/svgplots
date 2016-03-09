

# calc_point_radius <- function(){
#   if(svg_par_points$stroke_width_type == 'relative')
# }

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
  return(paste0(paste0("<circle ",class_attr," cx='",svg_coods_xs,"' cy='",svg_coods_ys,"' ",style_attr," />"), collapse = '\n'))
}


make_svg_instance <- function(template, width=svg_par_instance$width, height=svg_par_instance$height, class_points = NULL, styles = NULL){
  svg_content <- ''
  if(any(names(template) == 'points')){
    svg_content <- paste('',make_svg_points(template$points, width, height, svg_class = class_points, styles ),sep='\n')
  }
  axes <- make_axes_lines(width, height, style='default')
  t_and_l <- make_ticks_and_labels(template$x_tick_ats, template$x_tick_labels,
                                                            height, width, svg_par_instance$margin, svg_par_instance$font_size)
  return(paste(paste0('<svg viewbox = "0 0 ',width,' ',height,'">'),axes,t_and_l,svg_content,'</svg>', sep="\n"))
}




