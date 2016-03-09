

make_axes_lines <- function(width = svg_par_instance$width, height = svg_par_instance$height,
                            margin = svg_par_instance$margin, font_size = svg_par_instance$font_size, 
                            units = svg_par_instance$units,
                            axes_class = NULL, style = NULL){
  if(is.null(axes_class)){
    class_attr <- ''
  }else{
    class_attr <- paste0("class='",axes_class,"'")
  }
  if(is.null(style)){
    style_attr <- ''
  }else{
    style_attr <- "style='stroke:black;stroke-width:2px;'"
  }
  
  #x-axis
  #line
  x_axis_line <- paste0("<line x1='",margin[2]*font_size,units,
                        "' x2='",width - (margin[4]*font_size),units,
                        "' y1='",height-(margin[1]*font_size),units,
                        "' y2='",height-(margin[1]*font_size),units,
                        "' ",class_attr," ",style_attr,"  />")
  #y-axis
  #line
  y_axis_line <- paste0("<line x1='",margin[2]*font_size,units,
                        "' x2='",margin[2]*font_size,units,
                        "' y1='",margin[3]*font_size,units,
                        "' y2='",height-(margin[1]*font_size),units,
                        "' ",class_attr," ",style_attr,"  />")
  
  return(paste(x_axis_line,y_axis_line,sep="\n"))
}


