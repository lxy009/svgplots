

make_axes_lines <- function(width = svg_par_instance$width, height = svg_par_instance$height,
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
  x_axis_line <- paste0("<line x1='",svg_par_instance$margin[2]*svg_par_instance$font_size,svg_par_instance$units,
                        "' x2='",width - (svg_par_instance$margin[4]*svg_par_instance$font_size),svg_par_instance$units,
                        "' y1='",height-(svg_par_instance$margin[1]*svg_par_instance$font_size),svg_par_instance$units,
                        "' y2='",height-(svg_par_instance$margin[1]*svg_par_instance$font_size),svg_par_instance$units,
                        "' ",class_attr," ",style_attr,"  />")
  #y-axis
  #line
  y_axis_line <- paste0("<line x1='",svg_par_instance$margin[2]*svg_par_instance$font_size,svg_par_instance$units,
                        "' x2='",svg_par_instance$margin[2]*svg_par_instance$font_size,svg_par_instance$units,
                        "' y1='",svg_par_instance$margin[3]*svg_par_instance$font_size,svg_par_instance$units,
                        "' y2='",height-(svg_par_instance$margin[1]*svg_par_instance$font_size),svg_par_instance$units,
                        "' ",class_attr," ",style_attr,"  />")
  
  return(paste(x_axis_line,y_axis_line,sep="\n"))
}


