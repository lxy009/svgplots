
make_y_ticks_and_labels <- function(tcks,lbls, height, width, margin, font_size){
  
  svg_ticks_ys <- tcks*(height-font_size*(margin[1]+margin[3])) + margin[3]*font_size
  y_axis_x <- margin[2]*font_size
  y_axis_ln <- margin[2]*font_size - (1*font_size)
  ticks <- paste0(paste0("<line x1='",y_axis_x,"' x2='",y_axis_ln,
                         "' y1='",svg_ticks_ys,"' y2='",svg_ticks_ys,
                         "' style='stroke:black;stroke-width:2;'/>"), collapse="\n")
  
  if(is.null(lbls)){
    labels <- ''
  }else{
    labels <- paste0(paste0("<text x='",margin[2]*font_size - (2*font_size),"' y='",svg_ticks_ys,
                            "' text-anchor='middle' transform='rotate(270 ",margin[2]*font_size - (2*font_size),
                            ",",svg_ticks_ys,")' font-size='",font_size,"'>",lbls,"</text>"),collapse="\n")
  }
  
  return(paste(ticks,labels,sep='\n'))
}





make_x_ticks_and_labels <- function(tcks,lbls, height, width, margin, font_size){
  
  svg_ticks_xs <- tcks*(width-font_size*(margin[2]+margin[4])) + margin[2]*font_size
  x_axis_y <- height - margin[1]*font_size
  x_axis_ln <- height - margin[1]*font_size + (1*font_size)
  ticks <- paste0(paste0("<line x1='",svg_ticks_xs,"' x2='",svg_ticks_xs,
                  "' y1='",x_axis_y,"' y2='",x_axis_ln,"' style='stroke:black;stroke-width:2;'/>"), collapse="\n")
  
  if(is.null(lbls)){
    labels <- ''
  }else{
    labels <- paste0(paste0("<text x='",svg_ticks_xs,"' y='",x_axis_y + (2*font_size),
                            "' text-anchor='middle' font-size='",font_size,"'>",lbls,"</text>"),collapse="\n")
  }
    
  return(paste(ticks,labels,sep='\n'))
}

