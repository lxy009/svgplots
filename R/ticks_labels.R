
make_ticks_and_labels <- function(tcks,lbls, height, width, margin, font_size){
  svg_ticks_xs <- tcks*(width-font_size*(margin[2]+margin[4])) + margin[2]*font_size
  x_axis_y <- height - margin[1]*font_size
  x_axis_ln <- height - margin[1]*font_size + (1*font_size)
  ticks <- paste0(paste0("<line x1='",svg_ticks_xs,"' x2='",svg_ticks_xs,
                  "' y1='",x_axis_y,"' y2='",x_axis_ln,"' style='stroke:black;stroke-width:2;'/>"), collapse="\n")
  return(ticks)
}

