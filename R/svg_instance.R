
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

#   id = paste0('bar',i),
#   class = svg_class,
#   style = style,
#   x = svg_coods_xs[i],
#   y = svg_coods_ys[i],
#   width = bar_width,
#   height = bar_height[i],
#   hover_text = rel_coods$hover,
#   hover_x = svg_coods_xs[i] + (0.5*bar_width),
#   hover_y = svg_coods_ys[i],
#   hover_id = paste0('bar_info',i),
#   hover_font_size = font_size,
#   hover_translate = c(0,-0.02*min(c(width,height)))

