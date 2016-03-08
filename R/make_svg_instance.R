

make_svg_points <- function(rel_coods, width, height){
  svg_coods_xs <- rel_coods$x * width
  svg_coods_ys <- rel_coods$y * height
  return(paste0(paste0("<circle cx='",svg_coods_xs,"' cy='",svg_coods_ys,"' r='",0.01*min(c(width,height)),
        "' stroke='black' fill='blue' stroke-width='1' />"), collapse = '\n'))
}


make_svg_instance <- function(template, width, height){
  if(any(names(template) == 'points')){
    svg_content <- make_svg_points(template$points, width, height)
  }
  return(paste(paste0('<svg viewbox = "0 0 ',width,' ',height,'">'),svg_content,'</svg>', sep="\n"))
}




