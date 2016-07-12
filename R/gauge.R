
svg_gauge <- function(x,min,max){
  
  #build window and mapping
  y_window <- list(min = 0, max = 1, range = 1)
  x_window <- list(min = 0, max = 1, range = 1)
  x_map <- make_x_mapping(x_window)
  y_map <- make_y_mapping(y_window)
  
  return(structure(list(gauge = list( val = x, min = min, max = max,
                                      ang = pi*(x-min)/(max-min),
                                      cx = x_map(0.5), cy = y_map(0), r = c(0.4,0.2))), 
                        class='svg_template'))
}

make_svg_gauge <- function(template, length){
  # ARCS 
  gauge <- template$gauge
  cx <- length*gauge$cx
  cy <- length*gauge$cy
  r <- length*gauge$r[1]
  theta <- pi - gauge$ang
  f1 <- paste0("<path d='M",cx," ",cy,
               " L ", cx - r," ", cy ,
               " A ",r," ",r," 0 0 1 ",cx + r," ",cy,
               " Z' fill='lightgrey' />")
  a1 <- paste0("<path d='M",cx," ",cy,
               " L ", cx - r," ", cy ,
               " A ",r," ",r," 0 0 1 ",cx + r*cos(theta)," ",cy - r*sin(theta),
               " Z' fill='blue' />")
  r <- length*gauge$r[2]
  # white completely destroys below to take into account any rounding issues
  a2 <- paste0("<path d='M",cx," ",cy,
               " L ", cx - r," ", cy ,
               " A ",r," ",r," 0 0 1 ",cx + r," ",cy,
               " Z' fill='white' />")
  # LABELS
  r <- length*gauge$r[1]
  baseline <- paste0("<line x1='",cx+1.02*r,"' y1='",cy,
                     "' x2='",cx-1.02*r,"' y2='",cy,"' stroke='grey' stroke-width='1' />")
  min_label <- paste0("<text x='",cx - 0.5*length*sum(gauge$r) ,"' y='",length,
                      "' text-anchor='middle' dominant-baseline='text-before-edge'>",gauge$min,"</text>")
  max_label <- paste0("<text x='",cx + 0.5*length*sum(gauge$r) ,"' y='",length,
                      "' text-anchor='middle' dominant-baseline='text-before-edge'>",gauge$max,"</text>")
  val_label <- paste0("<text x='",cx,"' y='",length - 0.5*length*gauge$r[2],
                      "' text-anchor='middle' dominant-baseline='text-before-edge'>",gauge$val,"</text>")
  return(paste0(f1, a1,a2,baseline, min_label,max_label, val_label, collapse='\n'))
}
