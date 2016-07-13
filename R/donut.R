svg_donut <- function(x) UseMethod('svg_donut')
svg_donut.default <- function(x){
  cnts <- table(x)
  return(svg_donut(cnts))
}
svg_donut.table <- function(x){
  #build window and mapping
  y_window <- list(min = 0, max = 1, range = 1)
  x_window <- list(min = 0, max = 1, range = 1)
  x_map <- make_x_mapping(x_window)
  y_map <- make_y_mapping(y_window)
  
  angles <- 2*pi*x/sum(x)
  return(structure(list(donut = list(cx = x_map(0.5), cy = y_map(0.5), 
                                     r = c(0.4,0.2), angles = angles)), 
                   class='svg_template'))
}


make_svg_donut <- function(template, length){
  # ARCS 
  donut <- template$donut
  cx <- length*donut$cx
  cy <- length*donut$cy
  r1 <- length*donut$r[1]
  r2 <- length*donut$r[2]
  thetas_beg <- c(0,cumsum(head(donut$angles,-1)))
  thetas_end <- cumsum(donut$angles)
  slices <- character(length(thetas_beg))
  slice_col <- rainbow(length(slices))
  for(i in seq_along(slices)){
    delta_theta <- diff(c(thetas_beg[i],thetas_end[i]))
    if(delta_theta > pi){
      large_flag <- 1
    }else{
      large_flag <- 0
    }
    slices[i] <- paste0("<path d='M ",cos(thetas_beg[i])*r1+cx," ",cy - sin(thetas_beg[i])*r1,
                        " A ",r1," ",r1," 0 ",large_flag," 0 ",cx + cos(thetas_end[i])*r1," ",cy - sin(thetas_end[i])*r1,
                        " L ",cx + cos(thetas_end[i])*r2," ",cy - sin(thetas_end[i])*r2,
                        " A ",r2," ",r2," 0 ",large_flag," 1 ",cx + cos(thetas_beg[i])*r2," ",cy-sin(thetas_beg[i])*r2,
                        " Z' fill='",substr(slice_col[i],1,7),"' stroke='white' stroke-width='2' />"
                        )
  }
#   f1 <- paste0("<path d='M",cx," ",cy,
#                " L ", cx - r," ", cy ,
#                " A ",r," ",r," 0 0 1 ",cx + r," ",cy,
#                " Z' fill='lightgrey' />")
#   a1 <- paste0("<path d='M",cx," ",cy,
#                " L ", cx - r," ", cy ,
#                " A ",r," ",r," 0 0 1 ",cx + r*cos(theta)," ",cy - r*sin(theta),
#                " Z' fill='blue' />")
#   r <- length*gauge$r[2]
#   # white completely destroys below to take into account any rounding issues
#   a2 <- paste0("<path d='M",cx," ",cy,
#                " L ", cx - r," ", cy ,
#                " A ",r," ",r," 0 0 1 ",cx + r," ",cy,
#                " Z' fill='white' />")
#   # LABELS
#   r <- length*gauge$r[1]
#   baseline <- paste0("<line x1='",cx+1.02*r,"' y1='",cy,
#                      "' x2='",cx-1.02*r,"' y2='",cy,"' stroke='grey' stroke-width='1' />")
#   min_label <- paste0("<text x='",cx - 0.5*length*sum(gauge$r) ,"' y='",length,
#                       "' text-anchor='middle' dominant-baseline='text-before-edge'>",gauge$min,"</text>")
#   max_label <- paste0("<text x='",cx + 0.5*length*sum(gauge$r) ,"' y='",length,
#                       "' text-anchor='middle' dominant-baseline='text-before-edge'>",gauge$max,"</text>")
#   val_label <- paste0("<text x='",cx,"' y='",length - 0.5*length*gauge$r[2],
#                       "' text-anchor='middle' dominant-baseline='text-before-edge'>",gauge$val,"</text>")
#   return(paste0(f1, a1,a2,baseline, min_label,max_label, val_label, collapse='\n'))
  return(paste0(slices, collapse="\n"))
}
