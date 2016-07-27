svg_donut <- function(x, ...) UseMethod('svg_donut')
svg_donut.default <- function(x, col_map = NULL, hover_value = 'perc'){
  cnts <- table(x)
  if(is.null(col_map)){
    col <- rainbow(length(cnts))
  }else{
    col <- col_map[match(names(cnts),names(col_map))]
  }
  return(svg_donut(cnts, col, hover_value = hover_value))
}
svg_donut.table <- function(x, col=NULL, hover_value = 'perc'){
  if(is.null(col)){ col <- rainbow(length(x))}
  #build window and mapping
  y_window <- list(min = 0, max = 1, range = 1)
  x_window <- list(min = 0, max = 1, range = 1)
  x_map <- make_x_mapping(x_window)
  y_map <- make_y_mapping(y_window)
  
  angles <- 2*pi*x/sum(x)
  
  if(hover_value == 'perc'){
    ratios <- x / sum(x)
    hover_labels <- paste0(round(100*ratios,2),'%')
  }
  
  return(structure(list(donut = list(cx = x_map(0.5), cy = y_map(0.5), 
                                     r = c(0.4,0.2), angles = angles, col = col, labels = names(x),
                                     hover_text = hover_labels)), 
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
  n_angles <- length(thetas_beg)
  
  if(n_angles > 1){
    slices <- character(n_angles)
    labels <- character(n_angles)
    hover_labels <- character(n_angles)
    slice_col <- donut$col
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
                          " Z' fill='",substr(slice_col[i],1,7),"' stroke='white' stroke-width='2' ", #!!!!! substr color is bad
                          " onmouseover='document.getElementById(\"hover_",i,"\").style.display =\"inline\";'",
                          " onmouseout ='document.getElementById(\"hover_",i,"\").style.display=\"none\";'/>"
                          )
      text_anchor <- 'end'
      theta_label <- thetas_beg[i] + 0.5*(thetas_end[i] - thetas_beg[i])
      if(theta_label < 0.5*pi | theta_label > 1.5*pi) {
        text_anchor <- 'start'
      }
      labels[i] <- paste0("<text x='",cx+1.02*r1*cos(theta_label),"' y='",cy-1.02*r1*sin(theta_label),
                          "' text-anchor='",text_anchor,"'>",donut$labels[i],"</text>")
      hover_anchor = 'middle'
      hover_r <- r2 + 0.5*(r1-r2)
      hover_labels[i] <- paste0("<text id='hover_",i,"' x='",cx+hover_r*cos(theta_label),"' y='",cy-hover_r*sin(theta_label),
                                "' text-anchor='",hover_anchor,"'  style='display:none;fill:white;background-color:#bbb'>",donut$hover_text[i],"</text>")
    }
  
    return(paste0(slices,labels, hover_labels, sep="\n",collapse="\n"))
  }else{
    c1 <- paste0("<circle cx='",cx,"' cy='",cy,"' r='",r1,"' fill='",donut$col,"' />")
    c2 <- paste0("<circle cx='",cx,"' cy='",cy,"' r='",r2,"' fill='white' />")
    label <- paste0("<text x='",cx,"' y='",cy,"' text-anchor='middle'>",donut$labels,"</text>")
    return(paste0(c1,c2, label,sep="\n"))
  }
}
