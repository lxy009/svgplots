

test_data <- c(33.60, 34.80, 69.60, 78.00, 20.40, 9.60, 25.00, 28.80)

svg_ts <- function(ys){
  #make dimensions
  ranges <- list(y = 1.04*diff(range(ys)), x = length(ys)+1)
  extend <- 0.04*diff(range(ys))
  windows <- list(ymin = min(ys)-extend, ymax = max(ys)+extend, xmin = 0, xmax = length(ys)+1)
  
  #make mapping
  x_map <- function(old_x){
    return((old_x - windows$xmin)/ranges$x)
  }
  y_map <- function(old_y){
    return((ranges$y - (old_y - windows$ymin))/ranges$y)
  }
  
  return(list(points = list(x = x_map(1:length(ys)), y = y_map(ys))))
}



