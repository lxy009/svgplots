

svg_par <- list(
  'width' = 900,
  'height' = 600
)

#time series
test1 <- function(){
  svg_par_instance$font_size <- 20
  test_data <- c(33.60, 34.80, 69.60, 78.00, 20.40, 9.60, 25.00, 28.80)
  test_labels<- c('JAN', 'FEB','MAR', 'APR', 'MAY', 'JUN','JUL','AUG')
  svg_content <- make_svg_instance(svg_ts(test_data,test_labels),1600,900, styles = 'default')
  tmpfile <- paste0(tempfile(),'.html')
  conn <- file(tmpfile)
  writeLines(paste('<html>','<head>','</head>','<body>','<div style="height:300px;width:533px;">',svg_content,"</div>","</body>","</html>",sep="\n"), conn)
  close(conn)
  svg_par_instance$font_size <- 16
  viewer(tmpfile)
}

#bar charts
test2 <- function(){
  # svg_par_instance$font_size <- 20
  test_data <- c(26, 23, 27, 13, 21, 18, 9, 8)
  test_labels<- c('JAN', 'FEB','MAR', 'APR', 'MAY', 'JUN','JUL','AUG')
  svg_content <- make_svg_instance(svg_bar(test_data,test_labels),1600,900, styles = 'default')
  tmpfile <- paste0(tempfile(),'.html')
  conn <- file(tmpfile)
  writeLines(paste('<html>','<head>','</head>','<body>','<div style="height:300px;width:533px;">',svg_content,"</div>","</body>","</html>",sep="\n"), conn)
  close(conn)
  # svg_par_instance$font_size <- 16
  viewer(tmpfile)
  
  #test 2b
  
}

test3 <- function(){
  test_data <- c(26, 23, 27, 13, 21, 18, 9, 8)
  test_labels<- c('JAN', 'FEB','MAR', 'APR', 'MAY', 'JUN','JUL','AUG')
  res1 <- svg_bar(test_data, test_labels)
  print(res1)
  plot(res1)
  res2 <- dev_make_svg_bars(res1$bars, 1600, 900, svg_par_instance$margin, svg_par_instance$font_size)
  print(res2)
  view(res2, 1600, 900)
}

test4 <- function(){
  ell <- 200
  x <- svg_gauge(60,0,100)
  print(x)
  plot(x)
  res <- make_svg_gauge(x, ell)
  res <- paste0(paste0('<svg viewbox = "0 0 ',ell+20,' ',ell+20,'">'),res,"</svg>",collapse="\n")
  cat(res)
  tmpfile <- paste0(tempfile(),'.html')
  conn <- file(tmpfile)
  writeLines(paste('<html>','<head>','</head>','<body>',
                   paste0('<div style="height:',ell+20,'px;width:',ell+20,'px;">'),
                          res,"</div>","</body>","</html>",sep="\n"), conn)
  close(conn)
  viewer(tmpfile)
}

test5 <- function(){
  test_data <- c(33.60, 34.80, 69.60, 78.00, 20.40, 9.60, 25.00, 28.80)
  test_vol <-  c(1023,  1042,  1201,  1042,  1119,  992,  1301,  1092)
  test_labels<- c('JAN', 'FEB','MAR', 'APR', 'MAY', 'JUN','JUL','AUG')
  x <- svg_stock(test_data, labels = test_labels)
  plot(x)
  print(x)
  x <- svg_stock(test_data, test_vol, test_labels)
  plot(x)
  print(x)
#   res <- make_svg_gauge(x, ell)
#   res <- paste0(paste0('<svg viewbox = "0 0 ',ell+20,' ',ell+20,'">'),res,"</svg>",collapse="\n")
#   cat(res)
#   tmpfile <- paste0(tempfile(),'.html')
#   conn <- file(tmpfile)
#   writeLines(paste('<html>','<head>','</head>','<body>',
#                    paste0('<div style="height:',ell+20,'px;width:',ell+20,'px;">'),
#                    res,"</div>","</body>","</html>",sep="\n"), conn)
#   close(conn)
#   viewer(tmpfile) 
}

test6 <- function(){
  ell <- 250
  x <- svg_donut(c(rep('brown',5),rep('blue',3),'green'))
  plot(x)
  # print(x)
  res <- make_svg_donut(x, ell)
  res <- paste0(paste0('<svg viewbox = "0 0 ',ell+20,' ',ell+20,'">'),res,"</svg>",sep="\n")
  cat(res)
  tmpfile <- paste0(tempfile(),'.html')
  conn <- file(tmpfile)
  writeLines(paste('<html>','<head>','</head>','<body>',
                   paste0('<div style="height:',ell+20,'px;width:',ell+20,'px;">'),
                   res,"</div>","</body>","</html>",sep="\n"), conn)
  close(conn)
  viewer(tmpfile)
  
  x <- svg_donut(c(rep('brown',5),rep('blue',3),'green'), col_map = c('brown'='brown','blue'='blue','green'='green'))
  plot(x)
  # print(x)
  res <- make_svg_donut(x, ell)
  res <- paste0(paste0('<svg viewbox = "0 0 ',ell+20,' ',ell+20,'">'),res,"</svg>",sep="\n")
  cat(res)
  tmpfile <- paste0(tempfile(),'.html')
  conn <- file(tmpfile)
  writeLines(paste('<html>','<head>','</head>','<body>',
                   paste0('<div style="height:',ell+20,'px;width:',ell+20,'px;">'),
                   res,"</div>","</body>","</html>",sep="\n"), conn)
  close(conn)
  viewer(tmpfile)
}

test7 <- function(){
  test_data <- c(33.60, 34.80, 69.60, 78.00, 20.40, 9.60, 25.00, 28.80)
  test_labels<- c('JAN', 'FEB','MAR', 'APR', 'MAY', 'JUN','JUL','AUG')
  barplot(height = test_data, names.arg = test_labels)
  template <- dev_svg_barplot(test_data, test_labels)
  print(template)
  dev_plot_svg_template(template)
  instance <- make_svg_instance(x = template, width = 400, height = 300)
  print(instance[-1])
  cat(dev_write_svg_instance(instance))
  dev_view_svg_instance(instance)
}
