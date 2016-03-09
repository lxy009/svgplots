

svg_par <- list(
  'width' = 900,
  'height' = 600
)


test1 <- function(){
  test_data <- c(33.60, 34.80, 69.60, 78.00, 20.40, 9.60, 25.00, 28.80)
  svg_content <- make_svg_instance(svg_ts(test_data),800,600, styles = 'default')
  tmpfile <- paste0(tempfile(),'.html')
  conn <- file(tmpfile)
  writeLines(paste('<html>','<head>','</head>','<body>','<div style="height:200px;width:300px;">',svg_content,"</div>","</body>","</html>",sep="\n"), conn)
  close(conn)
  viewer(tmpfile)
}

