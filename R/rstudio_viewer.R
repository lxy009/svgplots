
view <- function(x, ...) UseMethod("view")

view.svg_instance <- function(x){
  html_code <- paste("<!DOCTYPE html><html><head><title>svg view</title></head><body>",
                     x,
                     "</body></head>",
                     sep="\n")
  tmpfile <- paste0(tempfile(),".html")
  conn <- file(tmpfile)
  writeLines(html_code, conn)
  close(conn)
  #view
  viewer(tmpfile)
}

dev_view_svg_instance <- function(x){
  html_code <- paste("<!DOCTYPE html><html><head><title>svg view</title></head><body>",
                     paste0("<div style='width:",x$width,"px;height:",x$height,"px;'>"),
                     dev_write_svg_instance(x),
                     "</div></body></head>",
                     sep="\n")
  tmpfile <- paste0(tempfile(),".html")
  conn <- file(tmpfile)
  writeLines(html_code, conn)
  close(conn)
  #view
  viewer(tmpfile)
}

view.svg_instance_bars <- function(x, width, height){
  #make svg code
  svg_bar_code <- write_svg_bars(x)
  svg_code <- paste("<svg viewbox='0 0 ",width," ",height,"'>",svg_bar_code,"</svg>",sep="\n")
  html_code <- paste("<html><head></head><body><div style='height:",height,"px;width:",width,"px;'>",svg_code,
                     "</div></body></html>",sep="\n")
  #write
  tmpfile <- paste0(tempfile(),".html")
  conn <- file(tmpfile)
  writeLines(html_code, conn)
  close(conn)
  #view
  viewer(tmpfile)
}

quick_view <- function(x, ...) UseMethod('quick_view')

quick_view.svg_instance_bars <- function(x, width, height){
  
}

