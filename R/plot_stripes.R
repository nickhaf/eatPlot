plot_stripes <- function(odd = grDevices::rgb(219, 238, 244, maxColorValue = 255), even = "#00000000"){
  ggstats::geom_stripped_rows(
  odd = odd,
  even = even)
}
