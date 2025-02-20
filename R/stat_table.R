
calc_y_coords <- function(data){
  data <- data %>%
  mutate(ymin = as.numeric(factor(row)) - 1, ymax = as.numeric(factor(row))) %>%
    mutate(y = (ymin + ymax)/2)
  return(data)
}

calc_table_coords <- function(data, scales) {
#
#   colnumber <- length(unique(long_2$x_axis)) - 1
#   col_width <- 1/colnumber

  position_width <- data %>%
    select(column, col_width) %>%
    unique() %>%
    arrange(as.numeric(column)) %>%
    mutate(xmin = lag(cumsum(col_width), default = 0),
           xmax = cumsum(col_width))


 data <- merge(data, position_width)
 data <- calc_y_coords(data)

 return(data)
}

StatTable <- ggproto("StatTable", Stat,
                         compute_panel = calc_table_coords,
                         required_aes = c("text", "column", "row", "col_width") ## x and y are optional, if provided the respective values are not computed
)

StatTableDebugg <- ggdebug::create_stat_with_caching(
  StatTable
)

stat_table <- function(mapping = NULL, data = NULL, geom = "table",
                           position = "identity", show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTable,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}
