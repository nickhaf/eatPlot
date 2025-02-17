transform_table_column_values <- function(x, xmin_col, xmax_col, xmin_old = min(x), xmax_old = max(x)) {
  (x - xmin_old) / (xmax_old - xmin_old) * (xmax_col - xmin_col) + xmin_col
}




# Define the custom ggplot2 stat
StatTableColumn <- ggproto("StatTableColumn", Stat,
                     compute_panel =  function(data, scales) { ## remove scales?
                       data <- data %>%
                         mutate(x = transform_table_column_values(x, xmin_col, xmax_col)) %>%
                         mutate(xmin = transform_table_column_values(0, xmin_col, xmax_col, old_min, old_max),
                                xmax = x)
                       data <- calc_y_coords(data)

                       return(data)
                     },
                     required_aes = c("x", "xmin_col", "xmax_col")
)

# Custom function to use the StatTableColumn stat
stat_table_column <- function(mapping = NULL, data = NULL, geom = "bar",
                              position = "identity", show.legend = NA,
                              inherit.aes = TRUE, xmin_col = NA, xmax_col = NA, ...) {
  ggplot2::layer(
    stat = StatTableColumn,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xmin_col = xmin_col, xmax_col = xmax_col, ...)
  )
}

StatTableColumnDebugg <- ggdebug::create_stat_with_caching(
  StatTableColumn
)
