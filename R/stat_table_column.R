scales::rescale(0, to = c(0.33, 0.66), from = c(-30, 40))

# Define the custom ggplot2 stat
StatTableColumn <- ggproto("StatTableColumn", Stat,
                     compute_panel =  function(data, scales) { ## remove scales?

                       old_min <- min(data$x, na.rm = TRUE)
                       old_max <- max(data$x, na.rm = TRUE)


                       data <- data %>%
                         mutate(x = scales::rescale(x, to =  c(unique(xmin_col), unique(xmax_col)), from = c(old_min, old_max))) %>%
                         mutate(xmin = scales::rescale(0, to = c(unique(xmin_col), unique(xmax_col)), from = c(old_min, old_max)), ## Not perfekt: min max values of the original scale )
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
