# Define the custom ggplot2 stat
StatTableColumn <- ggproto("StatTableColumn", Stat,
                           setup_data = function(data, params) {
                             calc_table_coords(data)
                           },
  compute_panel = function(data, scales) {

    ## I think the nas are automatically removed?
    data <- data %>%
      group_by(column) %>%
      mutate(x = scales::rescale(x, to = c(unique(xmin_col), unique(xmax_col)), from = c(unique(scale_min), unique(scale_max)))) %>%
      mutate(
        xmin = scales::rescale(0, to = c(unique(xmin_col), unique(xmax_col)), from = c(unique(scale_min), unique(scale_max))),
        xmax = x,
        xintercept = xmin
      )
    data <- calc_y_coords(data) %>%
      mutate(
        ymin = ymin_row + 0.25, ymax = ymax_row - 0.25,
        yend = max(ymax_row),
        y = min(ymin_row)
      ) %>%
      ungroup()

    return(data)
  },
  required_aes = c("x", "scale_min", "scale_max", "column")
)

# Custom function to use the StatTableColumn stat
stat_table_column <- function(mapping = NULL, data = NULL, geom = "bar",
                              position = "identity", show.legend = NA,
                              inherit.aes = TRUE, na.rm = FALSE, ...) {
  ggplot2::layer(
    stat = StatTableColumn,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)  )
}

StatTableColumnDebugg <- ggdebug::create_stat_with_caching(
  StatTableColumn
)
